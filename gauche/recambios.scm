(define-module recambios)

(select-module recambios)

(use gauche.parameter)
(use gauche.sequence)
(use gauche.record)
(use srfi-13) ;; String
(use srfi-19) ;; Date and time

;; Error

(define (show-error e)
  "Shows the error e message on the stderr"
  (display #"ERROR: ~e\n" (current-error-port)))

;; Patameter

(define brand-name (make-parameter "DESCONOCIDO"))
(define read-success (make-parameter 0))
(define read-failure (make-parameter 0))
(define write-total (make-parameter 0))
(define output (make-parameter "output"))

(define (inc-read-success! r)
  "Increments the read success count"
  (read-success (+ (read-success) 1))
  r)

(define (inc-read-failure!)
  "Increments the write failure count"
  (read-failure (+ (read-failure) 1)))

(define (inc-write-total!)
  "Increments the wirte total count"
  (write-total (+ (write-total) 1)))

;; Record

(define-record-type <recambio> make-recambio recambio?
  (reference reference)
  (name name)
  (stock stock)
  (price price)
  (purchase-date purchase-date)
  (brand brand))

;; Read

(define (read-file f)
  "Returns a string list of the file f"
  (guard
   (e [else (show-error #"read-file: ~f ~e") '()])
   (display #"Reading ~f\n")
   (with-input-from-file f (cut port->string-list (current-input-port)))))

;; Parse

(define (show-read-report rl)
  "Shows the current file processing report"
  (display #"  Success ~(read-success)\
             \n  Failure ~(read-failure)\
             \n  Total ~(+ (read-success) (read-failure))\n")
  rl)

(define (show-overall-total rl)
  "Shows the overall total of read <recambio> records from all files"
  (display #"Overall total ~(length rl)\n")
  rl)

(define (file->brand f)
  "Returns the brand from the file f"
  (let ([m (#/RECAMBIOS (?<brand>.+)$/i f)])
    (if m (m 'brand) "DESCONOCIDO")))

(define (create-record m)
  "Returns <recambio> record from the regexp match m"
  (let ([d ($ string-trim $ m 'date)])
    (make-recambio
     ($ string-trim-right $ m 'ref)
     ($ string-trim-both $ m 'name)
     ($ string->number $ string-trim $ m 'stock)
     ($ string->number $ regexp-replace "," (m 'price) ".")
     (if [zero? (string-length d)] #f (string->date (m 'date) "~d/~m/~y"))
     (brand-name))))

(define (parse-record i s)
  "Returns a <recambio> record if the string s with the index i is a valid <recambio>, \
   otherwise #f"
  (guard
   (e [else (show-error #"parse-record: ~(+ i 1) ~e") (inc-read-failure!) #f])
   (and-let* ([r #/^(?<ref>\w[\w ]{23})(?<name>.{26})(?<stock>[\d ]{9}\d) +(?<price>\d+,\d{2}) ?(?<date>\d{2}\/\d{2}\/\d{2}| {,8})/]
              [m (r s)])
     ($ inc-read-success! $ create-record m))))

(define (parse-file sl)
  "Returns a list of <recambio> records from the string list sl"
  ($ filter identity $ map-with-index parse-record sl))

(define (process-file f)
  "Returns a list of <recambio> records from the file f"
  (brand-name (file->brand f))
  (read-success 0) (read-failure 0)
  ($ show-read-report $ parse-file $ read-file f))

(define (read-recambios fl)
  "Returns a list of <recambio> records from the list of files fl"
  ($ show-overall-total $ append-map process-file fl))

;; Group

(define (group-by-year r)
  "Returns <recambio> year and the <recambio> r itself"
  (let ([d (purchase-date r)])
    (values (if d ($ number->string $ date-year d) "SIN FECHA") r)))

(define (group-recambios rl gf)
  "Returns a hash table of <recambio> records from the record list rl grouped as per \
   the group function gf"
  (let ([h (make-hash-table 'string=?)])
    (for-each
     (lambda (r)
       (receive (k v) (gf r)
         (hash-table-push! h k v)))
     rl)
    h))

;; Write

(define (format-recambio r)
  "Formats the <recambio> record r"
  (let* ([d (purchase-date r)]
         [fd (if d (date->string d "~Y-~d-~m") "")])
    (display #"\"~(reference r)\",\"~(name r)\",~(stock r),~(price r),~fd,\"~(brand r)\"\n")
    (inc-write-total!)))

(define (write-file f rl)
  "Writes to the file f the list rl of <recambio> records"
  (guard
   (e
    [else (show-error #"write-file: ~f ~e")])
   (let ([f #"~(output)/RECAMBIOS ~|f|.csv"])
     (write-total 0)
     (display #"Writing ~f\n")
     (with-output-to-file f
       (lambda ()
         (display #"Referencia,Denominación,Stock,Precio,Fecha de compra,Marca\n")
         (for-each format-recambio rl)))
     (display #"  Total ~(write-total)\n"))))

(define (write-recambios h)
  "Writes the hash h of <recambio> records to files named by hash keys"
  (hash-table-for-each h write-file))

;; Main

(define (main args)
  "Starts program execution"
  ($ write-recambios $ (cut group-recambios <> group-by-year) $ read-recambios (cdr args))
  0)
