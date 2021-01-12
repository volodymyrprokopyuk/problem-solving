(define-module organize-media)

(select-module organize-media)

(use util.match)
(use gauche.parameter)
(use gauche.record)
(use gauche.parseopt)
(use gauche.process)
(use file.util)
(use rfc.sha)

;; Error

(define (display-error e)
  "Displays error on the stderr"
  (display #"ERROR: ~e\n" (current-error-port)))

;; Parameters

(define (validate-date s :optional (r #/^\d{4}-\d{2}-\d{2}$/))
  "Validates the date s"
  (cond
    [(or (not s) (r s)) s]
    [else (error #"invalid date ~s")]))

(define *dry-run* (make-parameter #f))
(define *media-date* (make-parameter #f validate-date))
(define *media-source* (make-parameter (current-directory)))
(define *media-sink* (make-parameter (build-path (current-directory) "sink")))

;; Options

(define (parse-options args)
  "Parsers command line options"
  (let-args args
    ([n "-n|--dry-run"]
     [d "-d|--date=s"]
     . r)
    (*dry-run* n)
    (*media-date* d)
    (and (> (length r) 0) (*media-source* (resolve-path (car r))))
    (and (> (length r) 1) (*media-sink* (cadr r)))))

;; <media> record

(define-record-type <media> make-media media?
  (file media-file set-media-file)
  (type media-type set-media-type)
  (year media-year set-media-year)
  (month media-month set-media-month)
  (day media-day set-media-day))

(define (display-media m)
  (print (media-file m))
  ;; (print (media-type m))
  (print (media-year m) "-" (media-month m) "-" (media-day m))
  m)

;; Read media

(define (read-media s :optional (g #/\.(?:jpg|jpeg|mp4|mov|avi)$/i))
  "Returns a list of media files from the source s as per glob g"
  (directory-fold s (lambda (f r) (if [g f] (cons f r) r)) '()))

(define (classify-media f)
  "Classifies the media file f into photo or video based on the file extension"
  (let ([m (make-media #f #f #f #f #f)])
    (set! (media-file m) f)
    (cond
      [(#/\.(?:jpg|jpeg)$/i f) (set! (media-type m) 'photo)]
      [(#/\.(?:mp4|mov|avi)$/i f) (set! (media-type m) 'video)]
      [else (set! (media-type m) 'unknown)])
    m))

;; Extract metadata

(define (extract-photo-date f)
  "Extracts the date of the photo file f"
  (and-let* ([t (process-output->string `(exiv2 -K Exif.Image.DateTime -P v ,f))]
             [m (#/^(?<year>\d{4}):(?<month>\d{2}):(?<day>\d{2})/ t)])
    (list (m 'year) (m 'month) (m 'day))))

(define (extract-video-date f)
  "Extracts the date of the video file f"
  (and-let* ([t (process-output->string
                 `(ffprobe -v quiet -select_streams v:0
                           -show_entries stream_tags=creation_time
                           -of default=nw=1:nk=1 ,f))]
             [m (#/^(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/ t)])
    (list (m 'year) (m 'month) (m 'day))))

(define (extract-media-date m)
  "Extracts the year, month, day from the media m"
  (and-let* ([d (case (media-type m)
                  [(photo) (extract-photo-date (media-file m))]
                  [(video) (extract-video-date (media-file m))])])
    (match-let ([(year month day) d])
      (set! (media-year m) year)
      (set! (media-month m) month)
      (set! (media-day m) day)))
  m)

;; Main

(define (main args)
  "Starts program execution"
  ;; (guard
  ;;  (e [else (display-error e) 1])
  (parse-options (cdr args))
  (for-each (.$ display-media extract-media-date classify-media)
            (read-media (*media-source*)))
  ;; 0)
  )
