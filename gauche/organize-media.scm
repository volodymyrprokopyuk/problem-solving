(define-module organize-media)

(select-module organize-media)

(use util.match)
(use gauche.parameter)
(use gauche.record)
(use gauche.parseopt)
(use gauche.process)
(use file.util)
(use util.digest)
(use rfc.sha)
(use srfi-13) ;; string-downcase

;; Error

(define (display-error e)
  "Displays error on the stderr"
  (display #"ERROR: ~e\n" (current-error-port)))

;; Parameters

(define (set-default-date s :optional (r #/^(?<year>\d{4})(?<month>\d{2})(?<day>\d{2})$/))
  (define (set-date m)
    (*default-year* (m 'year))
    (*default-month* (m 'month))
    (*default-day* (m 'day)))
  "Sets the default date from the string s"
  (cond
    [(r s) => set-date]
    [else (error #"invalid date ~s")]))

(define *dry-run* (make-parameter #f))
(define *force-date* (make-parameter #f))
(define *default-year* (make-parameter "2000"))
(define *default-month* (make-parameter "01"))
(define *default-day* (make-parameter "01"))
(define *media-source* (make-parameter (current-directory)))
(define *media-sink* (make-parameter (build-path (current-directory) "sink")))

;; Options

(define (parse-options args)
  "Parsers command line options"
  (let-args args
    ([n "-n|--dry-run"]
     [f "-f|--force-date"]
     [d "-d|--date=s"]
     . r)
    (*dry-run* n)
    (*force-date* f)
    (and d (set-default-date d))
    (when (and f (not d)) (error "missing force date"))
    (and (> (length r) 0) (*media-source* (resolve-path (car r))))
    (and (> (length r) 1) (*media-sink* (cadr r)))))

;; <media> record

(define-record-type <media> make-media media?
  (file media-file set-media-file)
  (type media-type set-media-type)
  (year media-year set-media-year)
  (month media-month set-media-month)
  (day media-day set-media-day)
  (time media-time set-media-time)
  (digest media-digest set-media-digest)
  (status media-status set-media-status))

(define (display-media m)
  "Displyas the media m data, metadate and status"
  (match-let* ([(@ <media> (file fl) (year yr) (month mn) (day dy) (time tm) (status st)) m]
               [f (regexp-replace #/(?<=(.{20}))(.*)(?=(.{50}))/ fl "...")])
    (format #t "~73@a ~a~a~a ~a ~a\n" f yr mn dy tm st))
  m)

;; Read media

(define (read-media s :optional (g #/\.(?:jpg|jpeg|mp4|mov|avi)$/i))
  "Returns a list of media files from the source s as per glob g"
  (directory-fold s (lambda (f r) (if [g f] (cons f r) r)) '()))

(define (classify-media f)
  "Classifies the media file f into photo or video based on the file extension"
  (let ([m (make-media f #f
                       (*default-year*) (*default-month*) (*default-day*) "000000"
                       #f 'read)])
    (cond
      [(#/\.(?:jpg|jpeg)$/i f) (set! (media-type m) 'photo)]
      [(#/\.(?:mp4|mov|avi)$/i f) (set! (media-type m) 'video)])
    m))

;; Extract timestamp

(define (extract-photo-timestamp f)
  "Extracts the timestamp of the photo file f"
  (and-let* ([t (process-output->string `(exiv2 -K Exif.Image.DateTime -P v ,f))]
             [m (#/^(?<year>\d{4}):(?<month>\d{2}):(?<day>\d{2}) (?<hour>\d{2}):(?<min>\d{2}):(?<sec>\d{2})/ t)])
    (list (m 'year) (m 'month) (m 'day) (m 'hour) (m 'min) (m 'sec))))

(define (extract-video-timestamp f)
  "Extracts the timestamp of the video file f"
  (and-let* ([t (process-output->string
                 `(ffprobe -v quiet -select_streams v:0
                           -show_entries stream_tags=creation_time
                           -of default=nw=1:nk=1 ,f))]
             [m (#/^(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})T(?<hour>\d{2}):(?<min>\d{2}):(?<sec>\d{2})/ t)])
    (list (m 'year) (m 'month) (m 'day) (m 'hour) (m 'min) (m 'sec))))

(define (extract-media-timestamp m)
  "Extracts the timestamp from the media m"
  (guard
   (e [else (display-error e) (set! (media-status m) 'timestamp-failed)])
   (and-let* ([d (case (media-type m)
                   [(photo) (extract-photo-timestamp (media-file m))]
                   [(video) (extract-video-timestamp (media-file m))])])
     (match-let ([(year month day hour min sec) d])
       (set! (media-year m) year)
       (set! (media-month m) month)
       (set! (media-day m) day)
       (set! (media-time m) #"~|hour|~|min|~|sec|")
       (set! (media-status m) 'timestamp))))
  m)

(define (force-date m)
  "Sets the default timestamp for the media m if explicitly required"
  (when (*force-date*)
       (set! (media-year m) (*default-year*))
       (set! (media-month m) (*default-month*))
       (set! (media-day m) (*default-day*))
       (set! (media-status m) 'date-forced))
  m)

;; Digest media

(define (digest-media m)
  "Generates digest for the media m"
  (guard
   (e [else (display-error e) (set! (media-status m) 'digest-failed)])
   (let ([d (with-input-from-file (media-file m) sha1-digest)])
     (set! (media-digest m) (digest-hexify d))
     (set! (media-status m) 'digest)))
  m)

;; Write media

(define (write-media m)
  "Writes the media m to the sink location"
  (guard
   (e [else (display-error e) (set! (media-status m) 'write-failed)])
   (match-let* ([(@ <media> (year yr) (month mn) (day dy) (time tm) (digest dg) (file fl)) m]
                [d (build-path (*media-sink*) yr)]
                [e (regexp-replace "jpeg" (string-downcase (path-extension fl)) "jpg")]
                [f #"~|yr|~|mn|~|dy|_~|tm|_~|dg|.~|e|"])
     (make-directory* d)
     (if (copy-file fl (build-path d f) :if-exists :supersede)
         (set! (media-status m) 'written)
         (set! (media-status m) 'write-failed))))
  m)

;; Main

(define (main args)
  "Starts program execution"
  (guard
   (e [else (display-error e) 1])
   (parse-options (cdr args))
   (for-each
      (.$ display-media
          (if (*dry-run*) identity write-media)
          digest-media
          force-date
          extract-media-timestamp
          classify-media)
      (read-media (*media-source*)))
   0))
