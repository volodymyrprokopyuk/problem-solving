(define-module
  (fsm-algorithm)
  #:export ())

(use-modules
 (ice-9 receive)
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Объект управления ОУ
(define (make-alarm-clock-object)
  ;; Вычислительные состояния V
  (let ([hour 0] [minute 0] [alarm-hour 0] [alarm-minute 0])
    ;; Команды f-c <- выходные воздействия
    (let* ([inc-time-hour
            (lambda () (set! hour (remainder (1+ hour) 24)))]
           [inc-time-minute
            (lambda () (set! minute (remainder (1+ minute) 60)))]
           [inc-alarm-hour
            (lambda () (set! alarm-hour (remainder (1+ alarm-hour) 24)))]
           [inc-alarm-minute
            (lambda () (set! alarm-minute (remainder (1+ alarm-minute) 60)))]
           [inc-time ;; Is triggered every minute by a minute timer to count the time
            (lambda () (inc-time-minute) (when (zero? minute) (inc-time-hour))
               (format #t "Time: ~2,'0d:~2,'0d\n" hour minute))]
           [alarm-on
            (lambda () (pp 'alarm-on))]
           [alarm-off
            (lambda () (pp 'alarm-off))]
           ;; Запросы f-q -> входные воздействия
           [start-alarm? ;; Start alarm one minute before alarm due time
            (lambda () (and (= hour alarm-hour) (= minute (1- alarm-minute))))]
           [stop-alarm? ;; Stop alarm exactly at alarme due time
            (lambda () (and (= hour alarm-hour) (= minute alarm-minute)))])
      (values inc-time-hour inc-time-minute inc-alarm-hour inc-alarm-minute
              inc-time
              alarm-on alarm-off
              start-alarm? stop-alarm?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Управляющий автомат УА
(define (make-alarm-clock)
  ;; Управляющие состояния Y
  ;; 'alarm-off 'alarm-setup 'alarm-on
  (let ([state 'alarm-off])
    ;; Объект управления ОУ
    (receive
        (inc-time-hour inc-time-minute inc-alarm-hour inc-alarm-minute
                       inc-time
                       alarm-on alarm-off
                       start-alarm? stop-alarm?)
        (make-alarm-clock-object)
      ;; Функция переходов delta + новое управляющее состояние
      ;; Функция выходов fi + выходные воздействия + команды
      ;; События + входные воздействия
      ;; 'hour 'minute 'alarm 'timer
      (lambda (event)
        (cond
          [(eq? state 'alarm-off) ;; Dispatch on state
           (cond ;; Dispatch on event (alarm clock interface)
             [(eq? event 'hour) (inc-time-hour)]
             [(eq? event 'minute) (inc-time-minute)]
             [(eq? event 'alarm) (set! state 'alarm-setup)]
             [(eq? event 'timer) (inc-time)])]
          [(eq? state 'alarm-setup) ;; Dispatch on state
           (cond ;; Dispatch on event (alarm clock interface)
             [(eq? event 'hour) (inc-alarm-hour)]
             [(eq? event 'minute) (inc-alarm-minute)]
             [(eq? event 'alarm) (set! state 'alarm-on)]
             [(eq? event 'timer) (inc-time)])]
          [(eq? state 'alarm-on) ;; Dispatch on state
           (cond ;; Dispatch on event (alarm clock interface)
             [(eq? event 'hour) (inc-time-hour)]
             [(eq? event 'minute) (inc-time-minute)]
             ;; Команда объекту управления
             [(eq? event 'alarm) (alarm-off) (set! state 'alarm-off)]
             ;; Запрос к объекту управления
             [(and (eq? event 'timer) (start-alarm?)) (inc-time) (alarm-on)]
             [(and (eq? event 'timer) (stop-alarm?)) (inc-time) (alarm-off)]
             [(eq? event 'timer) (inc-time)])])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Автоматизированный объект управления АО
(let ([alarm-clock (make-alarm-clock)])
  ;; Автомат обрабатывает события от внешней среды интерфейс сервисов обработки событий
  ;; Set clock time to OO:01
  (alarm-clock 'minute)
  ;; Set alarm time to 00:04
  (alarm-clock 'alarm)
  (alarm-clock 'minute)
  (alarm-clock 'minute)
  (alarm-clock 'minute)
  (alarm-clock 'minute)
  ;; Set alarm on
  (alarm-clock 'alarm)
  ;; Time pases
  (alarm-clock 'timer)
  ;; (alarm-clock 'alarm) ;; Cancel alarm before due time
  (alarm-clock 'timer)
  (alarm-clock 'timer)
  ;; (alarm-clock 'alarm) ;; Stop alarm during due time
  (alarm-clock 'timer)
  (alarm-clock 'timer)
  (alarm-clock 'timer))
