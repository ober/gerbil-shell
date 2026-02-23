(declare (block) (standard-bindings) (extended-bindings))
(begin
  (define gsh/bench::timestamp 1771822104)
  (begin
    (define gsh/bench#fmt-secs
      (lambda (_%s149%_)
        (let ((_%ms151%_ (inexact->exact (round (* _%s149%_ '1000)))))
          (string-append (number->string _%ms151%_) '"ms"))))
    (define gsh/bench#time-thunk
      (lambda (_%label133%_ _%thunk134%_)
        (let* ((_%start136%_
                (let () (declare (not safe)) (##process-statistics)))
               (_%result138%_ (_%thunk134%_))
               (_%end-stats140%_
                (let () (declare (not safe)) (##process-statistics)))
               (_%wall142%_
                (- (f64vector-ref _%end-stats140%_ '2)
                   (f64vector-ref _%start136%_ '2)))
               (_%user144%_
                (- (f64vector-ref _%end-stats140%_ '0)
                   (f64vector-ref _%start136%_ '0)))
               (_%gc146%_
                (- (f64vector-ref _%end-stats140%_ '5)
                   (f64vector-ref _%start136%_ '5))))
          (std/format#fprintf
           (current-error-port)
           '"  ~a  ~a wall  ~a cpu  ~a gc~n"
           _%label133%_
           (gsh/bench#fmt-secs _%wall142%_)
           (gsh/bench#fmt-secs _%user144%_)
           (gsh/bench#fmt-secs _%gc146%_))
          _%result138%_)))
    (define gsh/bench#fib
      (lambda (_%n131%_)
        (if (< _%n131%_ '2)
            _%n131%_
            (+ (gsh/bench#fib (- _%n131%_ '1))
               (gsh/bench#fib (- _%n131%_ '2))))))
    (define gsh/bench#ack
      (lambda (_%m125%_ _%n126%_)
        (if (= _%m125%_ '0)
            (+ _%n126%_ '1)
            (if (= _%n126%_ '0)
                (gsh/bench#ack (- _%m125%_ '1) '1)
                (gsh/bench#ack
                 (- _%m125%_ '1)
                 (gsh/bench#ack _%m125%_ (- _%n126%_ '1)))))))
    (define gsh/bench#string-build
      (lambda (_%n118%_)
        (let _%loop120%_ ((_%i122%_ '0) (_%acc123%_ '""))
          (if (>= _%i122%_ _%n118%_)
              (string-length _%acc123%_)
              (_%loop120%_ (+ _%i122%_ '1) (string-append _%acc123%_ '"x"))))))
    (define gsh/bench#hash-churn
      (lambda (_%n105%_)
        (let ((_%ht107%_ (make-hash-table)))
          (let _%loop109%_ ((_%i111%_ '0))
            (if (< _%i111%_ _%n105%_)
                (begin
                  (hash-put! _%ht107%_ _%i111%_ (* _%i111%_ _%i111%_))
                  (_%loop109%_ (+ _%i111%_ '1)))
                '#!void))
          (let _%loop113%_ ((_%i115%_ '0) (_%sum116%_ '0))
            (if (>= _%i115%_ _%n105%_)
                _%sum116%_
                (_%loop113%_
                 (+ _%i115%_ '1)
                 (+ _%sum116%_ (hash-ref _%ht107%_ _%i115%_))))))))
    (define gsh/bench#list-ops
      (lambda (_%n94%_)
        (let ((_%lst101%_
               (let _%loop96%_ ((_%i98%_ '0) (_%acc99%_ '()))
                 (if (>= _%i98%_ _%n94%_)
                     _%acc99%_
                     (_%loop96%_ (+ _%i98%_ '1) (cons _%i98%_ _%acc99%_))))))
          (foldl +
                 '0
                 (map (lambda (_%x103%_) (* _%x103%_ _%x103%_)) _%lst101%_)))))
    (define gsh/bench#run-benchmarks
      (lambda ()
        (std/format#fprintf (current-error-port) '"~n=== bench ===~n")
        (gsh/bench#time-thunk '"fib(40)      " (lambda () (gsh/bench#fib '40)))
        (gsh/bench#time-thunk
         '"ack(3,11)    "
         (lambda () (gsh/bench#ack '3 '11)))
        (gsh/bench#time-thunk
         '"str-build 80k"
         (lambda () (gsh/bench#string-build '80000)))
        (gsh/bench#time-thunk
         '"hash-churn 3M"
         (lambda () (gsh/bench#hash-churn '3000000)))
        (gsh/bench#time-thunk
         '"list-ops 2M  "
         (lambda () (gsh/bench#list-ops '2000000)))
        (std/format#fprintf (current-error-port) '"=== done ===~n")))
    (gsh/bench#run-benchmarks)))
