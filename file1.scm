;; --- IMPORTS ---
(require-extension html-parser)
(require-extension sxpath)
(require-extension sxpath-lolevel)
(use posix)

;; --- DATA ---

;; DEFINE RECORDS
;; DAY RECORD
(define-record day events date)
(define-record-printer (event rt out)
  (fprintf out "-- EVENT --\nTITLE: ~s\nLOCATION: ~s\nTIME: ~s\n"
  (event-title rt) (event-location rt) (event-time rt)))

;; EVENT RECORD
(define-record event title location time)
(define-record-printer (day rt out)
  (fprintf out "-- DAY -- ~s \n ~s" (day-date	 rt) (day-events rt)))

;; --- METHODS ---

;; all-divs-with-attr symbol string -> list
;; finds divs in tree by its attribute
(define (all-divs-with-attr attrn id)
  (letrec ((all-divs
             (lambda (v |#!optional| (tail '()))
               (if (sxml:element? v)
                 (let ((rest (fold-right
                               (lambda (c tail) (all-divs c tail))
                               tail
                               (sxml:child-nodes v)))
                       (maybe-id (sxml:attr v attrn)))
                   (if (and (eq? (sxml:name v) 'div)
                            maybe-id
                            (string=? maybe-id id))
                     (cons v rest)
                     rest))
                 tail))))
    all-divs))


;; x-one object -> object
;; process object with only one variable
(define (x-one v)
	(if (and (pair? v) (null? (cdr v))) ;; check if count is one...
		(car v) ;; then process it
		(error "Expected list with one item, got: " v) ;; else throw error
	)
)

;; parse-event list -> event
;; turns the DOM of an event into an event object
(define (parse-event event)
	;; create the event
	(make-event
		(apply string-append (sxml:child-nodes ((all-divs-with-attr 'class "title_short") event)))
		(third (third (first ((all-divs-with-attr 'class "location") event))))
		(apply string-append (sxml:child-nodes ((all-divs-with-attr 'class "time") event)))
	)
)

;; parse-day list -> day
;; turns the DOM of a day into a day object
(define (parse-day day)
	;; create the day
	(make-day 
		(map parse-event ((all-divs-with-attr 'class "event") day))
		(seconds->local-time (current-seconds)) ;; get current time (not processed yet)
	)
)

;; parse-site
;; parses the site
(define (parse-site)
    (let*
		(
			;; get content
			(content (x-one ((all-divs-with-attr 'id "content") (gsal))))
			;; shake off all unecessary divs
	    	(singledays ((all-divs-with-attr 'class "singleday") content))
			;; generate the final list
			(final (map parse-day singledays))
		)
		final ;; return final
	)
)

;; gsal -> list
;; get webpage data and return it's DOM
(define (gsal)
    (call-with-input-file 
        "site.html"
         html->sxml
    )
)

;; in code execution for development
(pp (parse-site))