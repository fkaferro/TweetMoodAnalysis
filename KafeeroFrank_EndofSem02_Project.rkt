#lang racket
(require data-science-master)
(require plot)
(require math)
(require json)

;;; This function reads line-oriented JSON (as output by massmine),
;;; and packages it into an array.
;;; NB: Please note that for very large data sets, loading everything into memory like this is heavy handed.
;;;For data this small, working in memory is simpler
(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))


;;; Read in the entire tweet database tweets from Uganda)
(define tweets (string->jsexpr
                (with-input-from-file "TweetsUganda700.json" (λ () (json-lines->json-array)))))


;;; Remove just the tweet text from each tweet hash.
;;; Finally, remove retweets.

(define t
    (let ([tmp (map (λ (x) (list (hash-ref x 'text))) tweets)])
      (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))

(define (single-tweet-list lst)
  (define text "")
    (for-each (lambda(arg)
                (set! text (string-append text " " arg)))
              (flatten lst)
    ) text)

(define tweet-string (single-tweet-list t))

;;; Next, we capture the text from our input port, removing capitalization, 
;;; punctuation, and then extra spaces
(define processed-tweet-string (string-normalize-spaces
		   (remove-punctuation
		    (string-downcase tweet-string) #:websafe? #t)))


;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document
(define words (document->tokens processed-tweet-string #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;; We can take a sneak peak at the data...
(take sentiment 10)
;;; --> '(("word" "sentiment" "freq")
;;;       ("ship" "anticipation" 367)
;;;       ("sea" "positive" 364)
;;;       ("time" "anticipation" 318)
;;;       ("long" "anticipation" 311))

;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))
;;; --> '(("anticipation" 4739)
;;;       ("positive" 9206)
;;;       ("joy" 3196)
;;;       ("trust" 5095)
;;;       ("surprise" 2157)
;;;       ("negative" 7090)
;;;       ("fear" 4136)
;;;       ("sadness" 3317)
;;;       ("anger" 2765)
;;;       ("disgust" 1958))

;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))