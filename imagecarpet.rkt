;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname imagecarpet) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

;;================
;;CONSTANTS:

(define CUTOFF 2)

(define SPLOOEY
  (bitmap/url "https://shorturl.at/Va2L3"))
(define NIKE
  (bitmap/url "https://shorturl.at/N9z3Q"))


(@htdf imagecarpet)
(@signature Natural Image -> Image)
;; produces Sierpinski carpet of width n with any square image as backgroun

(check-expect (imagecarpet CUTOFF SPLOOEY)
              (scale (/ CUTOFF (image-height SPLOOEY)) SPLOOEY))
(check-expect
 (imagecarpet (* 3 CUTOFF) SPLOOEY)
 (overlay (local [(define sub
                    (square CUTOFF "solid" "transparent"))
                  (define blk
                   (scale (/ CUTOFF (image-height SPLOOEY)) SPLOOEY))]
            (above (beside sub sub sub)
                   (beside sub blk sub)
                   (beside sub sub sub)))
          (scale (/ (* 3 CUTOFF) (image-height SPLOOEY)) SPLOOEY)))
(check-expect
 (imagecarpet (* 3 CUTOFF) NIKE)
 (overlay (local [(define sub
                    (square CUTOFF "solid" "transparent"))
                  (define blk
                    (scale (/ CUTOFF (image-height NIKE)) NIKE))]
            (above (beside sub sub sub)
                   (beside sub blk sub)
                   (beside sub sub sub)))
          (scale (/ (* 3 CUTOFF) (image-height NIKE)) NIKE)))

;(define (imagecarpet n0 i) empty-image) ;stub

(@template-origin genrec)

(define (imagecarpet n0 i)
  (overlay
   (local [(define (nobg-imagecarpet n)
             (if (<= n CUTOFF)
                 (square n "solid" "transparent")
                 (overlay 
                  (local [(define sub
                            (nobg-imagecarpet (/ n 3)))
                          (define blk
                            (scale (/ (/ n 3) (image-height i)) i))]
                    (above (beside sub sub sub)
                           (beside sub blk sub)
                           (beside sub sub sub)))
                  (scale (/ n (image-height i)) i))))]
     (nobg-imagecarpet n0)) (scale (/ n0 (image-height i)) i)))