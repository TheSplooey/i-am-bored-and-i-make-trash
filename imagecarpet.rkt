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
(define SPLOOEY-TALL
  (bitmap/url "https://shorturl.at/OXqpx"))
(define SPLOOEY-WIDE
  (bitmap/url "https://shorturl.at/46pim"))


(@htdf imagecarpet)
(@signature Natural Image -> Image)
;; produces Sierpinski carpet of width n with any square image as backgroun

(check-expect (imagecarpet CUTOFF SPLOOEY)
              (scale (/ CUTOFF (image-height SPLOOEY)) SPLOOEY))
(check-expect
 (imagecarpet (* 3 CUTOFF) SPLOOEY)
 (overlay (local [(define sub
                    (scale (/ CUTOFF (image-height SPLOOEY)) SPLOOEY))
                  
                  (define blk
                    (scale (/ CUTOFF (image-height SPLOOEY)) SPLOOEY))]
            (above (beside sub sub sub)
                   (beside sub blk sub)
                   (beside sub sub sub)))
          (scale (/ (* 3 CUTOFF) (image-height SPLOOEY)) SPLOOEY)))
(check-expect
 (imagecarpet (* 3 CUTOFF) SPLOOEY-TALL)
 (local [(define (dimensions i)
                    (max (image-height i) (image-width i)))
                  (define SPLOOEY-TALL-RECTIFY
                    (scale/xy (/ (dimensions SPLOOEY-TALL)
                                 (image-width SPLOOEY-TALL))
                              (/ (dimensions SPLOOEY-TALL)
                                 (image-height SPLOOEY-TALL)) SPLOOEY-TALL))]
 (overlay (local [(define sub
                    (scale (/ CUTOFF (image-height SPLOOEY-TALL-RECTIFY))
                           SPLOOEY-TALL-RECTIFY))
                  
                  (define blk
                    (scale (/ CUTOFF (image-height SPLOOEY-TALL-RECTIFY))
                           SPLOOEY-TALL-RECTIFY))]
            (above (beside sub sub sub)
                   (beside sub blk sub)
                   (beside sub sub sub)))
          (scale (/ (* 3 CUTOFF) (image-height SPLOOEY-TALL-RECTIFY))
                 SPLOOEY-TALL-RECTIFY))))
(check-expect
 (imagecarpet (* 3 CUTOFF) SPLOOEY-WIDE)
 (local [(define (dimensions i)
                    (max (image-height i) (image-width i)))
                  (define SPLOOEY-WIDE-RECTIFY
                    (scale/xy (/ (dimensions SPLOOEY-WIDE)
                                 (image-width SPLOOEY-WIDE))
                              (/ (dimensions SPLOOEY-WIDE)
                                 (image-height SPLOOEY-WIDE)) SPLOOEY-WIDE))]
   (overlay (local [(define sub
                    (scale (/ CUTOFF (image-height SPLOOEY-WIDE-RECTIFY))
                           SPLOOEY-WIDE-RECTIFY))
                  
                  (define blk
                    (scale (/ CUTOFF (image-height SPLOOEY-WIDE-RECTIFY))
                           SPLOOEY-WIDE-RECTIFY))]
            (above (beside sub sub sub)
                   (beside sub blk sub)
                   (beside sub sub sub)))
          (scale (/ (* 3 CUTOFF) (image-height SPLOOEY-WIDE-RECTIFY))
                 SPLOOEY-WIDE-RECTIFY))))

;(define (imagecarpet n0 i0) empty-image) ;stub

(@template-origin fn-composition genrec)

(define (imagecarpet n0 i0)
  (local [(define (imagecarpet n i)
            (if (<= n CUTOFF)
                (scale (/ n (image-height i)) i)
                (overlay 
                 (local [(define sub
                           (imagecarpet (/ n 3) i))
                         
                         (define blk
                           (scale (/ (/ n 3) (image-height i)) i))]
                   (above (beside sub sub sub)
                          (beside sub blk sub)
                          (beside sub sub sub)))
                 (scale (/ n (image-height i)) i))))
          (define (squared i)
            (local [(define (dimensions i)
                      (max (image-height i) (image-width i)))]
              (scale/xy (/ (dimensions i) (image-width i))
                        (/ (dimensions i) (image-height i)) i)))]
    (imagecarpet n0 (squared i0))))
