#lang racket
(require graphics/graphics)

(define height 700) ;画框高
(define width 600) ;画框宽

(open-graphics) ;加载绘图函数
(define window (open-viewport "test frame" width height)) ;设置画框

(define (drawline a b)
  ((draw-line window) (make-posn (car a) (cdr a)) (make-posn (car b) (cdr b)) "blue"))
(define (drawlines lst)
  (if (= (length lst) 1)
      (void)
      (begin (drawline (car lst) (cadr lst))
             (drawlines (cdr lst)))))

;vectors
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
(define wave-list
  (let ((v1 (list (make-vect 0 26)
                  (make-vect 6 17)
                  (make-vect 12 25)
                  (make-vect 14 21)
                  (make-vect 10 0)))
        (v2 (list (make-vect 16 0)
                  (make-vect 21 13)
                  (make-vect 25 0)))
        (v3 (list (make-vect 31 0)
                  (make-vect 25 19)
                  (make-vect 41 6)))
        (v4 (list (make-vect 41 15)
                  (make-vect 31 27)
                  (make-vect 25 27)
                  (make-vect 27 35)
                  (make-vect 25 41)))
        (v5 (list (make-vect 16 41)
                  (make-vect 14 35)
                  (make-vect 16 27)
                  (make-vect 12 27)
                  (make-vect 6 25)
                  (make-vect 0 35))))
    (map (lambda (lst1)
            (map (lambda (a)
                   (cons (* 0.024 (car a)) (* 0.024 (cdr a)))) lst1))
         (list v1 v2 v3 v4 v5))))


;frames
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))
(define testFrame
  (make-frame (make-vect 0 400) (make-vect 400 0) (make-vect 0 -400)))
(define frame2
  (make-frame (make-vect 600 350) (make-vect 200 40) (make-vect -200 -300)))
(define frame3
  (make-frame (make-vect 300 200) (make-vect 180 0) (make-vect 0 180)))

(define (draw frame lst)
  (if (null? lst)
      (void)
      (let ((a (map (frame-coord-map frame) (car lst))))
        (begin (drawlines a) (draw frame (cdr lst))))))
(define wave
  (lambda (frame) (draw frame wave-list)))


;transform frames
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ((m (frame-coord-map frame))
           (new-origin (m origin)))
      (painter
       (make-frame new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin))))))
(define (beside paintera painterb)
  (lambda (frame)
    (begin ((transform-painter paintera (cons 0 0) (cons 0.5 0) (cons 0 1)) frame)
      ((transform-painter painterb (cons 0.5 0) (cons 1 0) (cons 0.5 1)) frame))))
(define (below paintera painterb)
  (lambda (frame)
    (begin ((transform-painter paintera (cons 0 0) (cons 1 0) (cons 0 0.5)) frame)
      ((transform-painter painterb (cons 0 0.5) (cons 1 0.5) (cons 0 1)) frame))))
(define (flip-vert painter)
  (lambda (frame)
    ((transform-painter painter (cons 0 1) (cons 1 1) (cons 0 0)) frame)))
(define (flip-hori painter)
  (lambda (frame)
    ((transform-painter painter (cons 1 0) (cons 0 0) (cons 1 1)) frame)))
(define (up-split painter n)
  (if (= n 1)
      painter
      (let ((up (up-split painter (- n 1))))
        (below painter (beside up up)))))
(define (right-split painter n)
  (if (= n 1)
      painter
      (let ((right (right-split painter (- n 1))))
        (beside painter (below right right)))))
(define (corner-split painter n)
  (if (= n 1)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
      (below (beside painter (below right right))
             (beside (beside up up) (corner-split painter (- n 1)))))))

(define (pic1)
  (let* ((a (corner-split wave 6))
        (b (beside (flip-hori a) a)))
    ((below (flip-vert b) b) testFrame)))
    
(define (pic2) ((beside wave (flip-vert wave)) frame2))

(pic1)
(pic2)
;(define ymq 1118)
;(define (transform r theta)
;  (cons (* r (cos theta)) (* r (sin theta))))
;(define (a n)
;  (define (generate k)
;    (let ((theta (/ (* 2 pi) n)))
;    (if (> k n)
;        '()
;        (cons (transform (+ 1 (sin (* k theta))) (* k theta)) (generate (+ k 1))))))
;  (generate 0))
;(define (love n)
;  (draw frame3 (list (a n))))
;(love ymq)