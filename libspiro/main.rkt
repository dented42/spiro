
#lang racket

(provide (struct-out control-point)
         control-points->beziers)

(require ffi/unsafe ffi/unsafe/define ffi/cvector data/queue)

(define-ffi-definer define-libspiro (ffi-lib "libspiro"))

(define _spiro_cp_type
  (_enum `(spiro-corner = ,(char->integer #\v)
           spiro-g4 = ,(char->integer #\o)
           spiro-g2 = ,(char->integer #\c)
           spiro-left = ,(char->integer #\[)
           spiro-right = ,(char->integer #\])
           spiro-close = ,(char->integer #\z)
           spiro-begin-segment = ,(char->integer #\{)
           spiro-end-segment = ,(char->integer #\}))
         _ubyte))

(define-cstruct _spiro-cp ([x _double]
                           [y _double]
                           [ty _spiro_cp_type]))

(struct control-point (x y type))

(define (control-point->spiro-cp cp)
  (make-spiro-cp (+ 0.0 (control-point-x cp))
                 (+ 0.0 (control-point-y cp))
                 (match (control-point-type cp)
                   ['corner 'spiro-corner]
                   ['g4 'spiro-g4]
                   ['g2 'spiro-g2]
                   [(or 'enter-line 'match-next) 'spiro-left]
                   [(or 'exit-line 'match-previous) 'spiro-right]
                   ['close 'spiro-close]
                   ['begin-segment 'spiro-begin-segment]
                   ['end-segment 'spiro-end-segment])))

(define-fun-syntax _control-point
  (syntax-id-rules (_control-point)
    [_control-point (type: _spiro-cp pre:(cp => (control-point->spiro-cp cp)))]))

(define-cstruct _bezctx (;; The bool tells us whether or not we are starting a closed curve.
                         [moveto (_fun _bezctx-ptr _double _double _bool -> _void)]
                         [lineto (_fun _bezctx-ptr _double _double -> _void)]
                         [quadto (_fun _bezctx-ptr _double _double _double _double -> _void)]
                         [curveto (_fun _bezctx-ptr _double _double _double _double
                                        _double _double -> _void)]
                         [mark-knot (_fun _bezctx-ptr _int -> _void)]))

(define-fun-syntax _bezctx-ptr
  (syntax-id-rules (_bezctx-ptr)
    [_bezctx-ptr (type: (_ptr i _bezctx))]))

#;(define-libspiro SpiroCPsToBezier
    (_fun (_ptr i (_cvector _spiro-cp)) _int _bool _bezctx-pointer -> _void))

(define-libspiro TaggedSpiroCPsToBezier
  (_fun (_list i _control-point) (_ptr i _bezctx) -> _void))

(define (control-points->beziers pts)
  (let ([q (make-queue)])
    (TaggedSpiroCPsToBezier pts
                            (make-bezctx (λ (ctx x y open?)
                                           (enqueue! q `(move-to ,x ,y ,(if open? 'open 'closed))))
                                         (λ (ctx x y)
                                           (enqueue! q `(line-to ,x ,y)))
                                         (λ (ctx x1 y1 x2 y2)
                                           (enqueue! q `(quad-to ,x1 ,y1 ,x2 ,y2)))
                                         (λ (ctx x1 y1 x2 y2 x3 y3)
                                           (enqueue! q `(curve-to ,x1 ,y1 ,x2 ,y2 ,x3 ,y3)))
                                         (λ (ctx knot)
                                           (enqueue! q `(knot ,knot)))))
    (queue->list q)))

(module+ main
  (define diagnostic-bezctx
    (make-bezctx (λ (ctx x y is-open?) (displayln (list x y)))
                 (λ (ctx x y) (displayln '--) (displayln (list x y)))
                 (λ (ctx x1 y1 x2 y2) (displayln "quadto"))
                 (λ (ctx x1 y1 x2 y2 x3 y3)
                   (displayln (list x1 y1))
                   (displayln (list x2 y2))
                   (displayln (list x3 y3)))
                 (λ (ctx knot) (void) #;(displayln "mark-knot"))))
  
  (define cpts
    (list (control-point -500.0 0.0 'spiro-begin-segment)
          (control-point -500.0 500.0 'spiro-g4)
          (control-point 0.0 500.0 'spiro-left)
          (control-point 500.0 0.0 'spiro-right)
          (control-point 500.0 -500.0 'spiro-g4)
          (control-point 0.0 -500.0 'spiro-end-segment)))
  
  (time (TaggedSpiroCPsToBezier  cpts diagnostic-bezctx))
  (time (control-points->beziers cpts)))