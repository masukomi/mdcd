(module listicles
  (
   nth
   range
  )
  (import chicken)
  (import scheme)
  
  (define (nth n lst)
    (if (or (> n (- (length lst) 1)) (< n 0))
      (error 'nth "Index out of bounds.")
      (if (eq? n 0)
        (car lst)
        (nth (- n 1) (cdr lst)))))

  (define (range a b #!optional (increment 1) (existing '()))
    (if (if (> increment 0) (<= a b) (>= a b))
      (cons a (range (+ increment a) b increment existing) )
      existing))
)
