# 3.2 The Environment Model of Evaluation

## 3.1 The Rules of Evaluation
```
(define (square x))
  (* x x)))

(define square
  (lambda (x) (* x x)))
```

```
                -------------------
global -------> | other variables |
env             | square: |       |
                ----------|--------
    (define (square x))   |    A
      (* x x)             V    |
                          OO---'
                          |
                          V
                    parameters: x
                    body: (* x x)
```
