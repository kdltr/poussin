($define! $sequence
  ((wrap ($vau ($seq2) #ignore
           ($seq2
             ($define! $aux
               ($vau (head . tail) env
                 ($if (null? tail)
                      (eval head env)
                      ($seq2
                        (eval head env)
                        (eval (cons $aux tail) env)))))
             ($vau body env
               ($if (null? body)
                    #inert
                    (eval (cons $aux body) env))))))
   ($vau (first second) env
     ((wrap ($vau #ignore #ignore (eval second env)))
      (eval first env)))))

($define! list (wrap ($vau x #ignore x)))

($define! list*
  (wrap ($vau args #ignore
          ($sequence
            ($define! aux
              (wrap ($vau ((head . tail)) #ignore
                      ($if (null? tail)
                           head
                           (cons head (aux tail))))))
            (aux args)))))

($define! $vau
  ((wrap ($vau ($vau) #ignore
            ($vau (formals eformal . body) env
               (eval (list $vau formals eformal
                           (cons $sequence body))
                     env))))
   $vau))

($define! $lambda
  ($vau (formals . body) env
     (wrap (eval (list* $vau formals #ignore body)
                 env))))

($define! car ($lambda ((x . #ignore)) x))
($define! cdr ($lambda ((#ignore . x)) x))
($define! cadr ($lambda (x) (car (cdr x))))

($define! apply
  ($lambda (appv arg . opt)
     (eval (cons (unwrap appv) arg)
           ($if (null? opt)
                (make-environment)
                (car opt)))))

($define! $cond
   ($vau clauses env
      ($define! aux
         ($lambda ((test . body) . clauses)
            ($if (eval test env)
                 (apply (wrap $sequence) body env)
                 (apply (wrap $cond) clauses env))))
      ($if (null? clauses)
           #inert
           (apply aux clauses))))

($define! list-tail
   ($lambda (ls k)
      ($if (>? k 0)
           (list-tail (cdr ls) (- k 1))
           ls)))

($define! map
  ($lambda (appv (x . xs))
     ($if (null? xs)
          (list (appv x))
          (cons (appv x) (map appv xs)))))

($define! $let
   ($vau (bindings . body) env
      (eval (cons (list* $lambda (map car bindings) body)
                  (map cadr bindings))
            env)))
