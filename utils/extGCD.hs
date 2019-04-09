-- (x, y, d) = extgcd a b
-- d == gcd a b
-- d == a * x + b * y

-- gcd formula
-- A_0 = a, A_1 = b
-- A_n = d, A_{n+1} = 0
-- A_{k-1} = q_k * A_k + A_{k+1}

-- gcd formula ( matrix form )
--- ( A_k     ) = ( 0   1   ) ( A_{k-1} ) = ( 0   1   ) ( 0   1       ) ... ( 0   1  ) ( A_0 )
--- ( A_{k+1} )   ( 1  -q_k ) ( A_k     )   ( 1  -q_k ) ( 1  -q_{k-1} )     ( 1  -q_1) ( A_1 )
---                                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- convert underlined form to another form (xy-form)
-- x_0 = 1, x_1 = 0, y_0 = 0, y_1 = 1
-- x_n = x, y_n = y
--- ( x_k     y_k     ) = ( 0   1   ) ( x_{k-1} y_{k-1} ) = ( 0   1   ) ( 0   1       ) ... ( 0   1  )
--- ( x_{k+1} y_{k+1} )   ( 1  -q_k ) ( x_k     y_k     )   ( 1  -q_k ) ( 1  -q_{k-1} )     ( 1  -q_1)

-- gcd formula presented by xy-form
--- ( A_k     ) = ( x_k     y_k     ) ( A_0 )
--- ( A_{k+1} )   ( x_{k+1} y_{k+1} ) ( A_1 )

-- especially when k=n, we have
--- ( A_n ) = ( x_n     y_n     ) ( A_0 )
--- ( 0   )   ( x_{n+1} y_{n+1} ) ( A_1 )
-- where A_0 = a, A_1 = b, A_n = d, x_n = x, y_n = y
-- so we can get a solution of next Bézout's equation:
-- d = a * x + b * y

-- in haskell programming, solving 3 reccurence formulae
-- A_{k-1} = q_k * A_k + A_{k+1}
-- x_{k+1} = x_{k-1} - q_k * x_k
-- y_{k+1} = y_{k-1} - y_k * x_k

extgcd :: Int -> Int -> (Int,Int,Int)
extgcd = go 1 0 0 1
  where go x y _ _ d 0 = (d, x, y)
        go x y x' y' a b = let q = a`div`b
                               r = a`mod`b
                           in go x' y' (x-q*x') (y-q*y') b r

