main :: IO ()
main = do
  _ <- getLine
  ct <- product . map (+1) . filter (/=0) . foldr count [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] <$> getLine :: IO Integer
  print $ (ct - 1) `mod` (10^9 + 7)

count :: Char -> [Integer] -> [Integer]
count char [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'a' = [a+1,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'b' = [a,b+1,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'c' = [a,b,c+1,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'd' = [a,b,c,d+1,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'e' = [a,b,c,d,e+1,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'f' = [a,b,c,d,e,f+1,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'g' = [a,b,c,d,e,f,g+1,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'h' = [a,b,c,d,e,f,g,h+1,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'i' = [a,b,c,d,e,f,g,h,i+1,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'j' = [a,b,c,d,e,f,g,h,i,j+1,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'k' = [a,b,c,d,e,f,g,h,i,j,k+1,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'l' = [a,b,c,d,e,f,g,h,i,j,k,l+1,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'm' = [a,b,c,d,e,f,g,h,i,j,k,l,m+1,n,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'n' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n+1,o,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'o' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o+1,p,q,r,s,t,u,v,w,x,y,z]
  | char == 'p' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p+1,q,r,s,t,u,v,w,x,y,z]
  | char == 'q' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q+1,r,s,t,u,v,w,x,y,z]
  | char == 'r' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r+1,s,t,u,v,w,x,y,z]
  | char == 's' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s+1,t,u,v,w,x,y,z]
  | char == 't' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t+1,u,v,w,x,y,z]
  | char == 'u' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u+1,v,w,x,y,z]
  | char == 'v' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v+1,w,x,y,z]
  | char == 'w' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w+1,x,y,z]
  | char == 'x' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x+1,y,z]
  | char == 'y' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y+1,z]
  | char == 'z' = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z+1]
  
