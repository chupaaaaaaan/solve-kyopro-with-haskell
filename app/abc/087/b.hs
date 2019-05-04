main :: IO ()
main=readLn>>= \a->readLn>>= \b->readLn>>= \c->readLn>>= \x->print$length[0|l<-[0..a],m<-[0..b],n<-[0..c],500*l+100*m+50*n==x]


