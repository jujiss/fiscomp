	write(*,*) 'digite r1 e r2:'
	read(*,*) r1, r2
	
	pi = acos(-1e0)
c formulas da area e do volume de um torus
	a = 4 * pi * pi * r1 * r2
	v = 2 * pi * pi * r1 * r1 * r2

	write(*,*) 'a área é:', a
	write(*,*) 'o volume é:', v

	end 
