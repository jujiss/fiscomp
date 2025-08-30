	write(*,*) 'digite Q, N e AJM:' 
	read(*,*) Q, N, AJM
c valor da parcela sem juros	
	SJ = Q/N
c valor da parcela com a taxa de juros
	V = AJM*SJ

	write(*,*) 'cada parcela sera:', V

	end 
 
 

	
