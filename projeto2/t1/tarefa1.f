        write(*,*) "digite N:"
        read(*,*) n
        
        soma1=0.0
        soma2=0.0
        soma3=0.0
        soma4=0.0

        do i=1,n
                x=rand()
                soma1=soma1+x
                soma2=soma2+x**2
                soma3=soma3+x**3
                soma4=soma4+x**4
        go to 10
10      end do

        xmedia1=soma1/n
        xmedia2=soma2/n
        xmedia3=soma3/n
        xmedia4=soma4/n
        
        write(*,*) "a media de x eh:", xmedia1
        write(*,*) "a media de x**2 eh:", xmedia2
        write(*,*) "a media de x**3 eh:", xmedia3
        write(*,*) "a media de x**4 eh:", xmedia4
        end

