        dimension x(100000000)
        write(*,*) "digite N:"
        read(*,*) n
        
        soma=0.0
        do i=1,n
                x(i)=rand()
                soma=soma+x(i)
        go to 10
10      end do

        xmedia=soma/float(n)
        write(*,*) "a media eh:", xmedia
        end

