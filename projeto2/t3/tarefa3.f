        parameter (m=1000)
c vetor que controla cada andarilho
        dimension npb(m,2)
        
        write(*,*) "digite N:"
        read(*,*) n
        
        rx=0.0
        ry=0.0
        rx2=0.0
        ry2=0.0

        open(unit=1, file='media_r5.dat')

        do i=1,m
                nx=0
                ny=0
c npb(i,1)->controla os passos em x
c npb(i,2)->controla os passos em y
                npb(i,1)=0
                npb(i,2)=0
                do j=1,n
                        r=rand()
c sao 4 casos possiveis, todos de igual probabilidade
                        if(r .lt. 0.5) then
c andou para leste
                                if(r .lt. 0.25) then
                                        nx=nx+1
                                else
c andou para oeste
                                        nx=nx-1
                                end if
                        else
c andou para o norte
                                if(r .lt. 0.75) then
                                        ny=ny+1
                                else
c andou para o sul
                                        ny=ny-1
                                end if
                        end if
                end do
                
c raios medios em x e y
                rx=rx+nx
                ry=ry+ny
                rx2=rx2+nx**2
                ry2=ry2+ny**2

c armazeno as coordenadas x e y do andarilho
                npb(i,1)=nx
                npb(i,2)=ny
                write(1,*) npb(i,1), npb(i,2)
        end do 
        
        d=sqrt(rx**2+ry**2)
        rm=d/m
        rm2=(rx2+ry2)/m
        
        write(*,*) "r=", d
        write(*,*) "media=", rm
        write(*,*) "media quadrada=", rm**2
        write(*,*) "quadrado da media=", rm2
        write(*,*) "delta=", abs(rm**2-rm2)

        close(1)
        end
