        parameter(m=1000, n=2000, lado=50)
        dimension isoma(-lado:lado,-lado:lado)

        open(unit=1, file='entropia.dat')

C --- simula até n passos
        do i=1,n
                s=0.0
C --- zera contador de posições para o passo k
                do ix=-lado,lado
                        do iy=-lado,lado
                                isoma(ix,iy)=0
                        end do
                end do

C --- simula m andarilhos
                do j=1,m
                        nx=0
                        ny=0
                        do k=1,i
                                r=rand()
                                if(r .lt. 0.25) then
                                        nx=nx+1
                                else if(r .lt. 0.50) then
                                        nx=nx-1
                                else if(r .lt. 0.75) then
                                        ny=ny+1
                                else
                                        ny=ny-1
                                end if
                        end do

                        if(abs(nx) .le. lado) then
                                if(abs(ny) .le. lado) then
                                        isoma(nx,ny)=isoma(nx,ny)+1
                                end if
                        end if
                end do
                
C --- calcula entropia para este k
                do ix=-lado,lado
                        do iy=-lado,lado
                                if(isoma(ix,iy) .gt. 0) then
                                        p=float(isoma(ix,iy))/m
                                        s=s-p*alog(p)
                                end if
                        end do
                end do

C --- escreve no arquivo: passo k, entropia
                write(1,*) i, s
        end do

        close(1)
        end

