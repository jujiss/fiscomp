        implicit real*8 (a-h, o-z)
        pi=acos(-1e0)
        gm=4*pi**2

        open(unit=1, file='a1.dat')
        open(unit=2, file='excent.dat')
        
        dt=0.0001e0
        n=1000/dt
        
        do j=1,9
                read(2,*) r

c tempo longo de simulacao para abranger 
c todos os diferentes periodos
c                n=100/dt
                
c assumindo orbita circular                
                vy=sqrt(gm/r)
                vx=0.0e0

                y=0.0e0
                tt=0.0
                x=r
                t=0

                ncont=0
        
                x_old=x-vx*dt
                y_old=y-vy*dt

                do i=1,n
                        t=t+dt
c raio para cada nova coordenada x, y                        
                        r_new=sqrt(x**2+y**2)
                        ax=-gm*x/r_new**3
                        ay=-gm*y/r_new**3

c metodo de verlet
                        y_aux=2*y-y_old+ay*dt**2
                        x_aux=2*x-x_old+ax*dt**2
                        
c                        write(1,*) y*y_old
c busca direta para encontrar o periodo
                        if(y*y_old .lt. 0.0) then
                                ncont=ncont+1
c                                write(*,*) y*y_old
                        end if

c atualizacao das variaveis para mais uma iteracao
                        x_old=x
                        y_old=y

                        x=x_aux
                        y=y_aux     
                end do

                per=2*t/ncont
                razao=per**2/r**3
                write(1,*) r,per,razao
        end do 
       
        close(1)
        close(2)
        end
