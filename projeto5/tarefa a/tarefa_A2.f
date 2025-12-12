        implicit real*8(a-h, o-z)
        pi=acos(-1e0)
        gm=4*pi**2

        open(unit=1, file='a2.dat')
        open(unit=2, file='excent.dat')
        open(unit=3, file='area.dat')

        dt=0.0001d0
        
        do j=1,9
                read(2,*) r,eps

c tempo longo de simulacao para abranger 
c todos os diferentes periodos
                n=1000/dt

c assumindo orbita eliptica
                vy=sqrt(gm*(1-eps)/(r*(1+eps)))
                vx=0.0e0

                x=r*(1+eps)
                y=0.0
                t=0

                ncont=0
                somax=0.0
                somay=0.0
        
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

c busca direta para encontrar o periodo
                        if(y*y_aux .lt. 0.0) then
                                ncont=ncont+1
                                somax=somax+abs(x)
                        end if

                        if(x*x_aux .lt. 0.0) then
                                somay=somay+abs(y)
                        end if
                        
c atualizacao das variaveis para mais uma iteracao
                        x_old=x
                        y_old=y

                        x=x_aux
                        y=y_aux     
                end do

                per=2*t/ncont
                a=2*somax/ncont
                b=2*somay/ncont
                razao=per**2/max(a,b)**3
                
                c=sqrt(max(a,b)**2-min(a,b)**2)
                apr_eps=c/max(a,b)
                write(1,*) r,per,razao,apr_eps
        end do
       
        close(1)
        close(2)
        close(3)
        close(10)
        end
