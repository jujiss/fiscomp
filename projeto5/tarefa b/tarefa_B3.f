        implicit real*8(a-h, o-z)
        dimension r(1:4)
        dimension v0(1:4)
        dimension vx(1:4)
        dimension x(1:4)
        dimension y(1:4)
        dimension x_old(1:4)
        dimension y_old(1:4)
        dimension x_aux(1:4)
        dimension y_aux(1:4)
c raios jupiter, ast 1, ast 2 e ast 3
        r=(/5.2d0, 3.0d0, 3.276d0, 3.7d0/)
        vx=(/0.0d0, 0.0d0, 0.0d0, 0.0d0/)
c v0 jup, ast 1, ast 2 e ast 3 - orbita circ
        v0=(/2.755d0, 3.628d0, 3.471d0, 3.267d0/)

c valores das constantes GMs e GMj
        pi=dacos(-1d0)
        gm=4*pi**2
        gmj=gm*1.0d-3

        open(unit=1, file='ast1.dat')
        open(unit=2, file='ast2.dat')
        open(unit=3, file='ast3.dat')
        open(unit=4, file='jup.dat')

        dt=0.0001d0
        n=300/dt
c como agora sao muitos corpos, convem usar vetores
        do i=1,4
                x(i)=r(i)
                y(i)=0.0d0
                
                x_old(i)=x(i)-vx(i)*dt
                y_old(i)=y(i)-v0(i)*dt
        end do
        
        do j=1,n
                t=t+dt
c distancia jup-sol
                rj=dsqrt(x(1)**2+y(1)**2)

                do k=2,4
c distancia jup-ast(1,2,3)
                        raj=dsqrt((x(k)-x(1))**2+(y(k)-y(1))**2)
c distancia ast(1,2,3)-sol
                        ras=dsqrt(x(k)**2+y(k)**2)

                        ax=-gm*x(k)/ras**3
                        ay=-gm*y(k)/ras**3

                        axja=-gmj*(x(k)-x(1))/raj**3
                        ayja=-gmj*(y(k)-y(1))/raj**3
c metodo de verlet asteroides
                        x_aux(k)=2*x(k)-x_old(k)+(ax+axja)*dt**2
                        y_aux(k)=2*y(k)-y_old(k)+(ay+ayja)*dt**2
                end do
                
                axj=-gm*x(1)/rj**3
                ayj=-gm*y(1)/rj**3
c metodo de verlet jupiter
                x_aux(1)=2*x(1)-x_old(1)+axj*dt**2
                y_aux(1)=2*y(1)-y_old(1)+ayj*dt**2

                do l=1,4
c atualizo todos os vetores para nova iteracao
                        x_old(l)=x(l)
                        y_old(l)=y(l)

                        x(l)=x_aux(l)
                        y(l)=y_aux(l)

                        write(l,*) x(l),y(l)
                end do
        end do

        close(1)
        close(2)
        close(3)
        close(4)
        end
