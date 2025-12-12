        parameter(tmax=1000, del_t=0.04, gama=0.5, omega=0.666667)
        pi=acos(-1e0)
        n=tmax/del_t

        open(unit=3, file='te_omg.dat')
        
        write(*,*) "digite F0 e theta0:"
        read(*,*) F0, theta1
       
        omg1=0.0
        t=0

c euler cromer
        do j=1,n
                t=t+del_t

                omg1_aux=omg1-(sin(theta1)+gama*omg1-
     & F0*sin(omega*t))*del_t
                theta1_aux=theta1+omg1_aux*del_t

c tratamento em theta para que ele esteja sempre
c no intervalo [-pi, pi]
                do while(theta1_aux .gt. pi)
                        theta1_aux=theta1_aux-2*pi
                end do
                do while (theta1_aux .lt. -pi)
                        theta1_aux=theta1_aux+2*pi
                end do

c tratamento temporal para que o tempo seja proximo ao 
c do periodo natural do forcamento
                if(mod(omega*t, pi) .lt. del_t/2) then
                        write(3,*) theta1_aux, omg1_aux
                end if

                theta1=theta1_aux
                omg1=omg1_aux
        end do

        close(3)
        end
