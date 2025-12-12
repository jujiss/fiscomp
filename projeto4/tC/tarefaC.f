        parameter(tmax=60, del_t=0.04, gama=0.5, omega=0.666667)
        pi=acos(-1e0)
        n=tmax/del_t
c guarda os resultados para F0=0.5
        open(unit=3, file='f05.dat')
c guarda os resultados para F0=1.2
        open(unit=4, file='f12.dat')
c arq com os dois valores de F0
        open(unit=1, file='trc.dat')

c loop que le cada F0 do arq
        do i=1,2
                read(1,*) F0
c valores iniciais de theta arbitrarios para os dois osc.
                theta1=0.112000
                theta2=0.113000

c valores iniciais de omega para os dois osciladores
                omg1=0.0
                omg2=0.0
                t=0

c loop de euler cromer
                do j=1,n
                        t=t+del_t

                        omg1_aux=omg1-(sin(theta1)+gama*omg1-
     & F0*sin(omega*t))*del_t
                        omg2_aux=omg2-(sin(theta2)+gama*omg2-
     & F0*sin(omega*t))*del_t

                        theta1_aux=theta1+omg1_aux*del_t
                        theta2_aux=theta2+omg2_aux*del_t
                        
                        omg1=omg1_aux
                        omg2=omg2_aux

                        dif=abs(theta2-theta1)
                        dln=alog(dif)

c if para desconsiderar os casos em que a dif de theta1 e 2
c eh menor do que a precisao das variaveis
                         if(dif .gt. 0.000001) then
                                if(F0 .eq. 0.5) then
                                        write(3,*) t, dln
                                else
                                        write(4,*) t, dln
                                end if
                        end if

                        theta1=theta1_aux
                        theta2=theta2_aux
                end do
        end do
        
        close(1)
        close(3)
        close(4)
        end
