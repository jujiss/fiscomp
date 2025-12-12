       parameter(tmax=80, del_t=0.04, gama=0.5, omega=0.666667)
       pi=acos(-1e0)
      
        write(*,*) "digite F0: "
        read(*,*) F0
     
c condicoes iniciais euler cromer
        n=tmax/del_t
        ncont=0   
        theta2=0.1
        omg2=0.0
        tt=0
        t=0

        open(unit=3, file='theta_t0.dat')
        open(unit=4, file='omega_t0.dat')

        open(unit=6, file='theta_t05.dat')
        open(unit=7, file='omega_t05.dat')

        open(unit=8, file='theta_t12.dat')
        open(unit=9, file='omega_t12.dat')

        do j=1,n
                t=t+del_t
c euler cromer  
                omg2_aux=omg2+(-sin(theta2)-gama*omg2+
     &F0*sin(omega*t))*del_t
                theta2_aux=theta2+omg2_aux*del_t
 
c faco a busca direta para a aprox do periodo para F0=0.5
                if(F0 .eq. 0.5) then
                        if(omg2*omg2_aux .lt. 0.0) then
                                ncont=ncont+1
                        end if
                end if

c casos de F0 analisados
c esses ifs foram feitos para facilitar a organizacao
c dos dados em arquivos
                if(F0 .eq. 0.0) then
                        write(3,*) t, theta2_aux
                        write(4,*) t, omg2_aux
                else if(F0 .eq. 0.5) then
                        write(6,*) t, theta2_aux
                        write(7,*) t, omg2_aux
                else if(F0 .eq. 1.2) then
                        write(8,*) t, theta2_aux
                        write(9,*) t, omg2_aux
                end if
                
                omg2=omg2_aux
                theta2=theta2_aux   
        end do
        
c calculo do periodo aproximado
        if(ncont .gt. 0) then
                tt=2*t/ncont
        end if
        
        write(*,*) "o periodo aproximado eh: ", tt
        write(*,*) "a frequencia aproximada eh: ", 1/tt

        close(3)
        close(4)
        close(6)
        close(7)
        close(8)
        close(9)
        end
