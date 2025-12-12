        parameter(g=9.8, l=9.8, del_t=0.01, tmax=50)
        pi=acos(-1e0)

        open(unit=1, file='energia1.dat')
        open(unit=2, file='theta1.dat')
        open(unit=3, file='energia2.dat')
        open(unit=4, file='theta2.dat')

        t=0

c condicoes iniciais euler
        e=0
        omg=0
        theta=0.1

c condicoes iniciais euler cromer   
        e2=0
        omg2=0
        theta2=0.1

c n=5000
        n=tmax/del_t
        do i=1,n
                t=t+del_t

c euler                
                omg_aux=omg-theta*del_t
                theta_aux=theta+omg*del_t

c euler cromer (e.c.)               
                omg2_aux=omg2-theta2*del_t
                theta2_aux=theta2+omg2_aux*del_t

c e = energia euler; e2 = energia e.c.
c os valores foram multiplicados por 10
c para que o grafico ficasse visualmente
c mais bonito
                e=10*0.5*(g*l*(omg**2+theta**2)*del_t**2)
                e2=10*0.5*(g*l*(omg2**2+theta2**2)*del_t**2)

c preparo as variaveis para mais um i
                omg=omg_aux
                theta=theta_aux 
                omg2=omg2_aux
                theta2=theta2_aux

c guardo as energias e thetas em arquivos
                write(1,*) t, e
                write(2,*) t, theta
                write(3,*) t, e2
                write(4,*) t, theta2
        end do
        close(1)
        close(2)
        close(3)
        close(4)
        end
