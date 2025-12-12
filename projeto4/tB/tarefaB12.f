       parameter(g=9.8, l=9.8, tmax=1000, del_t=0.3)
       parameter(eps=0.00001)
       pi=acos(-1e0)

        open(unit=2, file='theta0_new.dat')
c condicoes iniciais euler cromer   
        n=tmax/del_t

c construcao de uma tabela de comparacao dos result
        write(*,*) "   theta0        T aproximado    T pela integral   
     &T theta pequeno"

        do i=1,8
c le o arq com valores de theta0
                read(2,*) theta0
c extremos da integral
                a=-theta0+eps
                b=theta0-eps
c valor inicial da integral
                simp=f(a,theta0)+f(b,theta0)

c as variaveis sao zeradas para cada valor de theta0
                theta2=theta0
                tt=0
                omg2=0
                ncont=0
                t=0

c tamanho do passo da integral
                h=2*theta0/n

                do j=1,n
                        t=t+del_t
c euler cromer                
                        omg2_aux=omg2-sin(theta2)*del_t
                        theta2_aux=theta2+omg2_aux*del_t
                
c busca direta
                        if(omg2*omg2_aux .lt. 0.0) then
                                ncont=ncont+1
                        end if

c reinicio as variaveis
                        omg2=omg2_aux
                        theta2=theta2_aux

c calculo da int por simpson
                        do k=1,n-1
                                if(mod(k,2) .eq. 0) then
                                        simp=simp+2*f(a+k*h,theta0)
                                else
                                        simp=simp+4*f(a+k*h,theta0)
                                end if
                        end do
                        simp=(h*simp)/3
c o resultado final do periodo eh a integral calculada 
c adicionada da parte calculada analiticamente
                        resul=sqrt(2.0)*(simp+2*sqrt(eps/sin(theta0)))
                       
                end do
                
c periodo eh 2*o tempo final dividido pela qntd de 
c vezes em que foram identificadas mudanca de sinal 
c de omg2*omg2_aux
                tt=t/(ncont/2)
                
c periodo aproximado para t pequeno
                tt_apr=2*pi*(1+theta0**2/16)
                write(*,*) theta0, tt, resul, tt_apr
        end do

        close(2)
        end

c funcao a integrada
        function f(theta2, theta0)
        pi=acos(-1e0)
        f=1/sqrt(cos(theta2)-cos(theta0))
        return
        end
