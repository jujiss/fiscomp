        write(*,*) "digite m:"
        read(*,*) m
c m eh a quantidade total de ptos a serem considerados
        write(*,*) "MONTE CARLO:"
        v2=montecarlo(m,2)
        v3=montecarlo(m,3)
        v4=montecarlo(m,4)
        
        write(*,*) "FORMULA:"
        v2_gamma=vol_gamma(2)
        v3_gamma=vol_gamma(3)
        v4_gamma=vol_gamma(4)
        
        end
c casos da funcao gamma para d=2,3 e 4
        function vol_gamma(id)
        pi=acos(-1e0)
        gamma=0.0

        if(id .eq. 2) then
                gamma=1.0
        end if

        if(id .eq. 3) then
                gamma=0.75*sqrt(pi)
        end if

        if(id .eq. 4) then
                gamma=2.0
        end if    
c formula do volume dada no enunciado
        vgamma=pi**(id/2.0)/gamma
       
        write(*,*) "para d=", id
        write(*,*) "funcao gamma=", gamma
        write(*,*) "volume pela formula=", vgamma
        write(*,*) "                    "

        end

c funcao que calcula o metodo monte carlo
        function montecarlo(m,id)
        n=0
   
        do i=1,m
                r=0.0
                aux=0.0
                do j=1,id
c gero numeros aletorios que compoem um raio para a esfera que eu quero
                        r=rand()
                        aux=aux+r**2
                end do
c o meu raio eh menor ou igual que 1? se sim, adiciono 1 no meu contador
c de ptos no interior de um raio 1
                if(sqrt(aux) .le. 1.0) then
                        n=n+1
                end if
        end do
c o volume contido em um raio unitario vai ser a razao entre os 
c pts interno/pts totais*a area do meu cubo em d dimensoes
        v=2.0**id*(float(n)/float(m))     
        write(*,*) "para d=", id
        write(*,*) "volume=", v
        write(*,*) "pontos dentro do raio unitario=", n
        write(*,*) "                 "
        write(*,*) "                 "

        end
