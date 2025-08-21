        write(*,*) "digite a dimensao d:"
        read(*,*) id
       
        write(*,*) "digite o raio:"
        read(*,*) r
        
c loop para escrever as dimensoes e seus respectivos volumes num arq
c aqui, foram abertos 3 arq diferentes para guardar os casos em que r=0.9,1.0 e
c 1.1, pois esses valores serao usados no plot de um grafico
        if(r .eq. 0.9) then
                open(1, file='dim-esferas09.dat')
                do i=0,id
                        write(1,*) i, fgamma(i,r)
                end do
        end if
        
        if(r .eq. 1.0) then
                open(2, file='dim-esferas.dat')
                do i=0,id
                        write(2,*) i, fgamma(i,r)
                end do
        end if

        if(r .eq. 1.1) then
                open(3, file='dim-esferas.11')
                do i=0,id
                        write(3,*) i, fgamma(i,r)
                end do
        end if

        close(1)
        close(2)
        close(3)

        end

        function fgamma(id,r)
        pi=acos(-1.0e0)
        xgamma=1.0
        aux=1.0

c argumento da minha funcao gama
        x=id/2.0+1.0
        
10      if(x .gt. 0.0) then
c casos base gamma(1)=1 e gamma(1/2)=sqrt(pi)
                if(x .eq. 1.0) then
                        aux=1.0
                else if(x .eq. 0.5) then
                        aux=sqrt(pi)
c calculo gamma(n)=(n-1)!
                else if(x .gt. 1.0) then
                        xgamma=xgamma*(x-1)
                end if

                x=x-1
                xgamma=xgamma*aux
        go to 10
        end if
c formula do volume da esfera
        v=(pi**(id/2.0)/xgamma)*r**id
        fgamma=v
        end

