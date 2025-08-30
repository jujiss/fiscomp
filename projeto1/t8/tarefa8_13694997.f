        write(*,*) "digite a dimensao d:"
        read(*,*) id
       
        r=1.0

        open(1, file='dim-esferas-new.dat')
        open(2, file='dim-cubo.dat')
        open(3, file='razao.dat')

        do i=0,id
                write(1,*) i, fgamma(i,r)
                write(2,*) i, vcubo(i,r)
                write(3,*) i, fgamma(i,r)/vcubo(i,r)
        end do

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
c formula do volume da esfera de raio 1
        v=(pi**(id/2.0)/xgamma)*r**id
        fgamma=v
        end
        
        function vcubo(id,r)
        vc=(2*r)**id
        vcubo=vc
        end
