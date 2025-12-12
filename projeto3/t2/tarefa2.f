        implicit real*8 (a-h, o-z)
        parameter (pi=acos(-1d0))
        external f

        a=0.0d0
        b=2.0d0*pi

c     valor exato da integral
        rint=(1.0d0-dexp(-b))/2.0d0
      
        open(unit=1, file='integrais.dat')

        write(1,10)
   10   format(1x,' i',3x,'   n',6x,'   trapezio',13x,'simpson',
     &      15x,'boole')
        write(1,11)
   11   format(1x,70('-'))

        do i=2,13
                n=2**i
                h=(b-a)/dfloat(n)
         
                trap=(int(1/2d0))*(f(a)+f(b))
                simp=f(a)+f(b) 
                boole=0.0d0

c regra do trapezio

c o loop vai ate b-h
         do j=1,n-1
                x=a+j*h

                trap=trap+f(x)
         end do
c multiplico por h fora do loop
         trap=trap*h
         
c regra de simpson 
         do j=1,n-1
                x=a+j*h
c crio um caso para funcoes com j's pares 
c e outro para j's impares, por conta da
c definicao da regra de simpson
                if(mod(j,2).eq.0) then
                        simp=simp+2.0d0*f(x)
                else
                        simp=simp+4.0d0*f(x)
                end if
         end do

         simp=(h*simp)/3.0d0

c regra de boole, que considera intervalos de 4 pts
         do j=0,n-4,4
                x=a+j*h

                f1=f(x+h)
                f2=f(x+2*h)
                f3=f(x+3*h)
                f4=f(x+4*h)

                boole=boole+7.0d0*(f4+f(x))
     &          +32.0d0*(f3+f1)+12.0d0*f2
         end do
         boole=(2.0d0*h/45.0d0)*boole

         write(1,12) i, n, trap, simp, boole
 12      format(1x,i2,3x,i6,3(2x,f18.12))

      end do

      write(1,*)
      write(1,13) rint
 13   format('valor exato da integral=',f18.12)

      close(1)
      end

      double precision function f(x)
      double precision x
      f=dexp(-x)*dsin(x)
      return
      end

