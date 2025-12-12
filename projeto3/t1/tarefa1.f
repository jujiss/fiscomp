      implicit real*8 (a-h, o-z)

      external f

      x=0.5d0

      open(unit=3, file='derivadas.dat')

      t=tanh(2.0d0*x)
      c=1.0d0/cosh(2.0d0*x)

c derivadas calculadas
      df1=2.0d0*exp(2.0d0*x**2)*(2.0d0*x*t+c**2)

      df2=-4.0d0*exp(2.0d0*x**2)*
     &     (((2.0d0*c**2-4.0d0*x**2-1.0d0)*t)-4.0d0*x*c**2)

      write(3,10)
 10   format(1x,13('-'))
      write(3,11)
 11   format(5x,'h',10x,"f'_2f",15x,"f'_2t",15x,
     &        "f'_3s",15x,"f'_5s",15x,
     &        "f''_3s",15x,"f''_5s")
      write(3,10)

c derivadas numericas
      do i=1,12
         h=5.0d0**(-i)

c funcao f(X) calculada para diferentes valores 
c que serao utilizados nos calc de derivadas
         ff=f(x+h)
         ft=f(x-h)
         f2f=f(x+2.0d0*h)
         f2t=f(x-2.0d0*h)

c derivada 2 pts para frente
         dff=(ff-f(x))/h
c derivada 2 pts para tras
         dft=(f(x)-ft)/h

c derivada simetrica de 3 pts
         df3s=(ff-ft)/(2.0d0*h)
c derivada segunda simetrica de 3pts
         df3s2=(ff-2.0d0*f(x)+ft)/(h*h)

c derivada simetrica de 5 pts
         df5s=(f2t-8.0d0*ft+8.0d0*ff-f2f)/(12.0d0*h)
c derivada segunda simetrica de 5 pts
         df5s2=(-f2t+16.0d0*ft-30.0d0*f(x)+16.0d0*ff-f2f) 
     &           /(12.0d0*h*h)
         
         write(3,12) h, dff, dft, df3s, df5s, df3s2, df5s2
 12      format(1x,es12.4,6(2x,f18.12))
      end do

      write(3,10)
      write(3,13) df1, df2
 13   format(1x,'Exato',7x,2(2x,f18.12),4(2x,'-'))
      write(3,10)
      
      close(3)
      end

      double precision function f(x)
      double precision x
      f=exp(2.0d0*x**2)*tanh(2.0d0*x)
      return
      end

