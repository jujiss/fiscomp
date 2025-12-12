        implicit real*8 (a-h, o-z)
        parameter (h=0.01d0, b=5, a=-5, eps=1e-6)
c biss armazena as raizes por bissecao, raph armazena as 
c raizes por newton-raph. e sec pela secanta
        dimension xi(10), xf(10), biss(20), raph(20), sec(20)

        external d, f
 
        open(unit=1, file='raizes.dat')    
   
        write(1,1)
1       format(9x,' ',10x, ' ',23x,"bissecao",23x
     & , ' ', 6x,"newton-raphson",8x, ' ', 23x,"secante",22x)

        write(1,2)
2       format(5x,'iteracao ',6x,' ',5x,F15.11, 3x,
     & F15.11,4x,' ',5x, F15.11, 4x,' ',5x, F15.11, 3x,
     & "xi :", F15.11,4x,' ')     
      
3       format(' ',6x, I5 ,9x,' ',19x,F15.11,20x,' ',7x,F15.11,6x,' ',
     & 19x,F15.11, 18x,' ')

4       format(' ',6x, "exato" ,9x,' ',57X, F15.11, 64X,' ') 

c busca direta
        xe=a
        xd=xe+h
        nraiz=0

c se houver mudanca de sinal entre dois valores de x
c encontrei um subintervalo 
   11   if(f(xd)*f(xe) .lt. 0.0) then
                nraiz=nraiz+1
c o subintervalo sera [xe,xd]
                xi(nraiz)=xe
                xf(nraiz)=xd
        end if

c agora eu checo um intervalo a partir do xd 
        xe=xd
        xd=xd+h
        
c a busca varre o intervalo [a,b]        
        if(xd .le. b) then
        go to 11
        else
        go to 10
        end if

   10   do i=1,nraiz
                x1=xi(i)
                x2=xf(i)

c crio vetores de 0's
                do j=1,20
                        biss(j)=0
                        raph(j)=0
                        sec(j)=0
                end do
        
c metodo da bissecao
                ibiss=0
                do while(abs(x2-x1) .gt. eps)
                        xm=(x1+x2)/2d0
                
                        if(f(x2)*f(xm) .gt. 0.0) then
                                x2=xm
                        else
                                x1=xm
                        end if
               
                        ibiss=ibiss+1
                        biss(ibiss)=xm
                end do
                
c metodo de newton-raphson
                iraph=0
c preciso atualizar x1 e x2 a cada metodo
                x1=xi(i)
                x2=xf(i)
                dif=abs(x2-x1)
                do while(dif .gt. eps)
                        if(d(xn) .eq. 0.0) then
c evitando divisao por 0
                                xn=x2-f(x2)/d(x2+eps)
                        else
                                xn=x2-f(x2)/d(x2)
                        end if
                
                        iraph=iraph+1
c a diferenca eh atualizada para o novo interv
c e sera cada vez menor pois xn --> x2
                        dif=abs(xn-x2)
                        raph(iraph)=xn
                        x2=xn
                end do
 
c metodo da secante
                isec=0
                x1=xi(i)
                x2=xf(i)
                dif=abs(x2-x1)
                do while(dif .gt. eps)
                        if((f(x2)-f(x1)) .eq. 0.0) then
c evitando divisao por 0
                                xl=x2-((x2-x1)*f(x2))/(f(x2+eps)-f(x1))
                        else
                                xl=x2-((x2-x1)*f(x2))/(f(x2)-f(x1))
                        end if

                        isec=isec+1
                        dif=abs(xl-x2)
                        sec(isec)=xl
c o novo intevalo agora sera [x2,xl]
                        x1=x2
                        x2=xl
                end do       
                
c impressao da tabela
                imax=max(max(ibiss, iraph), isec)
                do k=1,imax
                        write(1,3) k, biss(k), raph(k)
     &          , sec(k)
                end do

c linha de valores exatos 
                if(i .eq. 1) then
                        write(1,4) -0.30d0
                else
                        if(i .eq. 2) then
                                write(1,4) 0.20d0
                        else
                                write(1,4) 0.70d0
                        end if
                end if
                write(1,*)
        end do
        close(1)
        end

c funcao que calcula df/dx
        double precision function d(x)
        implicit real*8 (a-h, o-z)
        d=-0.13-1.2*x+3*x**2
        return
        end
        
        double precision function f(x)
        implicit real*8 (a-h, o-z)
        f=0.042-0.13*x-0.6*x**2+x**3
        return
        end        
