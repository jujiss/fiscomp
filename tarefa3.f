        parameter (max = 2000)
        double precision a(1:max) !vetor que armazena os termos do arquivo
        double precision aux

        n=0 !contador de linhas do arquivo

        open(unit=1, file="tarefa-3-entrada-1.in")
        open(unit=2, file="t3-saida.out")
         
        do j=1,max
                read(1, *, end=13) a(j) !le cada linha do arq e guarda em a
                n=n+1 !conta os termos do arq
        end do

13      write(*,*) "N=", n
        write(*,*) "digite M:"
        read(*,*) m

        do i=1,m-1 !implementacao de selection sort
                k=i
                do j=i+1,m
                        if(a(j) .lt. a(k)) then
                                k=j
                        end 
                if(k .ne. i) then
                        aux=a(i)
                        a(i)=a(k)
                        a(k)=aux
                        end if
                end do
        end do

        do l=1,m !escreve os m termos ordenados no arq de saida
                write(2,*) a(l)
        end do
        
        write(2,*) "o valor de M é:", m
        close(1)
        close(2)

        end
