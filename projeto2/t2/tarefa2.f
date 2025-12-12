c vetor que armazena os passos a direita
        dimension npd(0:10000)

        n=1000
        m=10000
        pm=0
        pm2=0

c arq que armazena os passos dos andarilhos
        open(unit=1, file='media_x.dat')

        do i=1,m
c nd (passo a direita) eh inicializado para cada andarilho
                nd=0
                do j=1,n
                        x=rand()
c considero que qualquer numero aleatorio gerado menor do que 0.5 equivale a um passo a direita
                        if(x .lt. 0.5) then
                                nd=nd+1
                        end if
                end do
c se o passo for a direita, adiciono nd ao npm (media do numero de passos) e nd**2 ao npm2 
c (media quadrada do numero de passos)
                pm=pm+nd
                pm2=pm2+nd**2
c guardo o passo a direita atual no vetor de passos a direita
                npd(nd)=npd(nd)+1
        end do 
        
c guardo o resultado dos passos de cada andarilho no arq
        do k=0,n
                write(1,*) k, npd(k)
        end do

c media de p
        pmedia=pm/m
        xmedia=(2*pmedia)-n
c media de p**2
        pmq=pm2/m
c formula da media de x**2
        xmq=4*pmq-4*n*pmedia+n**2
        
        write(*,*) "media p=", pmedia
        write(*,*) "media x=", xmedia
        write(*,*) "media de x**2=", xmq
        
        close(1)
        end
