        dimension npd(0:10000)

        n=1000
        m=10000
        pm=0
        pm2=0
        
        open(unit=1, file='media_033.dat')

        do i=0,m
                nd=0
                do j=0,n
                        x=rand()
                        if(x .lt. 0.33) then
                                nd=nd+1
                        end if
                end do
                pm=pm+nd
                pm2=pm2+nd**2

                npd(nd)=npd(nd)+1
        end do 
        
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
        
        write(*,*) "media x=", xmedia
        write(*,*) "media de x**2=", xmq

        close(1)
        end
