        write(*,*) "digite M:"
        read(*,*) m

        n=1000
        nd=0
        ne=0

        do i=0,m
                do j=0,n
                        x=rand()
                        if(x .lt. 0.5) then
                                nd=nd+1
                        else
                                ne=ne+1
                        end if
                end do
        end do 
        
        write(*,*) "nd=", nd
        write(*,*) "ne=", ne
        end
