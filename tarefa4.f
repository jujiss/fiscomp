        double precision dx
        double precision dserie
        double precision dprec
        double precision dtermo
        double precision ddif

        write(*,*) "digite x"
        read(*,*) x
        
        prec=1e-5
        dprec=1e-16
        serie=0.0
        dserie=0.0
        n=1
        dn=1

        termo=2*prec
        dtermo=2*dprec
        
        if(x .ge. 2.0) then
                write(*,*) "a serie diverge"
        else if(x .le. 0.0) then    
                write(*,*) "fora do dominio"
        go to 10
        else
c loop que calcula o ln em precisao simples
                do while(abs(termo) .ge. prec)
                        termo=(1.0-x)**n/n
                        serie=serie-termo
                        n=n+1
                end do
c meu x agora tem dupla precisao
                dx=x
c loop que calcula o ln em dupla precisao
                do while(abs(dtermo) .ge. dprec)
                        dtermo=(1.0-dx)**dn/dn
                        dserie=dserie-dtermo
                        dn=dn+1
                end do

                dif=abs(serie-log(x))
                ddif=abs(dserie-dlog(dx))

                write(*,*) "O valor da serie eh:", serie
                write(*,*) "serie-log(x)=", dif
                write(*,*) "o valor da serie em dupla prec (dserie) eh:", dserie
                write(*,*) "dserie-dlog(x)=", ddif        
        end if

10      end
