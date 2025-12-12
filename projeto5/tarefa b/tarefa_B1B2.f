        implicit real*8(a-h, o-z)
        pi=acos(-1d0)

c valores das constantes GMs, GMt e GMj
        gm=4*pi**2
        gmj=gm*1.0d-3
        gmt=(gm/3)*1.0d-5

        open(unit=1, file='terra.dat')
        open(unit=3, file='jupiter.dat')

        dt=0.0001d0
                
c tempo longo de simulacao para abranger 
c todos os diferentes periodos
        n=10/dt

        rt=1.0d0
        rj=5.2d0
        eps=0.017d0
        
c assumindo orbita geral para a terra
        vyt=dsqrt(gm*(1-eps)/rt*(1+eps))
c assumindo orbita circular para jup
        vyj=dsqrt(gm/rj)

        vxt=0.0d0
        vxj=0.0d0

        xt=rt*(1+eps)
c assumindo que jupiter tem eps=0
        xj=rj

        yt=0.0d0
        yj=0.0d0
        t=0
        
        xt_old=xt-vxt*dt
        yt_old=yt-vyt*dt

        xj_old=xj-vxj*dt
        yj_old=yj-vyj*dt

        do i=1,n
                t=t+dt
c raio terra-jup para cada nova coordenada x, y                        
                rtj=dsqrt((xt-xj)**2+(yt-yj)**2)
c as coordenadas do sol sao (0,0)
c raio terra sol
                rts=dsqrt(xt**2+yt**2)
c raio jupiter sol
                rjs=dsqrt(xj**2+yj**2)
                
                axt=-gm*xt/rts**3
                ayt=-gm*yt/rts**3
                
                axj=-gm*xj/rjs**3
                ayj=-gm*yj/rjs**3
                
                r=1/rtj**3
c metodo de verlet para terra e jup               
                xt_aux=2*xt-xt_old+(axt+gmj*(xt-xj)*r)*dt**2
                yt_aux=2*yt-yt_old+(ayt+gmj*(yt-yj)*r)*dt**2
                
                xj_aux=2*xj-xj_old+(axj+gmt*(xj-xt)*r)*dt**2
                yj_aux=2*yj-yj_old+(ayj+gmt*(yj-yt)*r)*dt**2
                
                write(1,*) xt,yt
                write(3,*) xj,yj
c atualizacao das variaveis para mais uma iteracao
                xt_old=xt
                yt_old=yt
		
                xt=xt_aux
                yt=yt_aux

                xj_old=xj
                yj_old=yj

                xj=xj_aux
                yj=yj_aux
        end do

        close(1)
        close(3)
        end
