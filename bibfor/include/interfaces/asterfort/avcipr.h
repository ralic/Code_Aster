        interface
          subroutine avcipr(nbvec,vectn,vectu,vectv,nbordr,kwork,sommw&
     &,vwork,tdisp,tspaq,ipgn,nomcri,nomfor,fordef,fatsoc,proaxe,pseuil,&
     &method,ncycl,vmin,vmax,omin,omax)
            integer :: tdisp
            integer :: nbordr
            integer :: nbvec
            real(kind=8) :: vectn(3*nbvec)
            real(kind=8) :: vectu(3*nbvec)
            real(kind=8) :: vectv(3*nbvec)
            integer :: kwork
            integer :: sommw
            real(kind=8) :: vwork(tdisp)
            integer :: tspaq
            integer :: ipgn
            character(len=16) :: nomcri
            character(len=16) :: nomfor
            logical :: fordef
            real(kind=8) :: fatsoc
            character(len=16) :: proaxe
            real(kind=8) :: pseuil
            character(len=8) :: method
            integer :: ncycl(nbvec)
            real(kind=8) :: vmin(nbvec*(nbordr+2))
            real(kind=8) :: vmax(nbvec*(nbordr+2))
            integer :: omin(nbvec*(nbordr+2))
            integer :: omax(nbvec*(nbordr+2))
          end subroutine avcipr
        end interface
