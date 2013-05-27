        interface
          subroutine avcrit(nbvec,nbordr,vectn,vwork,tdisp,kwork,sommw&
     &,tspaq,i,vala,coefpa,ncycl,vmin,vmax,omin,omax,nomcri,nomfor,gdreq&
     &)
            integer :: tdisp
            integer :: nbordr
            integer :: nbvec
            real(kind=8) :: vectn(3*nbvec)
            real(kind=8) :: vwork(tdisp)
            integer :: kwork
            integer :: sommw
            integer :: tspaq
            integer :: i
            real(kind=8) :: vala
            real(kind=8) :: coefpa
            integer :: ncycl(nbvec)
            real(kind=8) :: vmin(nbvec*(nbordr+2))
            real(kind=8) :: vmax(nbvec*(nbordr+2))
            integer :: omin(nbvec*(nbordr+2))
            integer :: omax(nbvec*(nbordr+2))
            character(len=16) :: nomcri
            character(len=16) :: nomfor
            real(kind=8) :: gdreq(nbvec*nbordr)
          end subroutine avcrit
        end interface
