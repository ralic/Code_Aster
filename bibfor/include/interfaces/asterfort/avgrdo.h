        interface
          subroutine avgrdo(nbvec,nbordr,vectn,vwork,tdisp,kwork,sommw&
     &,tspaq,i,nommat,nomcri,nomfor,grdvie,forvie,vala,coefpa,ncycl,vmin&
     &,vmax,omin,omax,post,cudomx,vnormx,nbplan)
            integer :: tdisp
            integer :: nbordr
            integer :: nbvec
            real(kind=8) :: vectn(3*nbvec)
            real(kind=8) :: vwork(tdisp)
            integer :: kwork
            integer :: sommw
            integer :: tspaq
            integer :: i
            character(len=8) :: nommat
            character(len=16) :: nomcri
            character(len=16) :: nomfor
            character(len=8) :: grdvie
            character(len=16) :: forvie
            real(kind=8) :: vala
            real(kind=8) :: coefpa
            integer :: ncycl(nbvec)
            real(kind=8) :: vmin(nbvec*(nbordr+2))
            real(kind=8) :: vmax(nbvec*(nbordr+2))
            integer :: omin(nbvec*(nbordr+2))
            integer :: omax(nbvec*(nbordr+2))
            logical :: post
            real(kind=8) :: cudomx
            integer :: vnormx(2)
            integer :: nbplan
          end subroutine avgrdo
        end interface
