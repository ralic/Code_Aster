        interface
          subroutine avplcr(nbvec,vectn,vectu,vectv,nbordr,kwork,&
     &somnow,vwork,tdisp,tspaq,i,nomcri,nomfor,grdvie,forvie,fordef,&
     &fatsoc,proaxe,nommat,vala,coefpa,post,cudomx,nxm,nym,nzm)
            integer :: tdisp
            integer :: nbordr
            integer :: nbvec
            real(kind=8) :: vectn(3*nbvec)
            real(kind=8) :: vectu(3*nbvec)
            real(kind=8) :: vectv(3*nbvec)
            integer :: kwork
            integer :: somnow
            real(kind=8) :: vwork(tdisp)
            integer :: tspaq
            integer :: i
            character(len=16) :: nomcri
            character(len=16) :: nomfor
            character(len=8) :: grdvie
            character(len=16) :: forvie
            logical :: fordef
            real(kind=8) :: fatsoc
            character(len=16) :: proaxe
            character(len=8) :: nommat
            real(kind=8) :: vala
            real(kind=8) :: coefpa
            logical :: post
            real(kind=8) :: cudomx
            real(kind=8) :: nxm(2)
            real(kind=8) :: nym(2)
            real(kind=8) :: nzm(2)
          end subroutine avplcr
        end interface
