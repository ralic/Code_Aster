        interface
          subroutine propla(nbvec,vectn,vectu,vectv,nbordr,kwork,sommw&
     &,vwork,tdisp,tspaq,i,nomcri,nomfor,fordef,fatsoc,vectra)
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
            integer :: i
            character(len=16) :: nomcri
            character(len=16) :: nomfor
            logical :: fordef
            real(kind=8) :: fatsoc
            real(kind=8) :: vectra(2*nbvec*nbordr)
          end subroutine propla
        end interface
