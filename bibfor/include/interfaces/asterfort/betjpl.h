        interface
          subroutine betjpl(mod,nmat,mater,sig,vin,elgeom,dsde)
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: elgeom(*)
            real(kind=8) :: dsde(6,6)
          end subroutine betjpl
        end interface
