        interface
          subroutine lcjela(loi,mod,nmat,mater,vin,dsde)
            integer :: nmat
            character(len=16) :: loi
            character(len=8) :: mod
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: vin(*)
            real(kind=8) :: dsde(6,6)
          end subroutine lcjela
        end interface
