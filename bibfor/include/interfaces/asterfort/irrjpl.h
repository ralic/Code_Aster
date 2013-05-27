        interface
          subroutine irrjpl(model,nmat,mater,sigf,vind,vinf,dsde)
            integer :: nmat
            character(len=8) :: model
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            real(kind=8) :: dsde(6,6)
          end subroutine irrjpl
        end interface
