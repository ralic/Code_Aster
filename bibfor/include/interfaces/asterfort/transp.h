        interface
          subroutine transp(a,nlamax,dimal,dimac,b,nlbmax)
            integer :: nlbmax
            integer :: nlamax
            real(kind=8) :: a(nlamax,*)
            integer :: dimal
            integer :: dimac
            real(kind=8) :: b(nlbmax,*)
          end subroutine transp
        end interface
