        interface
          subroutine zerof2(f,x0,xap,epsi,nitmax,solu,iret,n)
            real(kind=8) :: f
            external f
            real(kind=8) :: x0
            real(kind=8) :: xap
            real(kind=8) :: epsi
            integer :: nitmax
            real(kind=8) :: solu
            integer :: iret
            integer :: n
          end subroutine zerof2
        end interface
