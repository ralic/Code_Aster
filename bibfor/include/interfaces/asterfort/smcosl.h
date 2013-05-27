        interface
          subroutine smcosl(trc,ind,a,b,x,nbhist)
            integer :: nbhist
            real(kind=8) :: trc((3*nbhist),5)
            integer :: ind(6)
            real(kind=8) :: a(6,6)
            real(kind=8) :: b(6)
            real(kind=8) :: x(5)
          end subroutine smcosl
        end interface
