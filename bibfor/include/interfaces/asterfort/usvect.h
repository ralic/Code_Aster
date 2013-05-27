        interface
          subroutine usvect(coef,alphad,alpham,alphaf,prof,ndim,vect)
            real(kind=8) :: coef
            real(kind=8) :: alphad
            real(kind=8) :: alpham
            real(kind=8) :: alphaf
            real(kind=8) :: prof
            integer :: ndim
            real(kind=8) :: vect(*)
          end subroutine usvect
        end interface
