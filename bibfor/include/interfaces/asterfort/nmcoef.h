        interface
          subroutine nmcoef(noeu1,noeu2,typpil,nbno,cnsln,compo,vect,i&
     &,n,coef1,coef2,coefi)
            integer :: noeu1
            integer :: noeu2
            character(len=24) :: typpil
            integer :: nbno
            character(len=19) :: cnsln
            character(len=8) :: compo
            real(kind=8) :: vect(3)
            integer :: i
            integer :: n
            real(kind=8) :: coef1
            real(kind=8) :: coef2
            real(kind=8) :: coefi
          end subroutine nmcoef
        end interface
