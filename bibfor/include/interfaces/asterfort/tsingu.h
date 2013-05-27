        interface
          subroutine tsingu(nelem,nbr,re,taille,he)
            integer :: nelem
            integer :: nbr(nelem)
            real(kind=8) :: re(nelem)
            real(kind=8) :: taille(nelem)
            real(kind=8) :: he(nelem)
          end subroutine tsingu
        end interface
