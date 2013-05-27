        interface
          subroutine utjac(l2d,geom,ipg,idfde,niv,ifm,nno,jacob)
            logical :: l2d
            real(kind=8) :: geom(*)
            integer :: ipg
            integer :: idfde
            integer :: niv
            integer :: ifm
            integer :: nno
            real(kind=8) :: jacob
          end subroutine utjac
        end interface
