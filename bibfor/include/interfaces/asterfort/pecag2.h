        interface
          subroutine pecag2(ndim,nsymx,nsymy,np,xyp,vale,valpar)
            integer :: ndim
            logical :: nsymx
            logical :: nsymy
            integer :: np
            real(kind=8) :: xyp(2)
            real(kind=8) :: vale(*)
            real(kind=8) :: valpar(*)
          end subroutine pecag2
        end interface
