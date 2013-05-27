        interface
          subroutine vdsiro(np,nbsp,matev,sens,goun,tens1,tens2)
            integer :: np
            integer :: nbsp
            real(kind=8) :: matev(2,2,1)
            character(len=2) :: sens
            character(len=1) :: goun
            real(kind=8) :: tens1(1)
            real(kind=8) :: tens2(1)
          end subroutine vdsiro
        end interface
