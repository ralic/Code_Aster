        interface
          subroutine apchar(typcha,k24rc,nk,lambda,theta,lraide,lmasse&
     &,ldynam,solveu,lamor,lc,impr,ifapm,ind)
            character(len=8) :: typcha
            character(len=24) :: k24rc
            integer :: nk
            complex(kind=8) :: lambda
            real(kind=8) :: theta
            integer :: lraide
            integer :: lmasse
            integer :: ldynam
            character(len=19) :: solveu
            integer :: lamor
            logical :: lc
            character(len=3) :: impr
            integer :: ifapm
            integer :: ind
          end subroutine apchar
        end interface
