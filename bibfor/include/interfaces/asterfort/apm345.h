        interface
          subroutine apm345(nbtetc,typcon,rayonc,centrc,nk,k24rc,&
     &pivot2,ltest,typcha,lraide,lmasse,ldynam,solveu,lamor,lc,impr,&
     &ifapm)
            integer :: nbtetc
            character(len=8) :: typcon
            real(kind=8) :: rayonc
            complex(kind=8) :: centrc
            integer :: nk
            character(len=24) :: k24rc
            integer :: pivot2
            logical :: ltest
            character(len=8) :: typcha
            integer :: lraide
            integer :: lmasse
            integer :: ldynam
            character(len=19) :: solveu
            integer :: lamor
            logical :: lc
            character(len=3) :: impr
            integer :: ifapm
          end subroutine apm345
        end interface
