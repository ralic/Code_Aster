        interface
          subroutine apm012(nk,k24rc,ltest,itest,rayonc,centrc,lraide,&
     &lmasse,solveu)
            integer :: nk
            character(len=24) :: k24rc
            logical :: ltest
            integer :: itest
            real(kind=8) :: rayonc
            complex(kind=8) :: centrc
            integer :: lraide
            integer :: lmasse
            character(len=19) :: solveu
          end subroutine apm012
        end interface
