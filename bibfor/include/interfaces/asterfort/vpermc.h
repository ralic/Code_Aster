        interface
          subroutine vpermc(lmasse,lraide,nbprop,vecp,fr,am,excl,&
     &omecor,ernorm)
            integer :: lmasse
            integer :: lraide
            integer :: nbprop
            complex(kind=8) :: vecp(*)
            real(kind=8) :: fr(*)
            real(kind=8) :: am(*)
            integer :: excl(*)
            real(kind=8) :: omecor
            real(kind=8) :: ernorm(*)
          end subroutine vpermc
        end interface
