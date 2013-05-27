        interface
          subroutine vpermo(lmasse,lraide,nbprop,vecp,valp,excl,omecor&
     &,ernorm)
            integer :: lmasse
            integer :: lraide
            integer :: nbprop
            real(kind=8) :: vecp(*)
            real(kind=8) :: valp(*)
            integer :: excl(*)
            real(kind=8) :: omecor
            real(kind=8) :: ernorm(*)
          end subroutine vpermo
        end interface
