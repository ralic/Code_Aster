        interface
          subroutine calcq(s,gamcjs,pref,epssig,q,codret)
            real(kind=8) :: s(6)
            real(kind=8) :: gamcjs
            real(kind=8) :: pref
            real(kind=8) :: epssig
            real(kind=8) :: q(6)
            integer :: codret
          end subroutine calcq
        end interface
