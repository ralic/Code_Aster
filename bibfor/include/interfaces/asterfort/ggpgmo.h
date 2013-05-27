        interface
          subroutine ggpgmo(s,h,theta,deuxmu,g,devpkk,dgdst,dgdev,&
     &tschem)
            real(kind=8) :: s
            real(kind=8) :: h
            real(kind=8) :: theta
            real(kind=8) :: deuxmu
            real(kind=8) :: g
            real(kind=8) :: devpkk
            real(kind=8) :: dgdst
            real(kind=8) :: dgdev
            real(kind=8) :: tschem
          end subroutine ggpgmo
        end interface
