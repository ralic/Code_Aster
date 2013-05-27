        interface
          subroutine radial(nbsig,sigm,sigp,indm,indp,icine,xm,xp,&
     &normdn)
            integer :: nbsig
            real(kind=8) :: sigm(nbsig)
            real(kind=8) :: sigp(nbsig)
            real(kind=8) :: indm
            real(kind=8) :: indp
            integer :: icine
            real(kind=8) :: xm(6)
            real(kind=8) :: xp(6)
            real(kind=8) :: normdn
          end subroutine radial
        end interface
