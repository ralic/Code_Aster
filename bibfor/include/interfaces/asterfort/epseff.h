        interface
          subroutine epseff(applic,nb1,depl,btild,sgmtd,epsi,wgt,&
     &effint)
            character(len=6) :: applic
            integer :: nb1
            real(kind=8) :: depl(*)
            real(kind=8) :: btild(5,*)
            real(kind=8) :: sgmtd(*)
            real(kind=8) :: epsi(*)
            real(kind=8) :: wgt
            real(kind=8) :: effint(*)
          end subroutine epseff
        end interface
