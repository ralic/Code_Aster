        interface
          subroutine epsett(applic,nbrddl,depl,btild,sgmtd,epsi,wgt,&
     &effint)
            character(len=6) :: applic
            integer :: nbrddl
            real(kind=8) :: depl(*)
            real(kind=8) :: btild(4,*)
            real(kind=8) :: sgmtd(*)
            real(kind=8) :: epsi(*)
            real(kind=8) :: wgt
            real(kind=8) :: effint(*)
          end subroutine epsett
        end interface
