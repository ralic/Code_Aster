        interface
          subroutine nmdeca(sddisc,iterat,ievdac,nomlis,instam,deltat,&
     &nbrpas,dtmin,ldcext,durdec,retdec)
            character(len=19) :: sddisc
            integer :: iterat
            integer :: ievdac
            character(len=24) :: nomlis
            real(kind=8) :: instam
            real(kind=8) :: deltat
            integer :: nbrpas
            real(kind=8) :: dtmin
            logical :: ldcext
            real(kind=8) :: durdec
            integer :: retdec
          end subroutine nmdeca
        end interface
