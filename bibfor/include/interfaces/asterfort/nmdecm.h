        interface
          subroutine nmdecm(sddisc,ievdac,nomlis,instam,deltat,nbrpas,&
     &dtmin,retdec)
            character(len=19) :: sddisc
            integer :: ievdac
            character(len=24) :: nomlis
            real(kind=8) :: instam
            real(kind=8) :: deltat
            integer :: nbrpas
            real(kind=8) :: dtmin
            integer :: retdec
          end subroutine nmdecm
        end interface
