        interface
          subroutine nmdecc(nomlis,linfo,optdez,deltat,instam,ratio,&
     &typdec,nbrpas,deltac,dtmin,retdec)
            character(len=24) :: nomlis
            logical :: linfo
            character(*) :: optdez
            real(kind=8) :: deltat
            real(kind=8) :: instam
            real(kind=8) :: ratio
            character(len=4) :: typdec
            integer :: nbrpas
            real(kind=8) :: deltac
            real(kind=8) :: dtmin
            integer :: retdec
          end subroutine nmdecc
        end interface
