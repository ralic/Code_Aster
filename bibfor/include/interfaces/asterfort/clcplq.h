        interface
          subroutine clcplq(ht,enrobg,typcmb,piva,pivb,cequi,sigaci,&
     &sigbet,effrts,dnsits,sigmbe,epsibe,ierr)
            real(kind=8) :: ht
            real(kind=8) :: enrobg
            integer :: typcmb
            real(kind=8) :: piva
            real(kind=8) :: pivb
            real(kind=8) :: cequi
            real(kind=8) :: sigaci
            real(kind=8) :: sigbet
            real(kind=8) :: effrts(8)
            real(kind=8) :: dnsits(5)
            real(kind=8) :: sigmbe
            real(kind=8) :: epsibe
            integer :: ierr
          end subroutine clcplq
        end interface
