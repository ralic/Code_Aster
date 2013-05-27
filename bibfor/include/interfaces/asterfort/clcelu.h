        interface
          subroutine clcelu(piva,pivb,effm,effn,ht,enrobg,sigaci,&
     &sigbet,dnsinf,dnssup,epsilb,ierr)
            real(kind=8) :: piva
            real(kind=8) :: pivb
            real(kind=8) :: effm
            real(kind=8) :: effn
            real(kind=8) :: ht
            real(kind=8) :: enrobg
            real(kind=8) :: sigaci
            real(kind=8) :: sigbet
            real(kind=8) :: dnsinf
            real(kind=8) :: dnssup
            real(kind=8) :: epsilb
            integer :: ierr
          end subroutine clcelu
        end interface
