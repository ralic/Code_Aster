        interface
          subroutine clcels(cequi,effm,effn,ht,enrobg,sigaci,sigbet,&
     &dnsinf,dnssup,sigmab,ierr)
            real(kind=8) :: cequi
            real(kind=8) :: effm
            real(kind=8) :: effn
            real(kind=8) :: ht
            real(kind=8) :: enrobg
            real(kind=8) :: sigaci
            real(kind=8) :: sigbet
            real(kind=8) :: dnsinf
            real(kind=8) :: dnssup
            real(kind=8) :: sigmab
            integer :: ierr
          end subroutine clcels
        end interface
