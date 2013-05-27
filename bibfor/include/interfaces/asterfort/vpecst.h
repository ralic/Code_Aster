        interface
          subroutine vpecst(ifm,typres,omgmin,omgmax,nbfre1,nbfre2,&
     &nbfreq,nblagr,typep,typcon,dimc1,zimc1)
            integer :: ifm
            character(len=16) :: typres
            real(kind=8) :: omgmin
            real(kind=8) :: omgmax
            integer :: nbfre1
            integer :: nbfre2
            integer :: nbfreq
            integer :: nblagr
            character(len=1) :: typep
            character(len=8) :: typcon
            real(kind=8) :: dimc1
            complex(kind=8) :: zimc1
          end subroutine vpecst
        end interface
