        interface
          subroutine rebdfr(freq,nfi,nff,freqi,freqf,nmodi,nmodf,nbm,&
     &npv)
            integer :: npv
            integer :: nbm
            real(kind=8) :: freq(2,nbm,npv)
            integer :: nfi
            integer :: nff
            real(kind=8) :: freqi
            real(kind=8) :: freqf
            integer :: nmodi
            integer :: nmodf
          end subroutine rebdfr
        end interface
