        interface
          subroutine rigmi2(noma,nogr,ifreq,nfreq,ifmis,rigma,rigma2,&
     &rigto)
            character(len=8) :: noma
            character(len=24) :: nogr
            integer :: ifreq
            integer :: nfreq
            integer :: ifmis
            real(kind=8) :: rigma(*)
            real(kind=8) :: rigma2(*)
            real(kind=8) :: rigto(*)
          end subroutine rigmi2
        end interface
