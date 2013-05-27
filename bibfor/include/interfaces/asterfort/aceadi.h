        interface
          subroutine aceadi(noma,nomo,mcf,lmax,nbocc,ivr,ifm)
            character(len=8) :: noma
            character(len=8) :: nomo
            character(*) :: mcf
            integer :: lmax
            integer :: nbocc
            integer :: ivr(*)
            integer :: ifm
          end subroutine aceadi
        end interface
