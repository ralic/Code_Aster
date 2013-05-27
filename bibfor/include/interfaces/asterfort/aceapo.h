        interface
          subroutine aceapo(noma,nomo,lmax,npoutr,nbocc,mclf,nbepo,&
     &ntyele,ivr,ifm,jdlm)
            character(len=8) :: noma
            character(len=8) :: nomo
            integer :: lmax
            integer :: npoutr
            integer :: nbocc
            character(*) :: mclf
            integer :: nbepo
            integer :: ntyele(*)
            integer :: ivr(*)
            integer :: ifm
            integer :: jdlm
          end subroutine aceapo
        end interface
