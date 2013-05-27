        interface
          subroutine aceaor(noma,nomo,lmax,nbepo,nbedi,nbtel,ntyele,&
     &nomele,ivr,ifm,nbocc)
            character(len=8) :: noma
            character(len=8) :: nomo
            integer :: lmax
            integer :: nbepo
            integer :: nbedi
            integer :: nbtel
            integer :: ntyele(*)
            character(len=16) :: nomele(*)
            integer :: ivr(*)
            integer :: ifm
            integer :: nbocc(*)
          end subroutine aceaor
        end interface
