        interface
          subroutine aceinc(noma,nomo,nbmcf,mclf,ntyele,nbocc,ivr,&
     &nbepo,nbedi,nbeco,nbeca,nbeba,nbema,nbegb,nbemb,nbtel,locaco,&
     &locagb,locamb,jdlm,jdln,lmax,ier)
            character(len=8) :: noma
            character(len=8) :: nomo
            integer :: nbmcf
            character(len=16) :: mclf(*)
            integer :: ntyele(*)
            integer :: nbocc(*)
            integer :: ivr(*)
            integer :: nbepo
            integer :: nbedi
            integer :: nbeco
            integer :: nbeca
            integer :: nbeba
            integer :: nbema
            integer :: nbegb
            integer :: nbemb
            integer :: nbtel
            logical :: locaco
            logical :: locagb
            logical :: locamb
            integer :: jdlm
            integer :: jdln
            integer :: lmax
            integer :: ier
          end subroutine aceinc
        end interface
