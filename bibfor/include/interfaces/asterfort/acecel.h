        interface
          subroutine acecel(noma,nomo,nbocc,nbepo,nbedi,nbeco,nbeca,&
     &nbeba,nbema,nbegb,nbtel,ntyele,npoutr,ndiscr,ncoque,ncable,nbarre,&
     &nmassi,ngrill,ngribt,nmembr,jdlm,jdln,ier)
            character(len=8) :: noma
            character(len=8) :: nomo
            integer :: nbocc(*)
            integer :: nbepo
            integer :: nbedi
            integer :: nbeco
            integer :: nbeca
            integer :: nbeba
            integer :: nbema
            integer :: nbegb
            integer :: nbtel
            integer :: ntyele(*)
            integer :: npoutr
            integer :: ndiscr
            integer :: ncoque
            integer :: ncable
            integer :: nbarre
            integer :: nmassi
            integer :: ngrill
            integer :: ngribt
            integer :: nmembr
            integer :: jdlm
            integer :: jdln
            integer :: ier
          end subroutine acecel
        end interface
