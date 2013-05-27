        interface
          subroutine fetprc(nbsd,nbi,vd1,vd2,vdo,matas,vddl,preco,&
     &infofe,irex,ifiv,nbproc,rang,k24ir2)
            integer :: nbi
            integer :: nbsd
            real(kind=8) :: vd1(nbi)
            real(kind=8) :: vd2(nbi)
            real(kind=8) :: vdo(nbi)
            character(len=19) :: matas
            integer :: vddl(nbsd)
            character(len=24) :: preco
            character(len=24) :: infofe
            integer :: irex
            integer :: ifiv
            integer :: nbproc
            integer :: rang
            character(len=24) :: k24ir2
          end subroutine fetprc
        end interface
