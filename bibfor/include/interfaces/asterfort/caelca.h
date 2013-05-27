        interface
          subroutine caelca(modele,chmat,caelem,irana1,icabl,nbnoca,&
     &numaca,regl,relax,ea,rh1000,prelax,fprg,frco,frli,sa)
            character(len=8) :: modele
            character(len=8) :: chmat
            character(len=8) :: caelem
            integer :: irana1
            integer :: icabl
            integer :: nbnoca(*)
            character(len=19) :: numaca
            character(len=4) :: regl
            logical :: relax
            real(kind=8) :: ea
            real(kind=8) :: rh1000
            real(kind=8) :: prelax
            real(kind=8) :: fprg
            real(kind=8) :: frco
            real(kind=8) :: frli
            real(kind=8) :: sa
          end subroutine caelca
        end interface
