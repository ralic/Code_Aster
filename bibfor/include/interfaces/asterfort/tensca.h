        interface
          subroutine tensca(tablca,icabl,nbnoca,nbf0,f0,delta,typrel,&
     &trelax,xflu,xret,ea,rh1000,mu0,fprg,frco,frli,sa,regl)
            character(len=19) :: tablca
            integer :: icabl
            integer :: nbnoca
            integer :: nbf0
            real(kind=8) :: f0
            real(kind=8) :: delta
            character(len=24) :: typrel
            real(kind=8) :: trelax
            real(kind=8) :: xflu
            real(kind=8) :: xret
            real(kind=8) :: ea
            real(kind=8) :: rh1000
            real(kind=8) :: mu0
            real(kind=8) :: fprg
            real(kind=8) :: frco
            real(kind=8) :: frli
            real(kind=8) :: sa
            character(len=4) :: regl
          end subroutine tensca
        end interface
