        interface
          subroutine desgfa(typent,numfam,nomfam,nbgf,nogrf,nbaf,&
     &valatt,nbnofa,nbelfa,ifm,codret)
            integer :: nbaf
            integer :: nbgf
            integer :: typent
            integer :: numfam
            character(*) :: nomfam
            character(*) :: nogrf(nbgf)
            integer :: valatt(nbaf)
            integer :: nbnofa
            integer :: nbelfa
            integer :: ifm
            integer :: codret
          end subroutine desgfa
        end interface
