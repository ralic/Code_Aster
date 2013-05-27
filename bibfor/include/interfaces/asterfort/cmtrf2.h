        interface
          subroutine cmtrf2(codcm1,codtrf,ncm1,lcm1,ntrf,ltrf,nbma,&
     &codint,lint,nint)
            integer :: nbma
            integer :: ntrf
            integer :: ncm1
            integer :: codcm1
            integer :: codtrf
            integer :: lcm1(ncm1)
            integer :: ltrf(ntrf)
            integer :: codint
            integer :: lint(nbma)
            integer :: nint
          end subroutine cmtrf2
        end interface
