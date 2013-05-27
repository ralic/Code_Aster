        interface
          subroutine lrmhdf(nomamd,nomu,ifm,nrofic,nivinf,infmed,&
     &nbnoeu,nbmail,nbcoor,vecgrm,nbcgrm)
            character(*) :: nomamd
            character(len=8) :: nomu
            integer :: ifm
            integer :: nrofic
            integer :: nivinf
            integer :: infmed
            integer :: nbnoeu
            integer :: nbmail
            integer :: nbcoor
            character(len=24) :: vecgrm
            integer :: nbcgrm
          end subroutine lrmhdf
        end interface
