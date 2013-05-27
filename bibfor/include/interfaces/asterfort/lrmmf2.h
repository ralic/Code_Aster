        interface
          subroutine lrmmf2(fid,nomamd,nbrfam,carafa,nbgrmx,nbatmx,&
     &infmed,nivinf,ifm)
            integer :: nbrfam
            integer :: fid
            character(*) :: nomamd
            integer :: carafa(3,nbrfam)
            integer :: nbgrmx
            integer :: nbatmx
            integer :: infmed
            integer :: nivinf
            integer :: ifm
          end subroutine lrmmf2
        end interface
