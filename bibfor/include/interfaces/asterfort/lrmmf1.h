        interface
          subroutine lrmmf1(fid,nomamd,nbrfam,carafa,nbnoeu,famnoe,&
     &nmatyp,jfamma,jnumty,tabaux,nomgro,numgro,nument,infmed,nivinf,ifm&
     &,vecgrm,nbcgrm)
            integer :: nbnoeu
            integer :: nbrfam
            integer :: fid
            character(*) :: nomamd
            integer :: carafa(3,nbrfam)
            integer :: famnoe(nbnoeu)
            integer :: nmatyp(69)
            integer :: jfamma(69)
            integer :: jnumty(69)
            integer :: tabaux(*)
            character(*) :: nomgro
            character(*) :: numgro
            character(*) :: nument
            integer :: infmed
            integer :: nivinf
            integer :: ifm
            character(*) :: vecgrm
            integer :: nbcgrm
          end subroutine lrmmf1
        end interface
