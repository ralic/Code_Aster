        interface
          subroutine lrmmf3(fid,nomamd,rangfa,carafa,nbnoeu,famnoe,&
     &nmatyp,jfamma,jnumty,vaatfa,nogrfa,tabaux,nomgro,numgro,nument,&
     &infmed,nivinf,ifm,vecgrm,nbcgrm,nbgrlo)
            integer :: nbnoeu
            integer :: fid
            character(*) :: nomamd
            integer :: rangfa
            integer :: carafa(3,*)
            integer :: famnoe(nbnoeu)
            integer :: nmatyp(69)
            integer :: jfamma(69)
            integer :: jnumty(69)
            integer :: vaatfa(*)
            character(*) :: nogrfa(*)
            integer :: tabaux(*)
            character(*) :: nomgro
            character(*) :: numgro
            character(*) :: nument
            integer :: infmed
            integer :: nivinf
            integer :: ifm
            character(*) :: vecgrm
            integer :: nbcgrm
            integer :: nbgrlo
          end subroutine lrmmf3
        end interface
