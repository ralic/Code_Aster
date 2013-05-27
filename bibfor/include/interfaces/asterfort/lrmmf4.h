        interface
          subroutine lrmmf4(nbrfam,carafa,nbnoeu,nbmail,nomgro,numgro,&
     &nument,grpnoe,gpptnn,grpmai,gpptnm,nbgrno,nbgrma,infmed,nivinf,ifm&
     &)
            integer :: nbrfam
            integer :: carafa(3,nbrfam)
            integer :: nbnoeu
            integer :: nbmail
            character(*) :: nomgro
            character(*) :: numgro
            character(*) :: nument
            character(len=24) :: grpnoe
            character(len=24) :: gpptnn
            character(len=24) :: grpmai
            character(len=24) :: gpptnm
            integer :: nbgrno
            integer :: nbgrma
            integer :: infmed
            integer :: nivinf
            integer :: ifm
          end subroutine lrmmf4
        end interface
