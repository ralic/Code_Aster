        interface
          subroutine lrmgrp(grpnoe,gpptnn,nbgrno,jnogno,jlggno,grpmai,&
     &gpptnm,nbgrma,jnogma,jlggma,nomgro,numgro,nument,nbfam)
            character(len=24) :: grpnoe
            character(len=24) :: gpptnn
            integer :: nbgrno
            integer :: jnogno
            integer :: jlggno
            character(len=24) :: grpmai
            character(len=24) :: gpptnm
            integer :: nbgrma
            integer :: jnogma
            integer :: jlggma
            character(*) :: nomgro
            character(*) :: numgro
            character(*) :: nument
            integer :: nbfam
          end subroutine lrmgrp
        end interface
