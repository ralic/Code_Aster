        interface
          subroutine asefen(muapde,nomsy,id,stat,neq,nbsup,ndir,nsupp,&
     &masse,nomsup,depsup,recmod,nintra,nbdis)
            integer :: nbsup
            integer :: neq
            logical :: muapde
            character(len=16) :: nomsy
            integer :: id
            character(*) :: stat
            integer :: ndir(*)
            integer :: nsupp(*)
            character(*) :: masse
            character(*) :: nomsup(nbsup,*)
            real(kind=8) :: depsup(nbsup,*)
            real(kind=8) :: recmod(nbsup,neq,*)
            integer :: nintra
            integer :: nbdis(nbsup)
          end subroutine asefen
        end interface
