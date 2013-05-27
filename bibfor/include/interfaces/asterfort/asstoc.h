        interface
          subroutine asstoc(mome,resu,nomsy,neq,repdir,ndir,comdir,&
     &typcdi,glob,prim)
            integer :: neq
            character(*) :: mome
            character(*) :: resu
            character(len=16) :: nomsy
            real(kind=8) :: repdir(neq,*)
            integer :: ndir(*)
            logical :: comdir
            character(*) :: typcdi
            logical :: glob
            logical :: prim
          end subroutine asstoc
        end interface
