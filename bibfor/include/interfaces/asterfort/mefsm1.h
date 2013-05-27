        interface
          subroutine mefsm1(vale,matgen,base,nomnum,nomsto,nbmode,&
     &nbloc,nterm)
            real(kind=8) :: vale(*)
            character(len=19) :: matgen
            character(len=1) :: base
            character(len=19) :: nomnum
            character(len=19) :: nomsto
            integer :: nbmode
            integer :: nbloc
            integer :: nterm
          end subroutine mefsm1
        end interface
