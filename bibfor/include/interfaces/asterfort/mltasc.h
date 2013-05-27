        interface
          subroutine mltasc(nbloc,lgbloc,adinit,nommat,lonmat,factol,&
     &factou,typsym)
            integer :: lonmat
            integer :: nbloc
            integer :: lgbloc(*)
            integer :: adinit(lonmat)
            character(*) :: nommat
            character(len=24) :: factol
            character(len=24) :: factou
            integer :: typsym
          end subroutine mltasc
        end interface
