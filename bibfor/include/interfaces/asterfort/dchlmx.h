        interface
          subroutine dchlmx(opt,ligrel,iparg,nin,lpain,nout,lpaout,&
     &taille)
            integer :: opt
            character(len=19) :: ligrel
            integer :: iparg
            integer :: nin
            character(len=8) :: lpain(*)
            integer :: nout
            character(len=8) :: lpaout(*)
            integer :: taille
          end subroutine dchlmx
        end interface
