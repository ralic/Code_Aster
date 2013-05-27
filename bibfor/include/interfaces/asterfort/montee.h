        interface
          subroutine montee(opt,ligrel,nout,lchout,lpaout,fin)
            integer :: opt
            character(len=19) :: ligrel
            integer :: nout
            character(*) :: lchout(*)
            character(len=8) :: lpaout(*)
            character(*) :: fin
          end subroutine montee
        end interface
