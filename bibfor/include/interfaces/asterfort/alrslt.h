        interface
          subroutine alrslt(iopt,ligrel,nout,lchout,lpaout,base,ldist,&
     &lfeti)
            integer :: iopt
            character(len=19) :: ligrel
            integer :: nout
            character(*) :: lchout(*)
            character(len=8) :: lpaout(*)
            character(*) :: base
            logical :: ldist
            logical :: lfeti
          end subroutine alrslt
        end interface
