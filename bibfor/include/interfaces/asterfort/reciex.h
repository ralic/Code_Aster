        interface
          subroutine reciex(intexc,iderex,nindex,nnoeex,ncmpex,nvasex,&
     &graexc,excmod,napexc)
            character(len=8) :: intexc
            integer :: iderex
            integer :: nindex
            integer :: nnoeex
            integer :: ncmpex
            integer :: nvasex
            character(len=16) :: graexc
            character(len=4) :: excmod
            integer :: napexc
          end subroutine reciex
        end interface
