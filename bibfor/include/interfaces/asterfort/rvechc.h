        interface
          subroutine rvechc(dim,ssch19,sdlieu,sdeval,nbndf,clocf)
            character(len=2) :: dim
            character(len=19) :: ssch19
            character(len=19) :: sdlieu
            character(len=19) :: sdeval
            integer :: nbndf(6,*)
            integer :: clocf(6,4,*)
          end subroutine rvechc
        end interface
