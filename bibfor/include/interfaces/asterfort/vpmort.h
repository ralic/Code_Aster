        interface
          subroutine vpmort(neq,x,y,my,imode)
            integer :: neq
            real(kind=8) :: x(neq)
            real(kind=8) :: y(neq,*)
            real(kind=8) :: my(neq,*)
            integer :: imode
          end subroutine vpmort
        end interface
