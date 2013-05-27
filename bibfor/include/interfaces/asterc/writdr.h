        interface
          subroutine writdr(unit,buff,nbytes,irec,indic,s,ierr)
            integer :: unit
            integer(kind=1) :: buff(*)
            integer :: nbytes
            integer :: irec
            integer :: indic
            integer :: s
            integer :: ierr
          end subroutine writdr
        end interface
