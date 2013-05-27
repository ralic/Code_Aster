        interface
          subroutine readdr(unit,buff,nbytes,irec,ierr)
            real(kind=8) :: unit
            integer(kind=1) :: buff(*)
            integer :: nbytes
            integer :: irec
            integer :: ierr
          end subroutine readdr
        end interface
