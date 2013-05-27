        interface
          subroutine cnomax(cnoz,ncmp,licmp,rmax,numno)
            integer :: ncmp
            character(*) :: cnoz
            character(len=8) :: licmp(ncmp)
            real(kind=8) :: rmax
            integer :: numno
          end subroutine cnomax
        end interface
