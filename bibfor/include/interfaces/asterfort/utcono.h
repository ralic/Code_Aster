        interface
          subroutine utcono(mcfac,mocle,iocc,nomail,ndim,coor,iret)
            character(*) :: mcfac
            character(*) :: mocle(3)
            integer :: iocc
            character(len=8) :: nomail
            integer :: ndim
            real(kind=8) :: coor(*)
            integer :: iret
          end subroutine utcono
        end interface
