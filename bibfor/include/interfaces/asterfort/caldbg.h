        interface
          subroutine caldbg(inout,ncham,lcham,lparam)
            character(*) :: inout
            integer :: ncham
            character(len=19) :: lcham(*)
            character(len=8) :: lparam(*)
          end subroutine caldbg
        end interface
