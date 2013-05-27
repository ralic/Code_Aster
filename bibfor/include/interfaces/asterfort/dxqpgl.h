        interface
          subroutine dxqpgl(xyzg,pgl,kstop,iret)
            real(kind=8) :: xyzg(3,*)
            real(kind=8) :: pgl(3,3)
            character(len=1) :: kstop
            integer :: iret
          end subroutine dxqpgl
        end interface
