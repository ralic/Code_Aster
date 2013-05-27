        interface
          subroutine rcdiff(imate,comp,temp,c,diff)
            integer :: imate
            character(len=16) :: comp
            real(kind=8) :: temp
            real(kind=8) :: c
            real(kind=8) :: diff
          end subroutine rcdiff
        end interface
