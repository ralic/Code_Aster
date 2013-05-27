        interface
          subroutine tabcor(model,mate,ma1,ma2,moint,num,ndble,icor)
            character(len=2) :: model
            character(*) :: mate
            character(len=8) :: ma1
            character(len=8) :: ma2
            character(*) :: moint
            character(len=14) :: num
            integer :: ndble
            integer :: icor(2)
          end subroutine tabcor
        end interface
