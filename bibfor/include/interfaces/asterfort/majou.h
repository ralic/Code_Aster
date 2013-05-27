        interface
          subroutine majou(model,modmec,solveu,num,nu,ma,mate,moint,&
     &ndble,icor,tabad)
            character(len=2) :: model
            character(len=8) :: modmec
            character(len=19) :: solveu
            character(len=14) :: num
            character(len=14) :: nu
            character(len=8) :: ma
            character(*) :: mate
            character(len=8) :: moint
            integer :: ndble
            integer :: icor(2)
            integer :: tabad(5)
          end subroutine majou
        end interface
