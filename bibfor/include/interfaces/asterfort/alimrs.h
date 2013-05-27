        interface
          subroutine alimrs(mate,ma1,ma2,moint,ndble,num,cn1,chno,cmp,&
     &icor)
            character(*) :: mate
            character(len=8) :: ma1
            character(len=8) :: ma2
            character(*) :: moint
            integer :: ndble
            character(len=14) :: num
            character(len=19) :: cn1
            character(*) :: chno
            character(*) :: cmp
            integer :: icor(2)
          end subroutine alimrs
        end interface
