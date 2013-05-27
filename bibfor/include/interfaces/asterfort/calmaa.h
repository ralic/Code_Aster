        interface
          subroutine calmaa(moint,mate,dir,ligrmo,lchin,lpain,lpaout,&
     &num,maa)
            character(*) :: moint
            character(*) :: mate
            character(len=1) :: dir
            character(len=24) :: ligrmo
            character(len=24) :: lchin(1)
            character(len=8) :: lpain(1)
            character(len=8) :: lpaout(1)
            character(len=14) :: num
            character(len=19) :: maa
          end subroutine calmaa
        end interface
