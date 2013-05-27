        interface
          subroutine meceuc(stop,poux,option,caraez,ligrel,nin,lchin,&
     &lpain,nou,lchou,lpaou,base)
            integer :: nin
            character(len=1) :: stop
            character(len=8) :: poux
            character(*) :: option
            character(*) :: caraez
            character(*) :: ligrel
            character(*) :: lchin(*)
            character(*) :: lpain(*)
            integer :: nou
            character(*) :: lchou(*)
            character(*) :: lpaou(*)
            character(*) :: base
          end subroutine meceuc
        end interface
