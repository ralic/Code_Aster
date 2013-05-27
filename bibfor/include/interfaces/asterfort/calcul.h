        interface
          subroutine calcul(stop,optio,ligrlz,nin,lchin,lpain,nou,&
     &lchou,lpaou,base,mpic)
            integer :: nou
            integer :: nin
            character(len=1) :: stop
            character(*) :: optio
            character(*) :: ligrlz
            character(*) :: lchin(*)
            character(*) :: lpain(*)
            character(*) :: lchou(*)
            character(*) :: lpaou(*)
            character(*) :: base
            character(*) :: mpic
          end subroutine calcul
        end interface
