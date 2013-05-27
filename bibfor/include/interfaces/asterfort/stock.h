        interface
          subroutine stock(resu,chs,nocham,ligrel,tychas,numord,iouf,&
     &numode,masgen,amrge,prchno)
            character(*) :: resu
            character(*) :: chs
            character(*) :: nocham
            character(*) :: ligrel
            character(*) :: tychas
            integer :: numord
            real(kind=8) :: iouf
            integer :: numode
            real(kind=8) :: masgen
            real(kind=8) :: amrge
            character(len=19) :: prchno
          end subroutine stock
        end interface
