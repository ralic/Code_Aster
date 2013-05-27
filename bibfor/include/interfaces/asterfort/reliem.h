        interface
          subroutine reliem(mo,ma,typem,motfaz,iocc,nbmocl,limocl,&
     &tymocl,litroz,nbtrou)
            integer :: nbmocl
            character(*) :: mo
            character(len=8) :: ma
            character(*) :: typem
            character(*) :: motfaz
            integer :: iocc
            character(*) :: limocl(nbmocl)
            character(*) :: tymocl(nbmocl)
            character(*) :: litroz
            integer :: nbtrou
          end subroutine reliem
        end interface
