        interface
          subroutine calfig(guidag,resu,dimobs,dimtub,obsuse,tubuse)
            character(len=8) :: guidag
            character(len=19) :: resu
            integer :: dimobs
            integer :: dimtub
            real(kind=8) :: obsuse(*)
            real(kind=8) :: tubuse(*)
          end subroutine calfig
        end interface
