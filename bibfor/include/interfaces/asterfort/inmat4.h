        interface
          subroutine inmat4(elrefa,nno,nnos,npg,nofpg,mgano)
            character(len=8) :: elrefa
            integer :: nno
            integer :: nnos
            integer :: npg
            character(len=8) :: nofpg
            real(kind=8) :: mgano(*)
          end subroutine inmat4
        end interface
