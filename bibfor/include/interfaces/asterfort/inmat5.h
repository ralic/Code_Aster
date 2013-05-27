        interface
          subroutine inmat5(elrefa,nno,nnos,npg,mganos,mgano2)
            character(len=8) :: elrefa
            integer :: nno
            integer :: nnos
            integer :: npg
            real(kind=8) :: mganos(1000,27)
            real(kind=8) :: mgano2(1000,27)
          end subroutine inmat5
        end interface
