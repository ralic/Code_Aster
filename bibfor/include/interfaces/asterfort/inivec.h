        interface
          subroutine inivec(vec,neq,id,nbcp)
            integer :: nbcp
            integer :: neq
            real(kind=8) :: vec(neq)
            integer :: id(nbcp)
          end subroutine inivec
        end interface
