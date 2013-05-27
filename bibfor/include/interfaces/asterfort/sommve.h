        interface
          subroutine sommve(np,vec1,n1,vec2,n2,vecres)
            integer :: np
            real(kind=8) :: vec1(*)
            integer :: n1
            real(kind=8) :: vec2(*)
            integer :: n2
            real(kind=8) :: vecres(*)
          end subroutine sommve
        end interface
