        interface
          subroutine trndgl(nbx,vectn,vectpt,deplg,depll,rotfic)
            integer :: nbx
            real(kind=8) :: vectn(9,3)
            real(kind=8) :: vectpt(9,2,3)
            real(kind=8) :: deplg(*)
            real(kind=8) :: depll(*)
            real(kind=8) :: rotfic(*)
          end subroutine trndgl
        end interface
