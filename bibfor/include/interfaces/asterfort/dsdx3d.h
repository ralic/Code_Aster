        interface
          subroutine dsdx3d(loop,b,u,deps,d,nbn)
            integer :: nbn
            integer :: loop
            real(kind=8) :: b(3,nbn)
            real(kind=8) :: u(3,nbn)
            real(kind=8) :: deps(6)
            real(kind=8) :: d(9)
          end subroutine dsdx3d
        end interface
