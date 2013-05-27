        interface
          subroutine projvd(testc,np1,nb1,nb2,mat,u,v)
            integer :: np1
            integer :: testc
            integer :: nb1
            integer :: nb2
            real(kind=8) :: mat(np1,*)
            real(kind=8) :: u(*)
            real(kind=8) :: v(*)
          end subroutine projvd
        end interface
