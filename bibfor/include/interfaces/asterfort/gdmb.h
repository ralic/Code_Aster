        interface
          subroutine gdmb(ne,kp,ajacob,en,enprim,x0pg,b)
            integer :: ne
            integer :: kp
            real(kind=8) :: ajacob
            real(kind=8) :: en(3,2)
            real(kind=8) :: enprim(3,2)
            real(kind=8) :: x0pg(3)
            real(kind=8) :: b(6,6)
          end subroutine gdmb
        end interface
