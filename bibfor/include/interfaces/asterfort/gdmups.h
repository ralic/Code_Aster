        interface
          subroutine gdmups(ne,kp,ajacob,en,enprim,ups)
            integer :: ne
            integer :: kp
            real(kind=8) :: ajacob
            real(kind=8) :: en(3,2)
            real(kind=8) :: enprim(3,2)
            real(kind=8) :: ups(9,6)
          end subroutine gdmups
        end interface
