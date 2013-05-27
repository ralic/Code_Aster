        interface
          subroutine projac(np1,np2,nbm,ic,phii,jacob,mtmp1,mtmp6)
            integer :: np2
            integer :: np1
            integer :: nbm
            integer :: ic
            real(kind=8) :: phii(np2,np1,*)
            real(kind=8) :: jacob(3,3)
            real(kind=8) :: mtmp1(np1,*)
            real(kind=8) :: mtmp6(3,*)
          end subroutine projac
        end interface
