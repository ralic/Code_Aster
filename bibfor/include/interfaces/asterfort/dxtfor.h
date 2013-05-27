        interface
          subroutine dxtfor(global,xyzl,pgl,for,vecl)
            logical :: global
            real(kind=8) :: xyzl(3,*)
            real(kind=8) :: pgl(3,*)
            real(kind=8) :: for(6,*)
            real(kind=8) :: vecl(*)
          end subroutine dxtfor
        end interface
