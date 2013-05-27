        interface
          subroutine mltfld(n,front,adper,t1,ad,eps,ier)
            integer :: n
            real(kind=8) :: front(*)
            integer :: adper(*)
            real(kind=8) :: t1(*)
            integer :: ad(*)
            real(kind=8) :: eps
            integer :: ier
          end subroutine mltfld
        end interface
