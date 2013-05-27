        interface
          subroutine mltcld(n,front,adper,t1,ad,eps,ier)
            integer :: n
            complex(kind=8) :: front(*)
            integer :: adper(*)
            complex(kind=8) :: t1(*)
            integer :: ad(*)
            real(kind=8) :: eps
            integer :: ier
          end subroutine mltcld
        end interface
