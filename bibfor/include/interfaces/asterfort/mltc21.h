        interface
          subroutine mltc21(p,front,frn,n,t1,t2,eps,ier)
            integer :: p
            complex(kind=8) :: front(*)
            complex(kind=8) :: frn(*)
            integer :: n
            complex(kind=8) :: t1(*)
            complex(kind=8) :: t2(*)
            real(kind=8) :: eps
            integer :: ier
          end subroutine mltc21
        end interface
