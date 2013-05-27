        interface
          subroutine mlncld(n,frontl,frontu,adper,t1,t2,ad,eps,ier)
            integer :: n
            complex(kind=8) :: frontl(*)
            complex(kind=8) :: frontu(*)
            integer :: adper(*)
            complex(kind=8) :: t1(*)
            complex(kind=8) :: t2(*)
            integer :: ad(*)
            real(kind=8) :: eps
            integer :: ier
          end subroutine mlncld
        end interface
