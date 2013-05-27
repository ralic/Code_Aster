        interface
          subroutine mlnfld(n,frontl,frontu,adper,t1,t2,ad,eps,ier)
            integer :: n
            real(kind=8) :: frontl(*)
            real(kind=8) :: frontu(*)
            integer :: adper(*)
            real(kind=8) :: t1(*)
            real(kind=8) :: t2(*)
            integer :: ad(*)
            real(kind=8) :: eps
            integer :: ier
          end subroutine mlnfld
        end interface
