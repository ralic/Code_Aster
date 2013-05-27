        interface
          subroutine mltflm(nb,n,p,front,adper,t1,ad,eps,ier,c)
            integer :: nb
            integer :: n
            integer :: p
            real(kind=8) :: front(*)
            integer :: adper(*)
            real(kind=8) :: t1(*)
            integer :: ad(*)
            real(kind=8) :: eps
            integer :: ier
            real(kind=8) :: c(nb,nb,*)
          end subroutine mltflm
        end interface
