        interface
          subroutine mlnflm(nb,n,p,frontl,frontu,adper,tu,tl,ad,eps,&
     &ier,cl,cu)
            integer :: nb
            integer :: n
            integer :: p
            real(kind=8) :: frontl(*)
            real(kind=8) :: frontu(*)
            integer :: adper(*)
            real(kind=8) :: tu(*)
            real(kind=8) :: tl(*)
            integer :: ad(*)
            real(kind=8) :: eps
            integer :: ier
            real(kind=8) :: cl(nb,nb,*)
            real(kind=8) :: cu(nb,nb,*)
          end subroutine mlnflm
        end interface
