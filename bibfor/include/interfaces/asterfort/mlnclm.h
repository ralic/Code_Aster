        interface
          subroutine mlnclm(nb,n,p,frontl,frontu,adper,tu,tl,ad,eps,&
     &ier,cl,cu)
            integer :: nb
            integer :: n
            integer :: p
            complex(kind=8) :: frontl(*)
            complex(kind=8) :: frontu(*)
            integer :: adper(*)
            complex(kind=8) :: tu(*)
            complex(kind=8) :: tl(*)
            integer :: ad(*)
            real(kind=8) :: eps
            integer :: ier
            complex(kind=8) :: cl(nb,nb,*)
            complex(kind=8) :: cu(nb,nb,*)
          end subroutine mlnclm
        end interface
