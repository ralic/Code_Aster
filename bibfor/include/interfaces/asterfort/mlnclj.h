        interface
          subroutine mlnclj(nb,n,ll,m,it,p,frontl,frontu,frnl,frnu,&
     &adper,travl,travu,cl,cu)
            integer :: p
            integer :: nb
            integer :: n
            integer :: ll
            integer :: m
            integer :: it
            complex(kind=8) :: frontl(*)
            complex(kind=8) :: frontu(*)
            complex(kind=8) :: frnl(*)
            complex(kind=8) :: frnu(*)
            integer :: adper(*)
            complex(kind=8) :: travl(p,nb,*)
            complex(kind=8) :: travu(p,nb,*)
            complex(kind=8) :: cl(nb,nb,*)
            complex(kind=8) :: cu(nb,nb,*)
          end subroutine mlnclj
        end interface
