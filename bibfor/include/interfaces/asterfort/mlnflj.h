        interface
          subroutine mlnflj(nb,n,ll,m,it,p,frontl,frontu,frnl,frnu,&
     &adper,travl,travu,cl,cu)
            integer :: p
            integer :: nb
            integer :: n
            integer :: ll
            integer :: m
            integer :: it
            real(kind=8) :: frontl(*)
            real(kind=8) :: frontu(*)
            real(kind=8) :: frnl(*)
            real(kind=8) :: frnu(*)
            integer :: adper(*)
            real(kind=8) :: travl(p,nb,*)
            real(kind=8) :: travu(p,nb,*)
            real(kind=8) :: cl(nb,nb,*)
            real(kind=8) :: cu(nb,nb,*)
          end subroutine mlnflj
        end interface
