        interface
          subroutine mlnfmj(nb,n,p,frontl,frontu,frnl,frnu,adper,t1,t2&
     &,cl,cu)
            integer :: p
            integer :: nb
            integer :: n
            real(kind=8) :: frontl(*)
            real(kind=8) :: frontu(*)
            real(kind=8) :: frnl(*)
            real(kind=8) :: frnu(*)
            integer :: adper(*)
            real(kind=8) :: t1(p,nb,*)
            real(kind=8) :: t2(p,nb,*)
            real(kind=8) :: cl(nb,nb,*)
            real(kind=8) :: cu(nb,nb,*)
          end subroutine mlnfmj
        end interface
