        interface
          subroutine nmvecd(imate,mate,nmat,matcst,loi,hook,dt,tp,p,np&
     &,beta,nb,ep,rm,dm,dsgde,dsgdb,dsgdp,drbde,drpde,rb,rp,drbdb,drbdp,&
     &drpdb,drpdp,etatf,ier)
            integer :: nb
            integer :: np
            integer :: nmat
            integer :: imate
            real(kind=8) :: mate(nmat,2)
            character(len=3) :: matcst
            character(len=16) :: loi
            real(kind=8) :: hook(6,6)
            real(kind=8) :: dt
            real(kind=8) :: tp
            real(kind=8) :: p(np)
            real(kind=8) :: beta(nb)
            real(kind=8) :: ep(*)
            real(kind=8) :: rm
            real(kind=8) :: dm
            real(kind=8) :: dsgde(nb,nb)
            real(kind=8) :: dsgdb(nb,nb)
            real(kind=8) :: dsgdp(nb,np)
            real(kind=8) :: drbde(nb,nb)
            real(kind=8) :: drpde(np,nb)
            real(kind=8) :: rb(nb)
            real(kind=8) :: rp(np)
            real(kind=8) :: drbdb(nb,nb)
            real(kind=8) :: drbdp(nb,np)
            real(kind=8) :: drpdb(np,nb)
            real(kind=8) :: drpdp(np,np)
            character(len=7) :: etatf(3)
            integer :: ier
          end subroutine nmvecd
        end interface
