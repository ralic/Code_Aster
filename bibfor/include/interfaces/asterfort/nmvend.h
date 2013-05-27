        interface
          subroutine nmvend(fami,kpg,ksp,materd,materf,nmat,dt1,epsm,&
     &deps,sigm,vim,ndim,crit,dammax,etatf,p,np,beta,nb,iter,ier)
            integer :: nb
            integer :: np
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: dt1
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(*)
            integer :: ndim
            real(kind=8) :: crit(*)
            real(kind=8) :: dammax
            character(len=7) :: etatf(3)
            real(kind=8) :: p(np)
            real(kind=8) :: beta(nb)
            integer :: iter
            integer :: ier
          end subroutine nmvend
        end interface
