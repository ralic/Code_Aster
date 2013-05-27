subroutine lc0082(fami,kpg,ksp,ndim,imate,compor,crit,instam,&
     &                instap,epsm,deps,sigm,vim,option,angmas,sigp,vip,&
     &                  tampon,typmod,icomp,nvi,dsidep,codret)
! AJOUT FERMETUR
! TOLE CRP_21
    implicit none
    integer :: imate,ndim,kpg,ksp,codret,icomp,nvi
    real(kind=8)           :: crit(*), angmas(3)
    real(kind=8)           :: instam,instap,tampon(*)
    real(kind=8)           :: epsm(6),deps(6)
    real(kind=8)           :: sigm(6),sigp(6)
    real(kind=8)           :: vim(*),vip(*)
    real(kind=8)           :: dsidep(6,6)
    character(len=16)     :: compor(*),option
    character(len=8)      :: typmod(*)
    character(len=*)    :: fami
    call u2mess('F','FERMETUR_11')
end subroutine
