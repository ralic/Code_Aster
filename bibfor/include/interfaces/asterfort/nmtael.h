        interface
          subroutine nmtael(fami,kpg,ksp,imate,ndimsi,matm,mat,sigm,&
     &epsm,deps,epm,sigdv,sigp)
            integer :: ndimsi
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            real(kind=8) :: matm(3)
            real(kind=8) :: mat(3)
            real(kind=8) :: sigm(ndimsi)
            real(kind=8) :: epsm(ndimsi)
            real(kind=8) :: deps(ndimsi)
            real(kind=8) :: epm(ndimsi)
            real(kind=8) :: sigdv(ndimsi)
            real(kind=8) :: sigp(ndimsi)
          end subroutine nmtael
        end interface
