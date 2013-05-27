        interface
          subroutine edgmat(fami,kpg,ksp,imat,c1,zalpha,temp,dt,mum,mu&
     &,troikm,troisk,alpham,alphap,ani,m,n,gamma)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imat
            character(len=1) :: c1
            real(kind=8) :: zalpha
            real(kind=8) :: temp
            real(kind=8) :: dt
            real(kind=8) :: mum
            real(kind=8) :: mu
            real(kind=8) :: troikm
            real(kind=8) :: troisk
            real(kind=8) :: alpham
            real(kind=8) :: alphap
            real(kind=8) :: ani(6,6)
            real(kind=8) :: m(3)
            real(kind=8) :: n(3)
            real(kind=8) :: gamma(3)
          end subroutine edgmat
        end interface
