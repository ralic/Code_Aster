        interface
          subroutine dikini(nbt,nu1,mu1,dxu1,dryu1,nu2,mu2,dxu2,dryu2,&
     &ky,kz,krx,krz,k01,k02,rbid)
            integer :: nbt
            real(kind=8) :: nu1
            real(kind=8) :: mu1
            real(kind=8) :: dxu1
            real(kind=8) :: dryu1
            real(kind=8) :: nu2
            real(kind=8) :: mu2
            real(kind=8) :: dxu2
            real(kind=8) :: dryu2
            real(kind=8) :: ky
            real(kind=8) :: kz
            real(kind=8) :: krx
            real(kind=8) :: krz
            real(kind=8) :: k01(nbt)
            real(kind=8) :: k02(nbt)
            real(kind=8) :: rbid
          end subroutine dikini
        end interface
