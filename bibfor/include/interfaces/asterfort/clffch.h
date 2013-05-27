        interface
          subroutine clffch(alias,type,nno,xi,yi,zi,xin,yin,zin,tn,ajx&
     &,ajy,ajz,bjxx,bjyy,bjzz,bjxy,bjxz,bjyz,ider)
            character(len=6) :: alias
            character(len=6) :: type
            integer :: nno
            real(kind=8) :: xi
            real(kind=8) :: yi
            real(kind=8) :: zi
            real(kind=8) :: xin(1)
            real(kind=8) :: yin(1)
            real(kind=8) :: zin(1)
            real(kind=8) :: tn(1)
            real(kind=8) :: ajx(1)
            real(kind=8) :: ajy(1)
            real(kind=8) :: ajz(1)
            real(kind=8) :: bjxx(1)
            real(kind=8) :: bjyy(1)
            real(kind=8) :: bjzz(1)
            real(kind=8) :: bjxy(1)
            real(kind=8) :: bjxz(1)
            real(kind=8) :: bjyz(1)
            integer :: ider
          end subroutine clffch
        end interface
