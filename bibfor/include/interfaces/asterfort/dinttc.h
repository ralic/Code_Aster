        interface
          subroutine dinttc(coord1,coord2,xo1o2,yo1o2,zo1o2,do1o2,r,&
     &norm,nint,nhop,npir,coord,nbi)
            real(kind=8) :: coord1(3)
            real(kind=8) :: coord2(3)
            real(kind=8) :: xo1o2
            real(kind=8) :: yo1o2
            real(kind=8) :: zo1o2
            real(kind=8) :: do1o2
            real(kind=8) :: r
            integer :: norm(2,4)
            integer :: nint
            integer :: nhop
            integer :: npir
            real(kind=8) :: coord(3,12)
            integer :: nbi
          end subroutine dinttc
        end interface
