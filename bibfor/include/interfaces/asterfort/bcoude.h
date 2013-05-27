        interface
          subroutine bcoude(igau,icou,isect,l,h,a,m,nno,ncou,nsect,ff,&
     &df1,df2,mmt,b)
            integer :: igau
            integer :: icou
            integer :: isect
            real(kind=8) :: l
            real(kind=8) :: h
            real(kind=8) :: a
            integer :: m
            integer :: nno
            integer :: ncou
            integer :: nsect
            real(kind=8) :: ff(*)
            real(kind=8) :: df1(*)
            real(kind=8) :: df2(*)
            integer :: mmt
            real(kind=8) :: b(4,*)
          end subroutine bcoude
        end interface
