        interface
          subroutine bcoudc(igau,icou,isect,h,a,m,omega,xpg,nno,ncou,&
     &nsect,ff,df1,df2,rayon,theta,mmt,b)
            integer :: igau
            integer :: icou
            integer :: isect
            real(kind=8) :: h
            real(kind=8) :: a
            integer :: m
            real(kind=8) :: omega
            real(kind=8) :: xpg(4)
            integer :: nno
            integer :: ncou
            integer :: nsect
            real(kind=8) :: ff(*)
            real(kind=8) :: df1(*)
            real(kind=8) :: df2(*)
            real(kind=8) :: rayon
            real(kind=8) :: theta
            integer :: mmt
            real(kind=8) :: b(4,*)
          end subroutine bcoudc
        end interface
