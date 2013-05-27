        interface
          subroutine wp2ayl(appr,lmatra,lmasse,lamor,sigma,lbloq,yh,yb&
     &,zh,zb,u1,u2,u3,u4,v,n,solveu)
            character(len=1) :: appr
            integer :: lmatra
            integer :: lmasse
            integer :: lamor
            complex(kind=8) :: sigma
            integer :: lbloq(*)
            real(kind=8) :: yh(*)
            real(kind=8) :: yb(*)
            real(kind=8) :: zh(*)
            real(kind=8) :: zb(*)
            real(kind=8) :: u1(*)
            real(kind=8) :: u2(*)
            real(kind=8) :: u3(*)
            real(kind=8) :: u4(*)
            complex(kind=8) :: v(*)
            integer :: n
            character(len=19) :: solveu
          end subroutine wp2ayl
        end interface
