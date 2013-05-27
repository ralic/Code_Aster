        interface
          subroutine wp2ayc(lmatra,lmasse,lamor,sigma,lbloq,yh,yb,zh,&
     &zb,u1,u2,u3,n,solveu)
            integer :: lmatra
            integer :: lmasse
            integer :: lamor
            complex(kind=8) :: sigma
            integer :: lbloq(*)
            complex(kind=8) :: yh(*)
            complex(kind=8) :: yb(*)
            complex(kind=8) :: zh(*)
            complex(kind=8) :: zb(*)
            complex(kind=8) :: u1(*)
            complex(kind=8) :: u2(*)
            complex(kind=8) :: u3(*)
            integer :: n
            character(len=19) :: solveu
          end subroutine wp2ayc
        end interface
