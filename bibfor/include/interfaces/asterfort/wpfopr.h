        interface
          subroutine wpfopr(lmasse,lamor,lraide,appr,fmin,sigma,matopa&
     &,matpsc,raide,lqz,solveu)
            integer :: lmasse
            integer :: lamor
            integer :: lraide
            character(*) :: appr
            real(kind=8) :: fmin
            complex(kind=8) :: sigma
            character(*) :: matopa
            character(*) :: matpsc
            character(*) :: raide
            logical :: lqz
            character(len=19) :: solveu
          end subroutine wpfopr
        end interface
