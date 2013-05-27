        interface
          subroutine ccl21j(fronti,frontj,frn,j,l,n,n1,t1,t2)
            integer :: n
            complex(kind=8) :: fronti(*)
            complex(kind=8) :: frontj(*)
            complex(kind=8) :: frn(*)
            integer :: j
            integer :: l
            integer :: n1
            complex(kind=8) :: t1(n)
            complex(kind=8) :: t2(n)
          end subroutine ccl21j
        end interface
