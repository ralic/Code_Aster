        interface
          subroutine divgra(e1,e2,dfde,dfdk,vibarn,divsig)
            real(kind=8) :: e1(3,9)
            real(kind=8) :: e2(3,9)
            real(kind=8) :: dfde(9,9)
            real(kind=8) :: dfdk(9,9)
            real(kind=8) :: vibarn(2,9)
            real(kind=8) :: divsig(9)
          end subroutine divgra
        end interface
