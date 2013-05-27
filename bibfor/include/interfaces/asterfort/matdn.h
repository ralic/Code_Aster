        interface
          subroutine matdn(nb1,xr,intsn,madn,nks1,nks2)
            integer :: nb1
            real(kind=8) :: xr(*)
            integer :: intsn
            real(kind=8) :: madn(3,51)
            real(kind=8) :: nks1(3,51)
            real(kind=8) :: nks2(3,51)
          end subroutine matdn
        end interface
