        interface
          subroutine dxktan(delas,mp1,mp2,nbackn,ncrit,dcc1,dcc2,&
     &dsidep)
            real(kind=8) :: delas(6,6)
            real(kind=8) :: mp1(3)
            real(kind=8) :: mp2(3)
            real(kind=8) :: nbackn(6)
            integer :: ncrit
            real(kind=8) :: dcc1(3,3)
            real(kind=8) :: dcc2(3,3)
            real(kind=8) :: dsidep(6,6)
          end subroutine dxktan
        end interface
