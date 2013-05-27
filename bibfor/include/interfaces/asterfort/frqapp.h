        interface
          subroutine frqapp(dt,neq,dep1,dep2,acc1,acc2,vmin,freq)
            real(kind=8) :: dt
            integer :: neq
            real(kind=8) :: dep1(*)
            real(kind=8) :: dep2(*)
            real(kind=8) :: acc1(*)
            real(kind=8) :: acc2(*)
            real(kind=8) :: vmin(*)
            real(kind=8) :: freq
          end subroutine frqapp
        end interface
