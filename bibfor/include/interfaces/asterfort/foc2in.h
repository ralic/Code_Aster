        interface
          subroutine foc2in(method,nbpts,var,fon,cste,res)
            character(*) :: method
            integer :: nbpts
            real(kind=8) :: var(*)
            real(kind=8) :: fon(*)
            real(kind=8) :: cste
            real(kind=8) :: res(*)
          end subroutine foc2in
        end interface
