        interface
          subroutine nuainr(method,np1,nx1,nc1,ic1,nuax1,nual1,nuav1,&
     &x2,dref,val2)
            integer :: nx1
            character(*) :: method
            integer :: np1
            integer :: nc1
            integer :: ic1
            real(kind=8) :: nuax1(*)
            logical :: nual1(*)
            real(kind=8) :: nuav1(*)
            real(kind=8) :: x2(nx1)
            real(kind=8) :: dref
            real(kind=8) :: val2
          end subroutine nuainr
        end interface
