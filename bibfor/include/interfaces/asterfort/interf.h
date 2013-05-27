        interface
          subroutine interf(mater,kfonc1,kfonc2,normf,x0,xrac)
            character(len=8) :: mater
            character(len=8) :: kfonc1
            character(len=8) :: kfonc2
            real(kind=8) :: normf
            real(kind=8) :: x0
            real(kind=8) :: xrac
          end subroutine interf
        end interface
