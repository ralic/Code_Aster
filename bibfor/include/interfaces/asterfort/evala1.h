        interface
          subroutine evala1(mod,relcom,sig,vin,imat,module,icode)
            character(len=8) :: mod
            character(len=16) :: relcom
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(50)
            integer :: imat
            real(kind=8) :: module
            integer :: icode
          end subroutine evala1
        end interface
