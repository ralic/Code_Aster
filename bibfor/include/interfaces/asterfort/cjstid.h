        interface
          subroutine cjstid(mod,mater,nvi,eps,sig,vin,dsde)
            character(len=8) :: mod
            real(kind=8) :: mater(14,2)
            integer :: nvi
            real(kind=8) :: eps(6)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: dsde(6,6)
          end subroutine cjstid
        end interface
