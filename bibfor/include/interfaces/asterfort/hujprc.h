        interface
          subroutine hujprc(kk,k,tin,vin,mater,yf,p,q,toud)
            integer :: kk
            integer :: k
            real(kind=8) :: tin(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: mater(22,2)
            real(kind=8) :: yf(18)
            real(kind=8) :: p
            real(kind=8) :: q
            real(kind=8) :: toud(3)
          end subroutine hujprc
        end interface
