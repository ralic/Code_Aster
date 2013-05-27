        interface
          subroutine hujddd(carac,k,mater,ind,yf,vin,vec,mat,iret)
            character(len=6) :: carac
            integer :: k
            real(kind=8) :: mater(22,2)
            integer :: ind(7)
            real(kind=8) :: yf(18)
            real(kind=8) :: vin(*)
            real(kind=8) :: vec(6)
            real(kind=8) :: mat(6,6)
            integer :: iret
          end subroutine hujddd
        end interface
