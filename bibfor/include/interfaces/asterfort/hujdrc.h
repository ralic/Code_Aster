        interface
          subroutine hujdrc(k,mater,sig,vin,pst)
            integer :: k
            real(kind=8) :: mater(22,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: pst
          end subroutine hujdrc
        end interface
