        interface
          subroutine majsig(materf,se,seq,i1e,alpha,dp,plas,sig)
            real(kind=8) :: materf(5,2)
            real(kind=8) :: se(*)
            real(kind=8) :: seq
            real(kind=8) :: i1e
            real(kind=8) :: alpha
            real(kind=8) :: dp
            real(kind=8) :: plas
            real(kind=8) :: sig(6)
          end subroutine majsig
        end interface
