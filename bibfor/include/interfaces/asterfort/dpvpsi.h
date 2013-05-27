        interface
          subroutine dpvpsi(nbmat,mater,se,seqe,i1e,fonecr,dp,sig)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: se(6)
            real(kind=8) :: seqe
            real(kind=8) :: i1e
            real(kind=8) :: fonecr(3)
            real(kind=8) :: dp
            real(kind=8) :: sig(6)
          end subroutine dpvpsi
        end interface
