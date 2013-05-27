        interface
          subroutine hbmajs(dg,nbmat,materf,se,i1e,sigeqe,etap,sigp)
            integer :: nbmat
            real(kind=8) :: dg
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: se(6)
            real(kind=8) :: i1e
            real(kind=8) :: sigeqe
            real(kind=8) :: etap
            real(kind=8) :: sigp(6)
          end subroutine hbmajs
        end interface
