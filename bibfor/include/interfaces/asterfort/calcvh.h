        interface
          subroutine calcvh(nbmat,materf,eta,vp,sigeqe,vh,vg)
            integer :: nbmat
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: eta
            real(kind=8) :: vp(3)
            real(kind=8) :: sigeqe
            real(kind=8) :: vh
            real(kind=8) :: vg
          end subroutine calcvh
        end interface
