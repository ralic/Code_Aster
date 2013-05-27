        interface
          subroutine hbderi(gamma,nbmat,materf,vg,eta,param2,parame)
            integer :: nbmat
            real(kind=8) :: gamma
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: vg
            real(kind=8) :: eta
            real(kind=8) :: param2(4)
            real(kind=8) :: parame(5)
          end subroutine hbderi
        end interface
