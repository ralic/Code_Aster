        interface
          subroutine parmat(nbm,dt,amor,puls,pulsd,s0,z0,sr0,za1,za2,&
     &za3)
            integer :: nbm
            real(kind=8) :: dt
            real(kind=8) :: amor(*)
            real(kind=8) :: puls(*)
            real(kind=8) :: pulsd(*)
            complex(kind=8) :: s0(*)
            complex(kind=8) :: z0(*)
            complex(kind=8) :: sr0(*)
            complex(kind=8) :: za1(*)
            complex(kind=8) :: za2(*)
            complex(kind=8) :: za3(*)
          end subroutine parmat
        end interface
