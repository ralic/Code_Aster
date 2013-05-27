        interface
          subroutine parmtr(np4,nfour,nbm,ttrans,amor,puls,pulsd,s0,z0&
     &,omegaf,za4,za5)
            integer :: np4
            integer :: nfour
            integer :: nbm
            real(kind=8) :: ttrans
            real(kind=8) :: amor(*)
            real(kind=8) :: puls(*)
            real(kind=8) :: pulsd(*)
            complex(kind=8) :: s0(*)
            complex(kind=8) :: z0(*)
            real(kind=8) :: omegaf(*)
            complex(kind=8) :: za4(np4,*)
            complex(kind=8) :: za5(np4,*)
          end subroutine parmtr
        end interface
