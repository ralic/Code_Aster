        interface
          subroutine matran(nbm,s0,z0,omega,omegad,trans)
            integer :: nbm
            complex(kind=8) :: s0(*)
            complex(kind=8) :: z0(*)
            real(kind=8) :: omega(*)
            real(kind=8) :: omegad(*)
            real(kind=8) :: trans(2,2,*)
          end subroutine matran
        end interface
