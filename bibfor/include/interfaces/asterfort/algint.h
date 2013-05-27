        interface
          subroutine algint(nbm,vitg,vitg0,depg,depg0,zin,trans,omegad&
     &,s0)
            integer :: nbm
            real(kind=8) :: vitg(*)
            real(kind=8) :: vitg0(*)
            real(kind=8) :: depg(*)
            real(kind=8) :: depg0(*)
            complex(kind=8) :: zin(*)
            real(kind=8) :: trans(2,2,*)
            real(kind=8) :: omegad(*)
            complex(kind=8) :: s0(*)
          end subroutine algint
        end interface
