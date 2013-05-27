        interface
          subroutine wp1dft(lmat,imode,zeropo,z,detnor,det,idet,isturm&
     &)
            integer :: lmat
            integer :: imode
            complex(kind=8) :: zeropo(*)
            complex(kind=8) :: z
            complex(kind=8) :: detnor
            real(kind=8) :: det
            integer :: idet
            integer :: isturm
          end subroutine wp1dft
        end interface
