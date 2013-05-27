        interface
          subroutine utpvgl(nn,nc,p,vg,vl)
            integer :: nn
            integer :: nc
            real(kind=8) :: p(3,3)
            real(kind=8) :: vg(*)
            real(kind=8) :: vl(*)
          end subroutine utpvgl
        end interface
