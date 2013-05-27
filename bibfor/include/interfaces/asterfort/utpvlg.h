        interface
          subroutine utpvlg(nn,nc,p,vl,vg)
            integer :: nn
            integer :: nc
            real(kind=8) :: p(3,3)
            real(kind=8) :: vl(*)
            real(kind=8) :: vg(*)
          end subroutine utpvlg
        end interface
