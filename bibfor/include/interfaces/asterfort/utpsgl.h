        interface
          subroutine utpsgl(nn,nc,p,sg,sl)
            integer :: nn
            integer :: nc
            real(kind=8) :: p(3,3)
            real(kind=8) :: sg(*)
            real(kind=8) :: sl(*)
          end subroutine utpsgl
        end interface
