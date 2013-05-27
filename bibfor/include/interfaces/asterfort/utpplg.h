        interface
          subroutine utpplg(nn,nc,p,sl,sg)
            integer :: nn
            integer :: nc
            real(kind=8) :: p(3,3)
            real(kind=8) :: sl(*)
            real(kind=8) :: sg(*)
          end subroutine utpplg
        end interface
