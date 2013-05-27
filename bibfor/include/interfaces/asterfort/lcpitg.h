        interface
          subroutine lcpitg(compor,df,line,dp,dvbe,dtaudf)
            character(len=16) :: compor
            real(kind=8) :: df(3,3)
            integer :: line
            real(kind=8) :: dp
            real(kind=8) :: dvbe(6)
            real(kind=8) :: dtaudf(6,3,3)
          end subroutine lcpitg
        end interface
