        interface
          subroutine crsvld(motfac,solveu,istop,nprec,syme,epsmat,&
     &mixpre,kmd)
            character(len=16) :: motfac
            character(len=19) :: solveu
            integer :: istop
            integer :: nprec
            character(len=3) :: syme
            real(kind=8) :: epsmat
            character(len=3) :: mixpre
            character(len=3) :: kmd
          end subroutine crsvld
        end interface
