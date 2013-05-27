        interface
          subroutine cfpcdi(resoco,neq,nbliai,tole,epsipc,mu,apcoef,&
     &apddl,appoin,inliac,matass,solveu,premax,ssgrad,ssgrpr)
            character(len=24) :: resoco
            integer :: neq
            integer :: nbliai
            real(kind=8) :: tole
            real(kind=8) :: epsipc
            real(kind=8) :: mu(*)
            real(kind=8) :: apcoef(*)
            integer :: apddl(*)
            integer :: appoin(*)
            integer :: inliac(*)
            character(len=19) :: matass
            character(len=19) :: solveu
            integer :: premax
            real(kind=8) :: ssgrad(*)
            real(kind=8) :: ssgrpr(*)
          end subroutine cfpcdi
        end interface
