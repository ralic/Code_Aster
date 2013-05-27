        interface
          subroutine vpfopr(option,typres,lmasse,lraide,ldynam,omemin,&
     &omemax,omeshi,nbfreq,npivot,omecor,precsh,nbrssa,nblagr,solveu,det&
     &,idet)
            character(*) :: option
            character(len=16) :: typres
            integer :: lmasse
            integer :: lraide
            integer :: ldynam
            real(kind=8) :: omemin
            real(kind=8) :: omemax
            real(kind=8) :: omeshi
            integer :: nbfreq
            integer :: npivot(2)
            real(kind=8) :: omecor
            real(kind=8) :: precsh
            integer :: nbrssa
            integer :: nblagr
            character(len=19) :: solveu
            real(kind=8) :: det(2)
            integer :: idet(2)
          end subroutine vpfopr
        end interface
