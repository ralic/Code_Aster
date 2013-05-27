        interface
          subroutine vpinte(option,nfreq,valp,det,idet,ieme,npas,tolf,&
     &nitf,lraide,lmasse,ldynam,resufi,resufr,nfreqb,solveu)
            integer :: nfreqb
            character(len=16) :: option
            integer :: nfreq
            real(kind=8) :: valp(*)
            real(kind=8) :: det(*)
            integer :: idet(*)
            integer :: ieme(*)
            integer :: npas(*)
            real(kind=8) :: tolf
            integer :: nitf
            integer :: lraide
            integer :: lmasse
            integer :: ldynam
            integer :: resufi(nfreqb,*)
            real(kind=8) :: resufr(nfreqb,*)
            character(len=19) :: solveu
          end subroutine vpinte
        end interface
