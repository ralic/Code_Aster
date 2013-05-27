        interface
          subroutine vp1pro(optiom,lraide,lmasse,ldynam,neq,nfreq,&
     &nfreqb,tolv,nitv,iexcl,fcorig,vec,resufi,resufr,resufk,nbrssa,&
     &nbpari,nbparr,nbpark,typres,optiof,solveu)
            integer :: nbpark
            integer :: nbparr
            integer :: nbpari
            integer :: nfreqb
            integer :: neq
            character(*) :: optiom
            integer :: lraide
            integer :: lmasse
            integer :: ldynam
            integer :: nfreq
            real(kind=8) :: tolv
            integer :: nitv
            integer :: iexcl(*)
            real(kind=8) :: fcorig
            real(kind=8) :: vec(neq,*)
            integer :: resufi(nfreqb,nbpari)
            real(kind=8) :: resufr(nfreqb,nbparr)
            character(*) :: resufk(nfreqb,nbpark)
            integer :: nbrssa
            character(len=16) :: typres
            character(len=16) :: optiof
            character(len=19) :: solveu
          end subroutine vp1pro
        end interface
