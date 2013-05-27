        interface
          subroutine vppara(modes,typcon,knega,lraide,lmasse,lamor,&
     &mxresf,neq,nfreq,omecor,dlagr,dbloq,vectr,vectc,nbpari,nbparr,&
     &nbpark,nopara,mod45,resui,resur,resuk,ktyp,lcomod,icom1,icom2,&
     &typres,nfreqg)
            character(len=8) :: modes
            character(len=16) :: typcon
            character(len=8) :: knega
            integer :: lraide
            integer :: lmasse
            integer :: lamor
            integer :: mxresf
            integer :: neq
            integer :: nfreq
            real(kind=8) :: omecor
            integer :: dlagr(*)
            integer :: dbloq(*)
            real(kind=8) :: vectr(*)
            complex(kind=8) :: vectc(*)
            integer :: nbpari
            integer :: nbparr
            integer :: nbpark
            character(*) :: nopara(*)
            character(len=4) :: mod45
            integer :: resui(*)
            real(kind=8) :: resur(*)
            character(*) :: resuk(*)
            character(len=1) :: ktyp
            logical :: lcomod
            integer :: icom1
            integer :: icom2
            character(len=16) :: typres
            integer :: nfreqg
          end subroutine vppara
        end interface
