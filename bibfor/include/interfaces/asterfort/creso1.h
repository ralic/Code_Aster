        interface
          subroutine creso1(solveu,method,preco,renum,syme,sdfeti,eps,&
     &resire,tbloc,nprec,nmaxit,istop,niremp,ifm,numsd,nbma,verif,testco&
     &,nbreor,tyreor,scalin,inumsd,imail,infofe,stogi,testok,nbreoi,acma&
     &,acsm,reacre)
            character(len=19) :: solveu
            character(len=16) :: method
            character(len=8) :: preco
            character(len=8) :: renum
            character(len=3) :: syme
            character(len=24) :: sdfeti
            real(kind=8) :: eps
            real(kind=8) :: resire
            real(kind=8) :: tbloc
            integer :: nprec
            integer :: nmaxit
            integer :: istop
            integer :: niremp
            integer :: ifm
            integer :: numsd
            integer :: nbma
            character(len=8) :: verif
            real(kind=8) :: testco
            integer :: nbreor
            character(len=8) :: tyreor
            character(len=8) :: scalin
            integer :: inumsd
            integer :: imail
            character(len=24) :: infofe
            character(len=8) :: stogi
            logical :: testok
            integer :: nbreoi
            character(len=8) :: acma
            character(len=8) :: acsm
            integer :: reacre
          end subroutine creso1
        end interface
