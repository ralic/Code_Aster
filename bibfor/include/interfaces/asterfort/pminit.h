        interface
          subroutine pminit(imate,nbvari,ndim,typmod,table,nbpar,&
     &iforta,nompar,typpar,ang,pgl,irota,epsm,sigm,vim,vip,vr,defimp,&
     &coef,indimp,fonimp,cimpo,kel,sddisc,parcri,pred,matrel,imptgt,&
     &option,nomvi,nbvita,nbvrcm,sderro)
            integer :: nbvari
            integer :: imate
            integer :: ndim
            character(len=8) :: typmod(2)
            character(len=8) :: table
            integer :: nbpar
            integer :: iforta
            character(len=16) :: nompar(*)
            character(len=8) :: typpar(*)
            real(kind=8) :: ang(7)
            real(kind=8) :: pgl(3,3)
            integer :: irota
            real(kind=8) :: epsm(9)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(nbvari)
            real(kind=8) :: vip(nbvari)
            real(kind=8) :: vr(*)
            integer :: defimp
            real(kind=8) :: coef
            integer :: indimp(9)
            character(len=8) :: fonimp(9)
            real(kind=8) :: cimpo(6,12)
            real(kind=8) :: kel(6,6)
            character(len=19) :: sddisc
            real(kind=8) :: parcri(*)
            integer :: pred
            integer :: matrel
            integer :: imptgt
            character(len=16) :: option
            character(len=8) :: nomvi(*)
            integer :: nbvita
            integer :: nbvrcm
            character(len=24) :: sderro
          end subroutine pminit
        end interface
