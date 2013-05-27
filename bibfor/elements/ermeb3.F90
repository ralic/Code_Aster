subroutine ermeb3(noe, ifa, tymvol, nnof, iref1,&
                  iref2, ivois, igeom, isig, nbcmp,&
                  inst, nx, ny, nz, sig11,&
                  sig22, sig33, sig12, sig13, sig23,&
                  chx, chy, chz)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: josselin.delmas at edf.fr
! TOLE CRP_21
! =====================================================================
!  ERREUR EN MECANIQUE - TERME DE BORD - DIMENSION 3
!  **        **                   *                *
! =====================================================================
!
!     BUT:
!         TROISIEME TERME DE L'ESTIMATEUR D'ERREUR EN RESIDU EXPLICITE :
!         CALCUL DE LA DIFFERENCE ENTRE LES EFFORTS APPLIQUES SUR LE
!         BORD ET LA CONTRAINTE NORMALE EN 3D.
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NOE    : LISTE DES NUMEROS DES NOEUDS PAR FACE (VOIR TE0003)
! IN   IFA    : NUMERO LOCAL DE LA FACE
! IN   TYMVOL : NUMERO DU TYPE DE LA MAILLE VOLUMIQUE COURANTE
!               1 : HEXAEDRE; 2 : PENTAEDRE; 3 : TETRAEDRE; 4 : PYRAMIDE
! IN   NNOF   : NOMBRE DE NOEUDS DE LA FACE
! IN   IREF1  : ADRESSE DES CHARGEMENTS DE TYPE FORCE
! IN   IREF2  : ADRESSE DES CHARGEMENTS DE TYPE PRESSION
! IN   IVOIS  : ADRESSE DES VOISINS
! IN   IGEOM  : ADRESSE DE LA GEOMETRIE
! IN   ISIG   : ADRESSE DES CONTRAINTES AUX NOEUDS
! IN   NBCMP  : NOMBRE DE COMPOSANTES DU VECTEUR CONTRAINTE PAR NOEUD
! IN   INST   : INSTANT DE CALCUL
! IN   NX     : VECTEUR DES ABSCISSES DES NORMALES AUX NOEUDS
! IN   NY     : VECTEUR DES ORDONNEES DES NORMALES AUX NOEUDS
! IN   NX     : VECTEUR DES COTES DES NORMALES AUX NOEUDS
!
!      SORTIE :
!-------------
! OUT  SIG11  : VECTEUR DES CONTRAINTES AUX NOEUDS - COMPOSANTE SIG11
! OUT  SIG22  : VECTEUR DES CONTRAINTES AUX NOEUDS - COMPOSANTE SIG22
! OUT  SIG33  : VECTEUR DES CONTRAINTES AUX NOEUDS - COMPOSANTE SIG33
! OUT  SIG12  : VECTEUR DES CONTRAINTES AUX NOEUDS - COMPOSANTE SIG12
! OUT  SIG13  : VECTEUR DES CONTRAINTES AUX NOEUDS - COMPOSANTE SIG13
! OUT  SIG23  : VECTEUR DES CONTRAINTES AUX NOEUDS - COMPOSANTE SIG23
! OUT  CHX    : VECTEUR DES CHARGEMENTS AUX NOEUDS SELON X
! OUT  CHY    : VECTEUR DES CHARGEMENTS AUX NOEUDS SELON Y
! OUT  CHZ    : VECTEUR DES CHARGEMENTS AUX NOEUDS SELON Z
!
! ......................................................................
!
    implicit none
!
! DECLARATION PARAMETRES D'APPEL
    include 'jeveux.h'
    include 'asterfort/fointe.h'
    integer :: noe(9, 6, 4), ifa, tymvol, iref1, iref2, ivois, igeom, isig
    integer :: nbcmp, nnof
    real(kind=8) :: inst
    real(kind=8) :: nx(9), ny(9), nz(9)
    real(kind=8) :: chx(9), chy(9), chz(9)
    real(kind=8) :: sig11(9), sig22(9), sig12(9), sig33(9), sig13(9), sig23(9)
!
!
!
!
! DECLARATION VARIABLES LOCALES
!
    integer :: iaux
    integer :: in
    integer :: ier, ier1, ier2, ier3
!
!     IMAV : NUMERO DE LA MAILLE SURFACIQUE A TRAITER
    integer :: ino, imav
    integer :: iagd, iacmp
    integer :: iade1, iava1, iaptm1, igd1, ient1, ncmpm1, numgd1
    integer :: iade2, iava2, iaptm2, igd2, ient2, ncmpm2, numgd2
!
    real(kind=8) :: pr, fx, fy, fz, valpar(4)
    real(kind=8) :: prc(9), fxc(9), fyc(9), fzc(9)
!
    character(len=4) :: nompar(4)
    character(len=8) :: prf, fxf, fyf, fzf
    character(len=19) :: nomgd1
    character(len=19) :: nomgd2
!
    logical :: calre1, calfo1
    logical :: calre2, calfo2
    logical :: calcul
!
!
! ----------------------------------------------------------------------
! 1. ---- RECHERCHE DES ADRESSES POUR LES CHARGES SUR LES FACES --------
!-------- VOIR RESLOC --------------------------------------------------
!
    iagd=zi(iref1+4)
!
! 1.1. ==> LES FORCES
!
    calre1 = .false.
    calfo1 = .false.
    nomgd1=' '
    iade1=zi(iref1+6)
    iava1=zi(iref1+7)
    iaptm1=zi(iref1+8)
    if (iade1 .ne. 0) then
        igd1=zi(iade1)
        iacmp=zi(iref1+5)
        ncmpm1=zi(iacmp-1+igd1)
        if (iaptm1 .eq. 0) then
!         CARTE CONSTANTE
            ient1=1
        else
!         LA CARTE A ETE ETENDUE
            imav=zi(ivois+ifa)
            ient1=zi(iaptm1-1+imav)
        endif
        numgd1=zi(iref1+9)
        nomgd1=zk8(iagd-1+numgd1)
        if (nomgd1(1:6) .eq. 'FORC_R') then
            fx=zr(iava1-1+(ient1-1)*ncmpm1+1)
            fy=zr(iava1-1+(ient1-1)*ncmpm1+2)
            fz=zr(iava1-1+(ient1-1)*ncmpm1+3)
            if (abs(fx) .gt. 1.d-15 .or. abs(fy) .gt. 1.d-15 .or. abs(fz) .gt. 1.d-15) then
                calre1 = .true.
            endif
        else if (nomgd1(1:6).eq.'FORC_F') then
            fxf=zk8(iava1-1+(ient1-1)*ncmpm1+1)
            fyf=zk8(iava1-1+(ient1-1)*ncmpm1+2)
            fzf=zk8(iava1-1+(ient1-1)*ncmpm1+3)
            if (fxf .ne. '&FOZERO' .or. fyf .ne. '&FOZERO' .or. fzf .ne. '&FOZERO') then
                calfo1 = .true.
            endif
        endif
    endif
!
! 1.2. ==> LES PRESSIONS
!
    calre2 = .false.
    calfo2 = .false.
    nomgd2=' '
    iade2 = zi(iref2+4)
    iava2 = zi(iref2+5)
    iaptm2 = zi(iref2+6)
    if (iade2 .ne. 0) then
        igd2 = zi(iade2)
        iacmp = zi(iref1+5)
        ncmpm2 = zi(iacmp-1+igd2)
        if (iaptm2 .eq. 0) then
!         CARTE CONSTANTE
            ient2=1
        else
!       LA CARTE A ETE ETENDUE
            imav=zi(ivois+ifa)
            ient2=zi(iaptm2-1+imav)
        endif
        numgd2 = zi(iref2+7)
        nomgd2 = zk8(iagd-1+numgd2)
!GN      WRITE(6,*) 'NUMGD2=',NUMGD2
        if (nomgd2(1:6) .eq. 'PRES_R') then
            pr=zr(iava2-1+(ient2-1)*ncmpm2+1)
            if (abs(pr) .gt. 1.d-15) then
                calre2 = .true.
            endif
        else if (nomgd2(1:6).eq.'PRES_F') then
            prf=zk8(iava2-1+(ient2-1)*ncmpm2+1)
            if (prf .ne. '&FOZERO') then
                calfo2 = .true.
            endif
        endif
    endif
    if (nnof .eq. 1789) then
        write(6,*) 'NOMGD2 :',nomgd2
        write(6,*) '.. IMAV=',imav,' ==> IENT2 =',ient2
!GN          WRITE(6,*) '.. (IENT2-1)*NCMPM2+1 =',(IENT2-1)*NCMPM2+1
        if (nomgd2(1:6) .eq. 'PRES_R') then
            write(6,*) '.. PR =',pr
        else if (nomgd2(1:6).eq.'PRES_F') then
            write(6,*) '.. PRF =',prf
        endif
        write(6,*) 'CALRE1 : ',calre1,', CALFO1 : ',calfo1
        write(6,*) 'CALRE2 : ',calre2,', CALFO2 : ',calfo2
    endif
!
! 1.3. ==> CALCULS PREALABLES
!
    if (calfo1 .or. calfo2) then
        nompar(1)='X'
        nompar(2)='Y'
        nompar(3)='Z'
        nompar(4)='INST'
    endif
!
    if (calre1 .or. calfo1 .or. calre2 .or. calfo2) then
        calcul = .true.
    else
        calcul = .false.
    endif
!
! 2. ---- BOUCLE SUR LES NOEUDS DE LA FACE -----------------------------
!
    do 20 , in = 1 , nnof
!
! 2.1. ==> RECUPERATION DES CONTRAINTES AUX NOEUDS ------------------
! ----- PAS DE CHARGEMENT EXPLICITE SUR LE BORD ==> SIGMA RESTE NUL
!
    if (calcul) then
!
        ino = noe(in,ifa,tymvol)
!
        iaux = isig-1+nbcmp*(ino-1)+1
        sig11(in) = zr(iaux)
        sig22(in) = zr(iaux+1)
        sig33(in) = zr(iaux+2)
        sig12(in) = zr(iaux+3)
        sig13(in) = zr(iaux+4)
        sig23(in) = zr(iaux+5)
!
    else
!
        sig11(in) = 0.d0
        sig22(in) = 0.d0
        sig33(in) = 0.d0
        sig12(in) = 0.d0
        sig13(in) = 0.d0
        sig23(in) = 0.d0
!
    endif
!
! 2.2. ==> CALCUL DES CHARGES APPLIQUEES SUR LE BORD -------------------
! 2.2.1. ==> RECUPERATION DES FORCES -----------------------------------
!
    if (calre1) then
!
        chx(in) = fx
        chy(in) = fy
        chz(in) = fz
!
    else if (calfo1) then
!
        valpar(1) = zr(igeom+3*ino-3)
        valpar(2) = zr(igeom+3*ino-2)
        valpar(3) = zr(igeom+3*ino-1)
        valpar(4) = inst
        call fointe('FM', fxf, 4, nompar, valpar,&
                    fxc(in), ier1)
        call fointe('FM', fyf, 4, nompar, valpar,&
                    fyc(in), ier2)
        call fointe('FM', fzf, 4, nompar, valpar,&
                    fzc(in), ier3)
!
        chx(in) = fxc(in)
        chy(in) = fyc(in)
        chz(in) = fzc(in)
!
! 2.2.2. ==> RECUPERATION DES PRESSIONS --------------------------------
!
    else if (calre2) then
!
        chx(in) = -pr*nx(in)
        chy(in) = -pr*ny(in)
        chz(in) = -pr*nz(in)
!
    else if (calfo2) then
!
        valpar(1) = zr(igeom+3*ino-3)
        valpar(2) = zr(igeom+3*ino-2)
        valpar(3) = zr(igeom+3*ino-1)
        valpar(4) = inst
        call fointe('FM', prf, 4, nompar, valpar,&
                    prc(in), ier)
!
        chx(in) = -prc(in)*nx(in)
        chy(in) = -prc(in)*ny(in)
        chz(in) = -prc(in)*nz(in)
!
! 2.2.3. ==> PAS DE CHARGEMENT EXPLICITE SUR LE BORD
!
    else
!
        chx(in) = 0.d0
        chy(in) = 0.d0
        chz(in) = 0.d0
!
    endif
!
    20 end do
!
    if (nnof .eq. 1789) then
        write(6,*) 'TYPE MAILLE VOLUMIQUE COURANTE :',tymvol
        write(6,1001)
        1000 format(i3,6x,(6(1x,1pe12.5)))
        1001 format('INO        SIXX         SIYY         SIZZ         SIXY',&
     &           '         SIXZ         SIYZ')
        1002 format('INO        CHX          CHY          CHZ          NX  ',&
     &           '         NY           NZ')
        do 110 , in = 1 , nnof
        ino=noe(in,ifa,tymvol)
        write(6,1000) ino,sig11(in),sig22(in),sig33(in), sig12(in)&
            ,sig13(in),sig23(in)
        110     end do
        write(6,1002)
        do 120 , in = 1 , nnof
        ino=noe(in,ifa,tymvol)
        write(6,1000) ino,chx(in),chy(in),chz(in),nx(in),ny(in),&
            nz(in)
        120     end do
    endif
!
end subroutine
