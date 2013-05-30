subroutine ermeb2(ino, iref1, iref2, ivois, igeom,&
                  isig, typema, nbcmp, inst, nx,&
                  ny, tx, ty, sig11, sig22,&
                  sig12, chx, chy)
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
! =====================================================================
!  ERREUR EN MECANIQUE - TERME DE BORD - DIMENSION 2
!  **        **                   *                *
! =====================================================================
!
!     BUT:
!         TROISIEME TERME DE L'ESTIMATEUR D'ERREUR EN RESIDU EXPLICITE :
!         CALCUL DE LA DIFFERENCE ENTRE LES EFFORTS APPLIQUES SUR LE
!         BORD ET LA CONTRAINTE NORMALE.
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   INO      : NUMERO DE L'ARETE
! IN   IREF1    : ADRESSE DES CHARGEMENTS DE TYPE FORCE
! IN   IREF2    : ADRESSE DES CHARGEMENTS DE TYPE PRESSION
! IN   IVOIS    : ADRESSE DES VOISINS
! IN   IGEOM    : ADRESSE DE LA GEOMETRIE
! IN   ISIG     : ADRESSE DES CONTRAINTES AUX NOEUDS
! IN   TYPEMA   : TYPE DE LA MAILLE COURANTE
!               'QU4', 'QU8', 'QU9'
!               'TR3', 'TR6', 'TR7'
! IN   NBCMP    : NOMBRE DE COMPOSANTES DU VECTEUR CONTRAINTE PAR NOEUD
! IN   INST     : INSTANT DE CALCUL
! IN   NX       : VECTEUR DES ABSCISSES DES NORMALES AUX NOEUDS
! IN   NY       : VECTEUR DES ORDONNEES DES NORMALES AUX NOEUDS
! IN   TX       : VECTEUR DES ABSCISSES DES TANGENTES AUX NOEUDS
! IN   TY       : VECTEUR DES ORDONNEES DES TANGENTES AUX NOEUDS
!
!      SORTIE :
!-------------
! OUT  SIG11    : VECTEUR DES CONTRAINTES AUX NOEUDS
!                 ( COMPOSANTE SIG11 )
! OUT  SIG22    : VECTEUR DES CONTRAINTES AUX NOEUDS
!                 ( COMPOSANTE SIG22 )
! OUT  SIG12    : VECTEUR DES CONTRAINTES AUX NOEUDS
!                 ( COMPOSANTE SIG12 )
! OUT  CHX      : VECTEUR DES CHARGEMENTS AUX NOEUDS SELON X
! OUT  CHY      : VECTEUR DES CHARGEMENTS AUX NOEUDS SELON Y
!
! ......................................................................
!
    implicit none
!
! DECLARATION PARAMETRES D'APPEL
    include 'jeveux.h'
    include 'asterfort/fointe.h'
    include 'asterfort/r8inir.h'
    integer :: ino, iref1, iref2, ivois, igeom, isig, nbcmp
    real(kind=8) :: inst, nx(3), ny(3), tx(3), ty(3), sig11(3), sig22(3)
    real(kind=8) :: sig12(3)
    real(kind=8) :: chx(3), chy(3)
    character(len=8) :: typema
!
!
!
!
! DECLARATION VARIABLES LOCALES
!
    integer :: iagd, iade1, iade2, iava1, iava2, iaptm1, iaptm2, igd1, igd2
    integer :: iacmp
    integer :: ncmpm1, ncmpm2, nbs, nbna, jno, mno, imav
    integer :: ier1, ier2, ier3, ier4, ier5, ier6
    integer :: ient1, ient2, numgd1, numgd2
!
    real(kind=8) :: pr, ci, fx, fy, valpar(3), prc(3), cic(3), fxc(3), fyc(3)
!
    character(len=2) :: form, noeu
    character(len=4) :: nompar(3)
    character(len=8) :: prf, cif, fxf, fyf
    character(len=19) :: nomgd1, nomgd2
!
    logical :: flag
!
! ----------------------------------------------------------------------
!
!              X1          X2          X3
!               o-----------o-----------o
!              INO         MNO         JNO
!
!         POINTS  1 --> INO PREMIER POINT DE L'ARETE COURANTE
!                 2 --> JNO DEUXIEME POINT  DE L'ARETE COURANTE
!                 3 --> MNO NOEUD MILIEU S'IL EXISTE
!
!
! ------- RECHERCHE DES ADRESSES POUR LES CHARGES SUR LES SEGMENTS ----
!
    iagd=zi(iref1+4)
!
    iade1=zi(iref1+6)
    iava1=zi(iref1+7)
    iaptm1=zi(iref1+8)
    if (iade1 .ne. 0) then
        igd1=zi(iade1)
        iacmp=zi(iref1+5)
        ncmpm1=zi(iacmp-1+igd1)
    endif
!
    iade2=zi(iref2+4)
    iava2=zi(iref2+5)
    iaptm2=zi(iref2+6)
    if (iade2 .ne. 0) then
        igd2 = zi(iade2)
        iacmp = zi(iref1+5)
        ncmpm2 = zi(iacmp-1+igd2)
    endif
!
! ----- TESTS SUR LA MAILLE COURANTE -----------------------------------
!
    form=typema(1:2)
    noeu=typema(3:3)
!
    if (form .eq. 'TR') then
        nbs=3
    else
        nbs=4
    endif
    if (noeu .eq. '3' .or. noeu .eq. '4') then
        nbna=2
    else
        nbna=3
    endif
!
    if (ino .eq. nbs) then
        jno=1
    else
        jno=ino+1
    endif
!
! --------------------
!
    nomgd1=' '
    nomgd2=' '
    if (iade1 .ne. 0) then
        imav=zi(ivois+ino)
        if (iaptm1 .eq. 0) then
!         CARTE CONSTANTE
            ient1=1
        else
!         LA CARTE A ETE ETENDUE
            ient1=zi(iaptm1-1+imav)
        endif
        numgd1=zi(iref1+9)
        nomgd1=zk8(iagd-1+numgd1)
    endif
!
    if (iade2 .ne. 0) then
        imav=zi(ivois+ino)
        if (iaptm2 .eq. 0) then
!         CARTE CONSTANTE
            ient2=1
        else
!       LA CARTE A ETE ETENDUE
            ient2=zi(iaptm2-1+imav)
        endif
        numgd2=zi(iref2+7)
        nomgd2=zk8(iagd-1+numgd2)
    endif
!
! ----- CALCUL DES CHARGES APPLIQUEES SUR LE BORD ----------------------
! ------- RECUPERATION DES PRESSIONS -----------------------------------
!
    flag=.true.
    call r8inir(3, 0.d0, chx, 1)
    call r8inir(3, 0.d0, chy, 1)
!
    if (nomgd2(1:6) .eq. 'PRES_R') then
        pr=zr(iava2-1+(ient2-1)*ncmpm2+1)
        ci=zr(iava2-1+(ient2-1)*ncmpm2+2)
!
        if (abs(pr) .gt. 1.d-15 .or. abs(ci) .gt. 1.d-15) then
!
            flag=.false.
!
            chx(1)=-pr*nx(1)+ci*tx(1)
            chy(1)=-pr*ny(1)+ci*ty(1)
            chx(2)=-pr*nx(2)+ci*tx(2)
            chy(2)=-pr*ny(2)+ci*ty(2)
!
            if (nbna .eq. 3) then
                chx(3)=-pr*nx(3)+ci*tx(3)
                chy(3)=-pr*ny(3)+ci*ty(3)
            endif
        endif
!
    else if (nomgd2(1:6).eq.'PRES_F') then
        prf=zk8(iava2-1+(ient2-1)*ncmpm2+1)
        cif=zk8(iava2-1+(ient2-1)*ncmpm2+2)
!
        if (prf .ne. '&FOZERO' .or. cif .ne. '&FOZERO') then
!
            flag=.false.
!
            nompar(1)='X'
            nompar(2)='Y'
            nompar(3)='INST'
            valpar(1)=zr(igeom-1+2*(ino-1)+1)
            valpar(2)=zr(igeom-1+2*(ino-1)+2)
            valpar(3)=inst
            call fointe('FM', prf, 3, nompar, valpar,&
                        prc(1), ier1)
            call fointe('FM', cif, 3, nompar, valpar,&
                        cic(1), ier2)
!
            valpar(1)=zr(igeom-1+2*(jno-1)+1)
            valpar(2)=zr(igeom-1+2*(jno-1)+2)
            valpar(3)=inst
            call fointe('FM', prf, 3, nompar, valpar,&
                        prc(2), ier3)
            call fointe('FM', cif, 3, nompar, valpar,&
                        cic(2), ier4)
!
            chx(1)=-prc(1)*nx(1)+cic(1)*tx(1)
            chy(1)=-prc(1)*ny(1)+cic(1)*ty(1)
            chx(2)=-prc(2)*nx(2)+cic(2)*tx(2)
            chy(2)=-prc(2)*ny(2)+cic(2)*ty(2)
!
            if (nbna .eq. 3) then
                mno=nbs+ino
                valpar(1)=zr(igeom-1+2*(mno-1)+1)
                valpar(2)=zr(igeom-1+2*(mno-1)+2)
                valpar(3)=inst
                call fointe('FM', prf, 3, nompar, valpar,&
                            prc(3), ier5)
                call fointe('FM', cif, 3, nompar, valpar,&
                            cic(3), ier6)
!
                chx(3)=-prc(3)*nx(3)+cic(3)*tx(3)
                chy(3)=-prc(3)*ny(3)+cic(3)*ty(3)
!
            endif
        endif
!
! ------- RECUPERATION DES FORCES --------------------------------------
!
    else if (nomgd1(1:6).eq.'FORC_R') then
        fx=zr(iava1-1+(ient1-1)*ncmpm1+1)
        fy=zr(iava1-1+(ient1-1)*ncmpm1+2)
!
        if (abs(fx) .gt. 1.d-15 .or. abs(fy) .gt. 1.d-15) then
!
            flag=.false.
!
            call r8inir(2, fx, chx, 1)
            call r8inir(2, fy, chy, 1)
!
            if (nbna .eq. 3) then
                chx(3)=fx
                chy(3)=fy
            endif
!
        endif
!
    else if (nomgd1(1:6).eq.'FORC_F') then
        fxf=zk8(iava1-1+(ient1-1)*ncmpm1+1)
        fyf=zk8(iava1-1+(ient1-1)*ncmpm1+2)
!
        if (fxf .ne. '&FOZERO' .or. fyf .ne. '&FOZERO') then
!
            flag=.false.
!
            nompar(1)='X'
            nompar(2)='Y'
            nompar(3)='INST'
            valpar(1)=zr(igeom-1+2*(ino-1)+1)
            valpar(2)=zr(igeom-1+2*(ino-1)+2)
            valpar(3)=inst
            call fointe('FM', fxf, 3, nompar, valpar,&
                        fxc(1), ier1)
            call fointe('FM', fyf, 3, nompar, valpar,&
                        fyc(1), ier2)
!
            valpar(1)=zr(igeom-1+2*(jno-1)+1)
            valpar(2)=zr(igeom-1+2*(jno-1)+2)
            valpar(3)=inst
            call fointe('FM', fxf, 3, nompar, valpar,&
                        fxc(2), ier3)
            call fointe('FM', fyf, 3, nompar, valpar,&
                        fyc(2), ier4)
!
            call r8inir(2, fxc, chx, 1)
            call r8inir(2, fyc, chy, 1)
!
            if (nbna .eq. 3) then
                mno=nbs+ino
                valpar(1)=zr(igeom-1+2*(mno-1)+1)
                valpar(2)=zr(igeom-1+2*(mno-1)+2)
                valpar(3)=inst
                call fointe('FM', fxf, 3, nompar, valpar,&
                            fxc(3), ier5)
                call fointe('FM', fyf, 3, nompar, valpar,&
                            fyc(3), ier6)
                chx(3)=fxc(3)
                chy(3)=fyc(3)
            endif
        endif
    endif
!
! ------- RECUPERATION DE SIGMA SUR LA MAILLE COURANTE -----------------
!
    if (flag) then
! ----- PAS DE CHARGEMENT EXPLICITE SUR LE BORD ==> ON PREND SIGMA NUL
        call r8inir(3, 0.d0, sig11, 1)
        call r8inir(3, 0.d0, sig22, 1)
        call r8inir(3, 0.d0, sig12, 1)
!
    else
!
        sig11(1)=zr(isig-1+nbcmp*(ino-1)+1)
        sig22(1)=zr(isig-1+nbcmp*(ino-1)+2)
        sig12(1)=zr(isig-1+nbcmp*(ino-1)+4)
!
        sig11(2)=zr(isig-1+nbcmp*(jno-1)+1)
        sig22(2)=zr(isig-1+nbcmp*(jno-1)+2)
        sig12(2)=zr(isig-1+nbcmp*(jno-1)+4)
!
        if (nbna .eq. 3) then
            mno=nbs+ino
!
            sig11(3)=zr(isig-1+nbcmp*(mno-1)+1)
            sig22(3)=zr(isig-1+nbcmp*(mno-1)+2)
            sig12(3)=zr(isig-1+nbcmp*(mno-1)+4)
!
        endif
    endif
!
end subroutine
