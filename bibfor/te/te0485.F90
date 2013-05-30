subroutine te0485(option, nomte)
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
! ======================================================================
! ELEMENT SHB15
!.......................................................................
    implicit none
!
!          ELEMENT SHB
!    FONCTION REALISEE:
!            OPTION : 'RIGI_MECA      '
!                            CALCUL DES MATRICES ELEMENTAIRES  3D
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!
    include 'jeveux.h'
!
!-----------------------------------------------------------------------
    include 'asterfort/elref4.h'
    include 'asterfort/idsshb.h'
    include 'asterfort/jevech.h'
    include 'asterfort/moytem.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/sh1sig.h'
    include 'asterfort/sh2sig.h'
    include 'asterfort/sh6sig.h'
    include 'asterfort/sh8sig.h'
    integer :: i, icont, idepl, idfde, igeom, imate, ipoids
    integer :: iret, ivf, j, jgano, lag, nbres, nbv
    integer :: ndim, nno, nnos, npg
    real(kind=8) :: tempm, ygot
!-----------------------------------------------------------------------
    parameter (nbres=2)
    character(len=4) :: fami
    integer :: icodre(nbres), kpg, spt
    character(len=8) :: nomres(nbres), famil, poum
    character(len=16) :: nomte, option, nomshb
    real(kind=8) :: sigma(120), para(11)
    real(kind=8) :: fstab(12)
    real(kind=8) :: valres(nbres), dusx(180)
    integer :: nbsig
    real(kind=8) :: nu, e
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
! --- INITIALISATIONS :
    call idsshb(ndim, nno, npg, nomshb)
    nbsig = 6
    do 10 i = 1, 11
        para(i) = 0.d0
10  end do
    famil='FPG1'
    kpg=1
    spt=1
    poum='+'
    if (option .eq. 'SIEF_ELGA') then
! ----  RECUPERATION DES COORDONNEES DES CONNECTIVITES
        call jevech('PGEOMER', 'L', igeom)
! ----  RECUPERATION DU MATERIAU DANS ZI(IMATE)
        call jevech('PMATERC', 'L', imate)
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nbv = 2
! ----  INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
! ----  ET DU TEMPS
!
        call moytem(fami, npg, 1, '+', tempm,&
                    iret)
        call rcvalb(famil, kpg, spt, poum, zi(imate),&
                    ' ', 'ELAS', 1, 'TEMP', tempm,&
                    nbv, nomres, valres, icodre, 1)
        e = valres(1)
        nu = valres(2)
! ----  PARAMETRES MATERIAUX
        ygot = e
! ----  PARAMETRES MATERIAUX POUR LE CALCUL DE LA
! ----  MATRICE TANGENTE PLASTIQUE
!       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
!       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
        lag = 0
        para(1) = e
        para(2) = nu
        para(3) = ygot
        para(4) = 0
        para(5) = 1
        para(6) = lag
!
    endif
!
!  ===========================================
!  -- CONTRAINTES
!  ===========================================
    if (option .eq. 'SIEF_ELGA') then
        if (nomshb .eq. 'SHB8') then
            do 60 i = 1, nbsig*npg
                sigma(i) = 0.d0
60          continue
!        SIGMA(1) = WORK(150)
!        RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT
!        DEPL Dans ZR(IDEPL)
            call jevech('PDEPLAR', 'L', idepl)
!        VECTEUR DES CONTRAINTES AUX POINTS D'INTEGRATION
            call sh8sig(zr(igeom), para, zr(idepl), dusx, sigma)
!        RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
            call jevech('PCONTRR', 'E', icont)
            call r8inir(12, 0.d0, fstab, 1)
            do 67 i = 1, 12
                zr(icont+i-1+6)=fstab(i)
67          continue
            do 69 i = 1, 5
                do 68 j = 1, 6
                    zr(icont+18*(i-1)+j-1)=sigma(6*(i-1)+j)
68              continue
69          continue
!
        else if (nomshb.eq.'SHB6') then
            do 70 i = 1, nbsig*npg
                sigma(i) = 0.d0
70          continue
!        RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT
!        DEPL Dans ZR(IDEPL)
            call jevech('PDEPLAR', 'L', idepl)
!        VECTEUR DES CONTRAINTES AUX POINTS D'INTEGRATION
            call sh6sig(zr(igeom), para, zr(idepl), dusx, sigma)
!        RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
            call jevech('PCONTRR', 'E', icont)
            do 79 i = 1, 5
                do 78 j = 1, 6
                    zr(icont+18*(i-1)+j-1)=sigma(6*(i-1)+j)
78              continue
79          continue
!
        else if (nomshb.eq.'SHB15') then
            do 80 i = 1, nbsig*npg
                sigma(i) = 0.d0
80          continue
!        RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT
!        DEPL Dans ZR(IDEPL)
            call jevech('PDEPLAR', 'L', idepl)
!        VECTEUR DES CONTRAINTES AUX POINTS D'INTEGRATION
!        RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
            call sh1sig(zr(igeom), para, zr(idepl), dusx, sigma)
            call jevech('PCONTRR', 'E', icont)
!
            do 89 i = 1, 15
                do 88 j = 1, 6
                    zr(icont+18*(i-1)+j-1)=sigma(6*(i-1)+j)
88              continue
89          continue
!
        else if (nomshb.eq.'SHB20') then
            do 91 i = 1, nbsig*npg
                sigma(i) = 0.d0
91          continue
!        RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT
!        DEPL Dans ZR(IDEPL)
            call jevech('PDEPLAR', 'L', idepl)
!        VECTEUR DES CONTRAINTES AUX POINTS D'INTEGRATION
!        RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
            call sh2sig(zr(igeom), para, zr(idepl), dusx, sigma)
            call jevech('PCONTRR', 'E', icont)
!
            do 93 i = 1, 20
                do 92 j = 1, 6
                    zr(icont+18*(i-1)+j-1)=sigma(6*(i-1)+j)
92              continue
93          continue
        endif
    endif
!
end subroutine
