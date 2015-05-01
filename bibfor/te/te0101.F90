subroutine te0101(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/cq3d2d.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/mudirx.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/reflth.h"
#include "asterfort/teattr.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'RIGI_THER      '
!                          CAS COQUE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: i, ind, itemps, j, l, nbddl, nbnoso
    integer :: nbres, nbv, nbvar, ndimax
    real(kind=8) :: un
!-----------------------------------------------------------------------
    parameter (ndimax=27)
    parameter (nbres=24)
    parameter (nbvar=2)
    integer :: icodre(nbres), kpg, spt
    character(len=2) :: num
    character(len=8) :: nompar(nbvar), alias8, fami, poum
    character(len=16) :: nomres(nbres)
    character(len=32) :: phenom
    real(kind=8) :: b(3, 3), a(3, 3, 2, 2), conduc, h, theta
    real(kind=8) :: valres(nbres), axe(3, 3), ang(2), hom(nbres)
    real(kind=8) :: dfdx(9), dfdy(9), poids, pk, coor2d(18)
    real(kind=8) :: mun, zero, deux, quatre, six, sept, huit
    real(kind=8) :: quinze, seize, r
    real(kind=8) :: cour, cosa, sina
    real(kind=8) :: matref(3), matele(3)
    real(kind=8) :: valpar(nbvar), tempe, instan
    real(kind=8) :: rigith(ndimax, ndimax)
    integer :: imate, icacoq, ibid
    integer :: nno, kp, npg1, npg2, gi, pi, gj, pj, k, imattt, ndim, nnos
    integer :: ipoids, ivf, idfde, igeom, jgano, jgano2
    integer :: ndim2, nno2, nnos2
!
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! --- INITIALISATIONS :
!     ---------------
    mun = -1.0d0
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
    quatre = 4.0d0
    six = 6.0d0
    sept = 7.0d0
    huit = 8.0d0
    quinze = 15.0d0
    seize = 16.0d0
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    tempe = zero
    instan = zero
    nompar(1) = 'INST'
    nompar(2) = 'TEMP'
    valpar(1) = instan
    valpar(2) = tempe
!
    matref(1) = zero
    matref(2) = zero
    matref(3) = zero
    matele(1) = zero
    matele(2) = zero
    matele(3) = zero
!
    do 20 i = 1, ndimax
        do 10 j = 1, ndimax
            rigith(i,j) = zero
10      continue
20  end do
!
!
! --- RECUPERATION DES COORDONNEES DES NOEUDS DE L'ELEMENT :
!     ----------------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! --- RECUPERATION DU MATERIAU :
!     ------------------------
    call jevech('PMATERC', 'L', imate)
!
! --- RECUPERATION DE L'EPAISSEUR DE LA COQUE ET DES 2 ANGLES
! --- PERMETTANT DE PASSER DU REPERE GLOBAL AU REPERE DE REFERENCE
! --- TANGENT A LA COQUE :
!     ------------------
    call jevech('PCACOQU', 'L', icacoq)
!
! --- RECUPERATION DE L'INSTANT DU CALCUL ET
! --- DU PARAMETRE THETA DE LA METHODE 'THETA' UTILISEE
! --- POUR RESOUDRE L'EQUATION DIFFERENTIELLE EN TEMPS DE LA
! --- TEMPERATURE (EN STATIONNAIRE THETA =1 ) :
!     ---------------------------------------
    call jevech('PTEMPSR', 'L', itemps)
    valpar(1) = zr(itemps)
    theta = zr(itemps+2)
!
! --- NOMBRE DE NOEUDS SOMMETS :
!     ------------------------
    call teattr('S', 'ALIAS8', alias8, ibid)
    if (alias8(6:7) .eq. 'TR') then
        nbnoso = 3
    else if (alias8(6:7).eq.'QU') then
        nbnoso = 4
    endif
!
! --- RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM
!     -------------------------------------------------
    call rccoma(zi(imate), 'THER', 1, phenom, icodre(1))
!
! --- DETERMINATION DES TENSEURS DE CONDUCTIVITE MEMBRANAIRE
! --- ET TRANSVERSE :
!     =============
!
! --- CAS DES COQUES MULTICOUCHES :
!     ---------------------------
    if (phenom .eq. 'THER_COQMU') then
!
! ---   DETERMINATION DE LA ROTATION FAISANT PASSER DU REPERE
! ---   DE REFERENCE AU REPERE DE L'ELEMENT :
!       -----------------------------------
        call mudirx(nbnoso, zr(igeom), 3, zr(icacoq+1), zr(icacoq+2),&
                    axe, ang)
!
! ---   NOM DES COMPOSANTES DU TENSEUR DE CONDUCTIVITE HOMOGENEISE :
!       ----------------------------------------------------------
        do 30 i = 1, nbres
            call codent(i, 'G', num)
            nomres(i) = 'HOM_'//num
30      continue
!
! ---   INTERPOLATION DES TERMES DU TENSEUR DE CONDUCTIVITE
! ---   EN FONCTION DU TEMPS ET DE LA TEMPERATURE
! ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
! ---    INACTIVE POUR LE MOMENT) :
!       -------------------------
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THER_COQMU', nbvar, nompar, valpar,&
                    nbres, nomres, valres, icodre, 1)
!
! ---   VALEURS DES CARACTERISIQUES DU MATERIAU DANS LE REPERE
! ---   DE L'ELEMENT ( PARCE QUE C'EST DANS CE REPERE QUE LE
! ---   FLUX THERMIQUE EST LE PLUS SIMPLE A ECRIRE) :
!       -------------------------------------------
        do 40 i = 1, 6
            call reflth(ang, valres(3* (i-1)+1), hom(3* (i-1)+1))
40      continue
!
! ---   TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
!       -----------------------------------
        a(1,1,1,1) = hom(1)
        a(1,1,2,2) = hom(2)
        a(1,1,1,2) = hom(3)
        a(2,1,1,1) = hom(4)
        a(2,1,2,2) = hom(5)
        a(2,1,1,2) = hom(6)
        a(3,1,1,1) = hom(7)
        a(3,1,2,2) = hom(8)
        a(3,1,1,2) = hom(9)
        a(2,2,1,1) = hom(10)
        a(2,2,2,2) = hom(11)
        a(2,2,1,2) = hom(12)
        a(3,2,1,1) = hom(13)
        a(3,2,2,2) = hom(14)
        a(3,2,1,2) = hom(15)
        a(3,3,1,1) = hom(16)
        a(3,3,2,2) = hom(17)
        a(3,3,1,2) = hom(18)
!
! ---   TENSEUR DE CONDUCTIVITE TRANSVERSE :
!       ----------------------------------
        b(1,1) = valres(19)
        b(2,1) = valres(20)
        b(3,1) = valres(21)
        b(2,2) = valres(22)
        b(3,2) = valres(23)
        b(3,3) = valres(24)
!
! --- CAS DES COQUES ISOTROPES :
!     ------------------------
    else if (phenom.eq.'THER') then
!
! ---   INTERPOLATION DE LA CONDUCTIVITE EN FONCTION DU TEMPS
! ---   ET DE LA TEMPERATURE
! ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
! ---    INACTIVE POUR LE MOMENT) :
!       -------------------------
        nbv = 1
        nomres(1) = 'LAMBDA'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THER', nbvar, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
! ---   CONDUCTIVITE  :
!       ------------
        conduc = valres(1)
!
! ---   DEMI-EPAISSEUR  :
!       --------------
        h = zr(icacoq)/deux
!
! ---   TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
!       -----------------------------------
        do 80 l = 1, 2
            do 70 k = 1, l
                do 60 i = 1, 3
                    do 50 j = 1, i
                        a(i,j,k,l) = zero
50                  continue
60              continue
70          continue
80      continue
!
        a(1,1,1,1) = seize*conduc*h/quinze
        a(1,1,2,2) = a(1,1,1,1)
        a(2,2,1,1) = quatre*conduc*h/quinze
        a(2,2,2,2) = a(2,2,1,1)
        a(3,3,1,1) = quatre*conduc*h/quinze
        a(3,3,2,2) = a(2,2,1,1)
        a(2,1,1,1) = deux*conduc*h/quinze
        a(2,1,2,2) = a(2,1,1,1)
        a(3,1,1,1) = deux*conduc*h/quinze
        a(3,1,2,2) = a(3,1,1,1)
        a(3,2,1,1) = mun*conduc*h/quinze
        a(3,2,2,2) = a(3,2,1,1)
!
! ---   TENSEUR DE CONDUCTIVITE TRANSVERSE :
!       ----------------------------------
        b(1,1) = seize*conduc/ (six*h)
        b(2,1) = mun*huit*conduc/ (six*h)
        b(3,1) = b(2,1)
        b(2,2) = sept*conduc/ (six*h)
        b(3,2) = conduc/ (six*h)
        b(3,3) = b(2,2)
!
! --- CAS DES COQUES HETEROGENES :
!     --------------------------
    else if (phenom.eq.'THER_COQUE') then
!
! ---   LES DIRECTIONS 1 ET 2 DESIGNENT CELLES DU PLAN DE LA PLAQUE
! ---   LA DIRECTION 3 EST PERPENDICULAIRE
! ---   ON ADMET QUE LE TENSEUR DE CONDUCTIVITE EN CHAQUE POINT
! ---   EST DIAGONAL ET QUE SES VALEURS PROPRES SONT
! ---   LAMBDA_1 , LAMBDA_2 ET LAMBDA_3
! ---   D'AUTRE PART, SOIENT P1(X3), P2(X3), P3(X3) LES POLYNOMES
! ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS A L'INTERPOLATION
! ---   DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
! ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
! ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
! ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
! ---   (I.E. T(X1,X2,X3) =    P1(X3)*TMOY(X1,X2)
! ---                        + P2(X3)*TINF(X1,X2)
! ---                        + P3(X3)*TSUP(X1,X2)
! ---   LES TERMES DU TENSEUR DE CONDUCTIVITE HOMOGENEISE SONT ALORS
! ---   POUR LE TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
!       -------------------------------------------
! ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P1(X3)*P1(X3).DX3)
        nomres(1) = 'COND_LMM'
! ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P1(X3)*P2(X3).DX3)
        nomres(2) = 'COND_LMP'
! ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P2(X3)*P2(X3).DX3)
        nomres(3) = 'COND_LPP'
! ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P2(X3)*P3(X3).DX3)
        nomres(4) = 'COND_LSI'
! ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P1(X3)*P1(X3).DX3)
        nomres(5) = 'COND_TMM'
! ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P1(X3)*P2(X3).DX3)
        nomres(6) = 'COND_TMP'
! ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P2(X3)*P2(X3).DX3)
        nomres(7) = 'COND_TPP'
! ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P2(X3)*P3(X3).DX3)
        nomres(8) = 'COND_TSI'
! ---   POUR LE TENSEUR DE CONDUCTIVITE TRANSVERSE :
!       ------------------------------------------
! ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P1'(X3)*P1'(X3).DX3)
        nomres(9) = 'COND_NMM'
! ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P1'(X3)*P2'(X3).DX3)
        nomres(10) = 'COND_NMP'
! ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P2'(X3)*P2'(X3).DX3)
        nomres(11) = 'COND_NPP'
! ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P2'(X3)*P3'(X3).DX3)
        nomres(12) = 'COND_NSI'
!
! ---  INTERPOLATION DES TERMES DU TENSEUR DE CONDUCTIVITE
! ---  EN FONCTION DU TEMPS ET DE LA TEMPERATURE :
! ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
! ---    INACTIVE POUR LE MOMENT) :
!      --------------------------
        nbv = 12
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', phenom, nbvar, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
! ---   DETERMINATION DE LA ROTATION FAISANT PASSER DU REPERE
! ---   DE REFERENCE AU REPERE DE L'ELEMENT :
!       -----------------------------------
        call mudirx(nbnoso, zr(igeom), 3, zr(icacoq+1), zr(icacoq+2),&
                    axe, ang)
!
! ---   PASSAGE DU REPERE DE REFERENCE AU REPERE DE L'ELEMENT :
!       -----------------------------------------------------
!
! ---   TERMES DE CONDUCTIVITE MEMBRANAIRE DANS LE REPERE DE L'ELEMENT :
!       --------------------------------------------------------------
! ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
! ---      ( SOMME_EP(LAMBDA_1*P1*P1.DX3)     0.                     )
! ---      (         0.                  SOMME_EP(LAMBDA_2*P1*P1.DX3))
!       --------------------------------------------------------------
        matref(1) = valres(1)
        matref(2) = valres(5)
        matref(3) = zero
        call reflth(ang, matref, matele)
!
        a(1,1,1,1) = matele(1)
        a(1,1,2,2) = matele(2)
        a(1,1,1,2) = matele(3)
        a(1,1,2,1) = matele(3)
!  ------------------------------------------------------------------
! ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
! ---      ( SOMME_EP(LAMBDA_1*P1*P2.DX3)     0.                     )
! ---      (         0.                  SOMME_EP(LAMBDA_2*P1*P2.DX3))
!       --------------------------------------------------------------
        matref(1) = valres(2)
        matref(2) = valres(6)
        matref(3) = zero
        call reflth(ang, matref, matele)
!
        a(1,2,1,1) = matele(1)
        a(1,2,2,2) = matele(2)
        a(1,2,1,2) = matele(3)
        a(1,2,2,1) = matele(3)
!
        a(2,1,1,1) = a(1,2,1,1)
        a(2,1,2,2) = a(1,2,2,2)
        a(2,1,1,2) = a(1,2,1,2)
        a(2,1,2,1) = a(1,2,2,1)
!
        a(1,3,1,1) = matele(1)
        a(1,3,2,2) = matele(2)
        a(1,3,1,2) = matele(3)
        a(1,3,2,1) = matele(3)
!
        a(3,1,1,1) = a(1,3,1,1)
        a(3,1,2,2) = a(1,3,2,2)
        a(3,1,1,2) = a(1,3,1,2)
        a(3,1,2,1) = a(1,3,2,1)
!  ------------------------------------------------------------------
! ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
! ---      ( SOMME_EP(LAMBDA_1*P2*P2.DX3)     0.                     )
! ---      (         0.                  SOMME_EP(LAMBDA_2*P2*P2.DX3))
!       --------------------------------------------------------------
        matref(1) = valres(3)
        matref(2) = valres(7)
        matref(3) = zero
        call reflth(ang, matref, matele)
!
        a(2,2,1,1) = matele(1)
        a(2,2,2,2) = matele(2)
        a(2,2,1,2) = matele(3)
        a(2,2,2,1) = matele(3)
!
        a(3,3,1,1) = matele(1)
        a(3,3,2,2) = matele(2)
        a(3,3,1,2) = matele(3)
        a(3,3,2,1) = matele(3)
!  ------------------------------------------------------------------
! ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
! ---      ( SOMME_EP(LAMBDA_1*P2*P3.DX3)     0.                     )
! ---      (         0.                  SOMME_EP(LAMBDA_2*P2*P3.DX3))
!       --------------------------------------------------------------
        matref(1) = valres(4)
        matref(2) = valres(8)
        matref(3) = zero
        call reflth(ang, matref, matele)
!
        a(2,3,1,1) = matele(1)
        a(2,3,2,2) = matele(2)
        a(2,3,1,2) = matele(3)
        a(2,3,2,1) = matele(3)
!
        a(3,2,1,1) = a(2,3,1,1)
        a(3,2,2,2) = a(2,3,2,2)
        a(3,2,1,2) = a(2,3,1,2)
        a(3,2,2,1) = a(2,3,2,1)
!  ------------------------------------------------------------------
!
! ---   TERMES DE CONDUCTIVITE TRANSVERSE :
!       ---------------------------------
        b(1,1) = valres(9)
        b(1,2) = valres(10)
        b(1,3) = valres(10)
        b(2,2) = valres(11)
        b(2,3) = valres(12)
        b(3,3) = valres(11)
        b(2,1) = b(1,2)
        b(3,1) = b(1,3)
        b(3,2) = b(2,3)
!
    else
        call utmess('F', 'ELEMENTS3_17', sk=phenom)
    endif
!
!======================================
! --- CALCUL DE LA RIGIDITE THERMIQUE =
!======================================
!
! --- CAS DES COQUES SURFACIQUES :
!     --------------------------
    if (nomte .ne. 'THCPSE3 ' .and. nomte .ne. 'THCASE3 ') then
!
! ---   CALCUL DES COORDONNEES DES CONNECTIVITES DANS LE REPERE
! ---   DE L'ELEMENT :
!       ------------
        call cq3d2d(nno, zr(igeom), un, zero, coor2d)
!
! ---  CALCUL DE LA RIGIDITE THERMIQUE MEMBRANAIRE :
!      -------------------------------------------
!
! ---  BOUCLE SUR LES POINTS D'INTEGRATION :
!      -----------------------------------
        do 130 kp = 1, npg1
            k = (kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, coor2d,&
                        poids, dfdx, dfdy)
            do 120 gi = 1, nno
                do 110 gj = 1, gi
                    do 100 pi = 1, 3
                        do 90 pj = 1, pi
                            pk = a(pi,pj,1,1)*dfdx(gi)*dfdx(gj) + a(pi,pj,2,2)*dfdy(gi)*dfdy(gj) &
                                 &+ a(pi,pj, 1,2)*dfdx(gi)*dfdy(gj) + a(pi,pj,1,2)* dfdy(gi)*dfdx&
                                 &(gj)
                            pk = pk*poids*theta
!
! ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
! ---     INFERIEURE DE LA SOUS-MATRICE :
!         -----------------------------
                            if ((pi.ne.pj) .and. (gi.ne.gj)) then
                                i = 3* (gi-1) + pj
                                j = 3* (gj-1) + pi
                                rigith(i,j) = rigith(i,j) + pk
                            endif
!
! ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
! ---     DE LA SOUS-MATRICE :
!         ------------------
                            i = 3* (gi-1) + pi
                            j = 3* (gj-1) + pj
                            rigith(i,j) = rigith(i,j) + pk
90                      continue
100                  continue
110              continue
120          continue
130      continue
!
! ---  CALCUL DE LA RIGIDITE THERMIQUE TRANSVERSE :
!      ------------------------------------------
!
! ---  UTILISATION D'UNE INTEGRATION AVEC UN NOMBRE DE POINTS
! ---  SUPERIEUR OU EGAL AU NOMBRE DE POINTS UTILISES POUR LA
! ---  RIGIDITE MEMBRANAIRE :
!      --------------------
        call elrefe_info(fami='MASS',ndim=ndim2,nno=nno2,nnos=nnos2,&
  npg=npg2,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano2)
!
! ---  BOUCLE SUR LES POINTS D'INTEGRATION :
!      -----------------------------------
        do 180 kp = 1, npg2
            k = (kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, coor2d,&
                        poids, dfdx, dfdy)
            do 170 gi = 1, nno
                do 160 gj = 1, gi
                    do 150 pi = 1, 3
                        do 140 pj = 1, pi
                            pk = b(pi,pj)*zr(ivf+k+gi-1)*zr(ivf+k+gj- 1)* poids*theta
!
! ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
! ---     INFERIEURE DE LA SOUS-MATRICE :
!         -----------------------------
                            if ((pi.ne.pj) .and. (gi.ne.gj)) then
                                i = 3* (gi-1) + pj
                                j = 3* (gj-1) + pi
                                rigith(i,j) = rigith(i,j) + pk
                            endif
!
! ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
! ---     DE LA SOUS-MATRICE :
!         ------------------
                            i = 3* (gi-1) + pi
                            j = 3* (gj-1) + pj
                            rigith(i,j) = rigith(i,j) + pk
140                      continue
150                  continue
160              continue
170          continue
180      continue
!
! --- CAS DES COQUES LINEIQUES :
!     ------------------------
    else
!
! ---  CALCUL DE LA RIGIDITE THERMIQUE MEMBRANAIRE :
!      -------------------------------------------
!
! ---  BOUCLE SUR LES POINTS D'INTEGRATION :
!      -----------------------------------
        do 240 kp = 1, npg1
            k = (kp-1)*nno
            call dfdm1d(nno, zr(ipoids+kp-1), zr(idfde+k), zr(igeom), dfdx,&
                        cour, poids, cosa, sina)
!
            if (nomte .eq. 'THCASE3') then
                r = zero
                do 190 i = 1, nno
                    r = r + zr(igeom+2* (i-1))*zr(ivf+k+i-1)
190              continue
                poids = poids*r
            endif
!
            do 230 gi = 1, nno
                do 220 gj = 1, gi
                    do 210 pi = 1, 3
                        do 200 pj = 1, pi
                            pk = a(pi,pj,1,1)*dfdx(gi)*dfdx(gj)
                            pk = pk*poids*theta
!
! ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
! ---     INFERIEURE DE LA SOUS-MATRICE :
!         -----------------------------
                            if ((pi.ne.pj) .and. (gi.ne.gj)) then
                                i = 3* (gi-1) + pj
                                j = 3* (gj-1) + pi
                                rigith(i,j) = rigith(i,j) + pk
                            endif
!
! ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
! ---     DE LA SOUS-MATRICE :
!         ------------------
                            i = 3* (gi-1) + pi
                            j = 3* (gj-1) + pj
                            rigith(i,j) = rigith(i,j) + pk
200                      continue
210                  continue
220              continue
230          continue
240      continue
!
! ---  CALCUL DE LA RIGIDITE THERMIQUE TRANSVERSE :
!      ------------------------------------------
!
! ---  BOUCLE SUR LES POINTS D'INTEGRATION :
!      -----------------------------------
        do 290 kp = 1, npg1
            k = (kp-1)*nno
            call dfdm1d(nno, zr(ipoids+kp-1), zr(idfde+k), zr(igeom), dfdx,&
                        cour, poids, cosa, sina)
            do 280 gi = 1, nno
                do 270 gj = 1, gi
                    do 260 pi = 1, 3
                        do 250 pj = 1, pi
                            pk = b(pi,pj)*zr(ivf+k+gi-1)*zr(ivf+k+gj- 1)*poids* theta
!
! ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
! ---     INFERIEURE DE LA SOUS-MATRICE :
!         -----------------------------
                            if ((pi.ne.pj) .and. (gi.ne.gj)) then
                                i = 3* (gi-1) + pj
                                j = 3* (gj-1) + pi
                                rigith(i,j) = rigith(i,j) + pk
                            endif
!
! ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
! ---     DE LA SOUS-MATRICE :
!         ------------------
                            i = 3* (gi-1) + pi
                            j = 3* (gj-1) + pj
                            rigith(i,j) = rigith(i,j) + pk
250                      continue
260                  continue
270              continue
280          continue
290      continue
!
    endif
!
! --- RECUPERATION DE LA MATRICE DE RIGIDITE THERMIQUE EN SORTIE DU TE :
!     ----------------------------------------------------------------
    call jevech('PMATTTR', 'E', imattt)
!
! --- AFFECTATION DE LA MATRICE DE RIGIDITE THERMIQUE EN SORTIE DU TE :
!     ---------------------------------------------------------------
    nbddl = 3*nno
    ind = 0
    do 310 i = 1, nbddl
        do 300 j = 1, i
            ind = ind + 1
            zr(imattt+ind-1) = rigith(i,j)
300      continue
310  end do
!
end subroutine
