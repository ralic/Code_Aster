subroutine te0110(option, nomte)
! aslint: disable=W1501
    implicit none
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/cq3d2d.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/mudirx.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/reflth.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ......................................................................
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
!
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_EVOL'
!                          CAS COQUES SURFACIQUES ET LEURS BORDS
!                         (CAS COQUES LINEIQUES NON FAIT)
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nbres, nbpar, ndimax
!-----------------------------------------------------------------------
    integer :: icoehf, icoehr, ier, iret, ivf1, ivf2, kq
    integer :: l, mzr, nbddl, nbnoso, nbv, nbvar
    real(kind=8) :: deltat, pk, pm, poi1, poi2, theta, un
    real(kind=8) :: xgau, ygau, zero, zgau
!-----------------------------------------------------------------------
    parameter (nbres=30)
    parameter (nbpar=4)
    parameter (ndimax=27)
    integer :: icodre(nbres)
    character(len=2) :: num
    integer :: codmat
    character(len=8) :: hfmoin, hfplus, hfbord, alias8, fami, poum, nompar(nbpar)
    character(len=16) :: nomres(nbres)
    character(len=32) :: phenom
    real(kind=8) :: valres(nbres), valpar(nbpar), hom(nbres)
    real(kind=8) :: m(3, 3), roc, h, tpg(3), lam, dtpgdx(3), b(3, 3)
    real(kind=8) :: coor2d(18), dfdx(9), dfdy(9), poids, dtpgdy(3)
    real(kind=8) :: axe(3, 3), ang(2), a(3, 3, 2, 2)
    real(kind=8) :: matn(3, 3), matp(3, 3)
    real(kind=8) :: matref(3), matele(3)
    real(kind=8) :: rigith(ndimax, ndimax), masse(ndimax, ndimax)
    real(kind=8) :: long, hmoin, hplus, hbord
    integer :: i, j, nno, kp, npg1, npg2, gi, pi, ivectt, itemp, icacoq
    integer :: ipoids, ivf, idfde, igeom, imate, nnos, jgano
    integer :: itemps, k, pj, gj, ndim, ibid, kpg, spt
!
!
! --- DETERMINATION DU SECOND MEMBRE CHAR_THER_EVOL :
! --- F =   1/DT*MASSE_THER*(T-) - (1-THETA)*RIGI_THER*(T-)
! ---    - (1-THETA)*HPLUS*(T-)  - (1-THETA)*HMOIN*(T-)
! --- MASSE_THER DESIGNE LA MASSE THERMIQUE
! --- RIGI_THER EST LA RIGIDITE THERMIQUE
! --- LES AUTRES TERMES PRIS EN COMPTE SONT SEULEMENT DES TERMES
! --- D'ECHANGE.
! --- T- DESIGNE LE CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT
! --- HPLUS EST LE COEFFICIENT D'ECHANGE DE LA SURFACE SUPERIEURE
! --- DE LA COQUE AVEC L'EXTERIEUR
! --- HMOIN EST LE COEFFICIENT D'ECHANGE DE LA SURFACE INFERIEURE
! --- DE LA COQUE AVEC L'EXTERIEUR
! --- EN FAIT LES EXPRESSIONS HPLUS ET HMOIN SONT
! --- DISCRETISEES ET INTEGREES SUR LES SURFACES SUR LESQUELLES
! --- ELLES S'APPLIQUENT .
! --- LES OPERATEURS DE MASSE THERMIQUE, RIGIDITE THERMIQUE ET
! --- D'ECHANGE S'APPLIQUENT SUR LE TRIPLET (T_MOY,T_INF,T_SUP)
! --- DANS CET ORDRE OU
! --- T_MOY EST LA TEMPERATURE SUR LE FEUILLET MOYEN DE LA COQUE
! --- T_INF EST LA TEMPERATURE SUR LE FEUILLET INFERIEUR DE LA COQUE
! --- T_SUP EST LA TEMPERATURE SUR LE FEUILLET SUPERIEUR DE LA COQUE
!     ==============================================================
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
    un = 1.0d0
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    nompar(1) = 'INST'
    nompar(2) = 'X'
    nompar(3) = 'Y'
    nompar(4) = 'Z'
    valpar(1) = zero
    valpar(2) = zero
    valpar(3) = zero
    valpar(4) = zero
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    hplus = zero
    hmoin = zero
    hbord = zero
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
            masse(i,j) = zero
10      continue
20  end do
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
! --- RECUPERATION DE L'INSTANT DU CALCUL, DU PAS DE TEMPS ET
! --- DU PARAMETRE THETA DE LA METHODE 'THETA' UTILISEE
! --- POUR RESOUDRE L'EQUATION DIFFERENTIELLE EN TEMPS DE LA
! --- TEMPERATURE (EN STATIONNAIRE THETA =1 ) :
!     ---------------------------------------
    call jevech('PTEMPSR', 'L', itemps)
!
! --- RECUPERATION DU CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT :
!     ----------------------------------------------------------
    call jevech('PTEMPER', 'L', itemp)
!
    valpar(1) = zr(itemps)
    deltat = zr(itemps+1)
    theta = zr(itemps+2)
!
! --- RECUPERATION EVENTUELLE DES COEFFICIENTS D'ECHANGE AVEC
! --- L'EXTERIEUR :
!     -----------
    call tecach('NNN', 'PCOEFHR', 'L', iret, iad=icoehr)
    call tecach('NNN', 'PCOEFHF', 'L', iret, iad=icoehf)
!
    if (nomte .ne. 'THCOSE3 ' .and. nomte .ne. 'THCOSE2 ') then
! ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES REELS :
        if (icoehr .ne. 0) then
            hmoin = zr(icoehr)
            hplus = zr(icoehr+1)
        endif
! ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES FONCTIONS :
        if (icoehf .ne. 0) then
            hfmoin = zk8(icoehf)
            hfplus = zk8(icoehf+1)
        endif
        else if (nomte.eq.'THCOSE3' .or. nomte.eq.'THCOSE2')&
    then
! ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES REELS :
        if (icoehr .gt. 0) then
            h = zr(icoehr)
        endif
! ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES FONCTIONS :
        if (icoehf .gt. 0) then
            hfbord = zk8(icoehf)
        endif
    endif
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
!..................................................................
!.    CAS DES COQUES SURFACIQUES                                  .
!..................................................................
!
    if (nomte.ne.'THCOSE3' .and. nomte.ne.'THCOSE2') then
!
! ---   RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM
!       -------------------------------------------------
        call rccoma(zi(imate), 'THER', 1, phenom, codmat)
!
! ---   DETERMINATION DES TENSEURS DE CONDUCTIVITE MEMBRANAIRE
! ---   ET TRANSVERSE ET DU TENSEUR DE CAPACITE THERMIQUE
! ---
!
! ---   CAS DES COQUES THERMIQUES MULTI-COUCHES
!       =======================================
        if (phenom .eq. 'THER_COQMU') then
!
! ---   DETERMINATION DE LA ROTATION FAISANT PASSER DU REPERE
! ---   DE REFERENCE AU REPERE DE L'ELEMENT
!       -----------------------------------
            call mudirx(nbnoso, zr(igeom), 3, zr(icacoq+1), zr(icacoq+2),&
                        axe, ang)
!
! ---   NOM DES COMPOSANTES DU TENSEUR DE CONDUCTIVITE HOMOGENEISE :
!       ----------------------------------------------------------
            do 30 i = 1, nbres
                call codent(i, 'G', num)
                nomres(i) = 'HOM_'//num
30          continue
!
! ---   INTERPOLATION DES TERMES DU TENSEUR DE CONDUCTIVITE
! ---   EN FONCTION DU TEMPS :
!       --------------------
            nbvar = 1
            call rcvalb(fami, kpg, spt, poum, zi(imate),&
                        ' ', 'THER_COQMU', nbvar, nompar, valpar,&
                        nbres, nomres, valres, icodre, 1)
!
! ---   CONSTRUCTION DE LA MATRICE DE PASSAGE DU REPERE UTILISATEUR
! ---   AU REPERE ELEMENT :
!       -----------------
            call mudirx(nbnoso, zr(igeom), 3, zr(icacoq+1), zr(icacoq+2),&
                        axe, ang)
!
! ---   VALEURS DES CARACTERISIQUES DU MATERIAU DANS LE REPERE
! ---   DE L'ELEMENT ( PARCE QUE C'EST DANS CE REPERE QUE LE
! ---   FLUX THERMIQUE EST LE PLUS SIMPLE A ECRIRE) :
!       -------------------------------------------
            do 40 i = 1, 6
                call reflth(ang, valres(3* (i-1)+1), hom(3* (i-1)+1))
40          continue
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
! ---   LES TERMES DE CONDUCTIVITE TRANSVERSE NE SE TRANSFORMENT PAS
! ---   PAR CHANGEMENT DE REPERE :
!       ------------------------
            b(1,1) = valres(19)
            b(2,1) = valres(20)
            b(3,1) = valres(21)
            b(2,2) = valres(22)
            b(3,2) = valres(23)
            b(3,3) = valres(24)
!
! ---   LES TERMES RELATIFS A LA CAPACITE CALORIFIQUE NE SE
! ---   TRANSFORMENT PAS PAR CHANGEMENT DE REPERE :
!       -----------------------------------------
            m(1,1) = valres(25)
            m(2,1) = valres(26)
            m(3,1) = valres(27)
            m(2,2) = valres(28)
            m(3,2) = valres(29)
            m(3,3) = valres(30)
!
! ---   CAS DES COQUES THERMIQUES ISOTROPES
!       ===================================
        else if (phenom.eq.'THER') then
!
! ---   INTERPOLATION DE LA CONDUCTIVITE EN FONCTION DU TEMPS
! ---   ET DE LA TEMPERATURE
! ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
! ---    INACTIVE POUR LE MOMENT) :
!       -------------------------
            nbv = 2
            nomres(1) = 'RHO_CP'
            nomres(2) = 'LAMBDA'
            nbvar = 1
            call rcvalb(fami, kpg, spt, poum, zi(imate),&
                        ' ', 'THER', nbvar, nompar, valpar,&
                        nbv, nomres, valres, icodre, 1)
!
! ---   INITIALISATION DES TENSEURS DE CONDUCTIVITE MEMBRANAIRE,
! ---   TRANSVERSE ET DU TENSEUR DE CAPACITE THERMIQUE :
!       ----------------------------------------------
            do 80 l = 1, 2
                do 70 k = 1, l
                    do 60 i = 1, 3
                        do 50 j = 1, i
                            a(i,j,k,l) = zero
                            b(i,j) = zero
                            m(i,j) = zero
50                      continue
60                  continue
70              continue
80          continue
!
! ---   CAPACITE THERMIQUE :
!       ------------------
            roc = valres(1)
!
! ---   CONDUCTIVITE :
!       ------------
            lam = valres(2)
!
! ---   DEMI-EPAISSEUR :
!       --------------
            h = zr(icacoq)/2.d0
!
! ---   TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
!       -----------------------------------
            a(1,1,1,1) = 16.d0*lam*h/15.d0
            a(1,1,2,2) = a(1,1,1,1)
            a(2,2,1,1) = 4.d0*lam*h/15.d0
            a(2,2,2,2) = a(2,2,1,1)
            a(3,3,1,1) = 4.d0*lam*h/15.d0
            a(3,3,2,2) = a(2,2,1,1)
            a(2,1,1,1) = 2.d0*lam*h/15.d0
            a(2,1,2,2) = a(2,1,1,1)
            a(3,1,1,1) = 2.d0*lam*h/15.d0
            a(3,1,2,2) = a(3,1,1,1)
            a(3,2,1,1) = -lam*h/15.d0
            a(3,2,2,2) = a(3,2,1,1)
!
! ---   MATRICE DE CONDUCTIVITE TRANSVERSE :
!       ----------------------------------
            b(1,1) = 16.d0*lam/ (6.d0*h)
            b(2,1) = -8.d0*lam/ (6.d0*h)
            b(3,1) = b(2,1)
            b(2,2) = 7.d0*lam/ (6.d0*h)
            b(3,2) = lam/ (6.d0*h)
            b(3,3) = b(2,2)
            b(1,2) = b(2,1)
            b(1,3) = b(3,1)
            b(2,3) = b(3,2)
!
! ---   TENSEUR DE CAPACITE THERMIQUE ISOTROPE :
!       --------------------------------------
            m(1,1) = 16.d0*roc*h/15.d0
            m(2,1) = 2.d0*roc*h/15.d0
            m(3,1) = 2.d0*roc*h/15.d0
            m(2,2) = 4.d0*roc*h/15.d0
            m(3,2) = -roc*h/15.d0
            m(3,3) = 4.d0*roc*h/15.d0
            m(1,2) = m(2,1)
            m(1,3) = m(3,1)
            m(2,3) = m(3,2)
!
! ---   CAS DES COQUES THERMIQUES HETEROGENES HOMOGENEISEES
!       ===================================================
        else if (phenom.eq.'THER_COQUE') then
!
! ---   LES DIRECTIONS 1 ET 2 DESIGNENT CELLES DU PLAN DE LA PLAQUE
! ---   LA DIRECTION 3 EST PERPENDICULAIRE
! ---   ON ADMET QUE LE TENSEUR DE CONDUCTIVITE EN CHAQUE POINT
! ---   EST DIAGONAL ET QUE SES VALEURS PROPRES SONT
! ---   LAMBDA_1 , LAMBDA_2 ET LAMBDA_3
! ---   D'AUTRE PART, SOIENT P1(X3), P2(X3), P3(X3) LES POLYNOMES
! ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS
! ---   A L'INTERPOLATION DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
! ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
! ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
! ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
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
!
! ---   ON NE DONNE QUE 8 TERMES CAR ON A LES EGALITES SUIVANTES :
! ---   SOMME_EP(P2(X3)*P2(X3).DX3) = SOMME_EP(P3(X3)*P3(X3).DX3)
! ---   SOMME_EP(P1(X3)*P2(X3).DX3) = SOMME_EP(P1(X3)*P3(X3).DX3)
!
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
! ---   ON N'A DONNE QUE 4 TERMES CAR ON A LES EGALITES SUIVANTES :
! ---   SOMME_EP(P2'(X3)*P2'(X3).DX3) = SOMME_EP(P3'(X3)*P3'(X3).DX3)
! ---   SOMME_EP(P1'(X3)*P2'(X3).DX3) = SOMME_EP(P1'(X3)*P3'(X3).DX3)
!
! ---   POUR LE TENSEUR DE CAPACITE THERMIQUE :
!       -------------------------------------
! ---   TERME SOMME_EPAISSEUR(RHOCP*P1(X3)*P1(X3).DX3) :
            nomres(13) = 'CMAS_MM'
! ---   TERME SOMME_EPAISSEUR(RHOCP*P1(X3)*P2(X3).DX3) :
            nomres(14) = 'CMAS_MP'
! ---   TERME SOMME_EPAISSEUR(RHOCP*P2(X3)*P2(X3).DX3) :
            nomres(15) = 'CMAS_PP'
! ---   TERME SOMME_EPAISSEUR(RHOCP*P2(X3)*P3(X3).DX3) :
            nomres(16) = 'CMAS_SI'
!
! ---  ON N'A DONNE QUE 4 TERMES A CAUSE DES 2 EGALITES DONNEES
! ---  APRES LA DEFINITION DES TERMES DU TENSEUR DE CONDUCTIVITE
! ---  MEMBRANAIRE
!
! ---   INTERPOLATION DES TERMES DES TENSEURS DE CONDUCTIVITE ET DE
! ---   CAPACITE THERMIQUE EN FONCTION DU TEMPS :
!       ---------------------------------------
            nbv = 16
            nbvar = 1
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
! ---   TERMES DE MASSE :
!       ---------------
            m(1,1) = valres(13)
            m(1,2) = valres(14)
            m(1,3) = valres(14)
            m(2,2) = valres(15)
            m(2,3) = valres(16)
            m(3,3) = valres(15)
            m(2,1) = m(1,2)
            m(3,1) = m(1,3)
            m(3,2) = m(2,3)
!
        else
            call utmess('F', 'ELEMENTS3_17', sk=phenom)
        endif
!
! --- PRISE EN COMPTE DANS LE TENSEUR DE CONDUCTIVITE TRANSVERSE DES
! --- ECHANGES DES PEAUX INFERIEURE ET SUPERIEURE AVEC L'EXTERIEUR :
!     ------------------------------------------------------------
! --- CAS OU LES COEFFICIENTS D'ECHANGES SONT DES FONCTIONS :
        if (icoehf .gt. 0) then
            call elrefe_info(fami='MASS',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
            do 100 kp = 1, npg2
                k = (kp-1)*nno
                call dfdm2d(nno, kp, ipoids, idfde, coor2d,&
                            poids, dfdx, dfdy)
!
! ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
!       ------------------------------------------
                xgau = zero
                ygau = zero
                zgau = zero
                do 90 i = 1, nno
                    xgau = xgau + zr(igeom+3* (i-1))*zr(ivf+k+i-1)
                    ygau = ygau + zr(igeom+3* (i-1)+1)*zr(ivf+k+i-1)
                    zgau = zgau + zr(igeom+3* (i-1)+2)*zr(ivf+k+i-1)
90              continue
!
                valpar(2) = xgau
                valpar(3) = ygau
                valpar(4) = zgau
                nbvar = 4
                call fointe('FM', hfmoin, nbvar, nompar, valpar,&
                            hmoin, ier)
                call fointe('FM', hfplus, nbvar, nompar, valpar,&
                            hplus, ier)
100          continue
        endif
!
! ---  CONTRIBUTION AU TENSEUR DE CONDUCTIVITE TRANSVERSE B DES
! ---  ECHANGES AVEC L'EXTERIEUR
! ---             (0 0  0 )
! ---    B_ECH =  (0 H- 0 )
! ---             (0 0  H+)
!      --------------------
        b(2,2) = b(2,2) + hmoin
        b(3,3) = b(3,3) + hplus
!
! --- CALCUL DES COORDONNEES DES CONNECTIVITES DANS LE REPERE
! --- DE L'ELEMENT :
!     ------------
        call cq3d2d(nno, zr(igeom), un, zero, coor2d)
        call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! --- CALCUL DE LA RIGIDITE THERMIQUE DUE A LA PARTIE MEMBRANAIRE
! --- DU TENSEUR DE CONDUCTIVITE :
!     ==========================
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
        do 180 kp = 1, npg1
            k = (kp-1)*nno
!
! ---   DERIVEES DES FONCTIONS DE FORME ET PRODUIT JACOBIEN*POIDS
! ---   (DANS POIDS)  SUR L'ELEMENT :
!       ---------------------------
            call dfdm2d(nno, kp, ipoids, idfde, coor2d,&
                        poids, dfdx, dfdy)
            do 110 pi = 1, 3
                tpg(pi) = zero
                dtpgdx(pi) = zero
                dtpgdy(pi) = zero
110          continue
!
! ---   TEMPERATURES ET GRADIENTS THERMIQUES AUX POINTS D'INTEGRATION :
!       -------------------------------------------------------------
            do 130 gi = 1, nno
                do 120 pi = 1, 3
                    i = 3* (gi-1) + pi - 1 + itemp
                    tpg(pi) = tpg(pi) + zr(i)*zr(ivf+k+gi-1)
                    dtpgdx(pi) = dtpgdx(pi) + zr(i)*dfdx(gi)
                    dtpgdy(pi) = dtpgdy(pi) + zr(i)*dfdy(gi)
120              continue
130          continue
            do 170 gi = 1, nno
                do 160 gj = 1, gi
                    do 150 pi = 1, 3
                        do 140 pj = 1, pi
                            pk = a(pi,pj,1,1)*dfdx(gi)*dfdx(gj) + a(pi,pj,2,2)*dfdy(gi)*dfdy(gj) &
                                 &+ a(pi,pj, 1,2)*dfdx(gi)*dfdy(gj) + a(pi,pj,1,2)* dfdy(gi)*dfdx&
                                 &(gj)
!
                            if ((pi.ne.pj) .and. (gi.ne.gj)) then
                                i = 3* (gi-1) + pj
                                j = 3* (gj-1) + pi
                                rigith(i,j) = rigith(i,j) + poids*pk
                            endif
                            i = 3* (gi-1) + pi
                            j = 3* (gj-1) + pj
                            rigith(i,j) = rigith(i,j) + poids*pk
140                      continue
150                  continue
160              continue
170          continue
180      continue
!
! --- CALCUL DE LA RIGIDITE THERMIQUE DUE A LA PARTIE TRANSVERSE
! --- DU TENSEUR DE CONDUCTIVITE ET CALCUL DE LA MASSE THERMIQUE :
!     ==========================================================
!
! --- ON PREND LA SECONDE FAMILLE DE POINTS D'INTEGRATION QUI
! --- EST D'UN ORDRE PLUS ELEVE :
!     -------------------------
        call elrefe_info(fami='MASS',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
        do 230 kp = 1, npg2
            k = (kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, coor2d,&
                        poids, dfdx, dfdy)
!
            do 220 gi = 1, nno
                ivf1 = ivf + k + gi - 1
                do 210 gj = 1, gi
                    ivf2 = ivf + k + gj - 1
                    do 200 pi = 1, 3
                        do 190 pj = 1, pi
                            pk = b(pi,pj)*zr(ivf1)*zr(ivf2)
                            pm = m(pi,pj)*zr(ivf1)*zr(ivf2)
!
! ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
! ---     INFERIEURE DE LA SOUS-MATRICE :
!         -----------------------------
                            if ((pi.ne.pj) .and. (gi.ne.gj)) then
                                i = 3* (gi-1) + pj
                                j = 3* (gj-1) + pi
                                rigith(i,j) = rigith(i,j) + poids*pk
                                masse(i,j) = masse(i,j) + poids*pm
                            endif
!
! ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
! ---     DE LA SOUS-MATRICE :
!         ------------------
                            i = 3* (gi-1) + pi
                            j = 3* (gj-1) + pj
                            rigith(i,j) = rigith(i,j) + poids*pk
                            masse(i,j) = masse(i,j) + poids*pm
190                      continue
200                  continue
210              continue
220          continue
230      continue
        do 250 i = 1, 3*nno
            do 240 j = 1, i - 1
                rigith(j,i) = rigith(i,j)
                masse(j,i) = masse(i,j)
240          continue
250      continue
!
!
!..................................................................
!.    CAS DES BORDS DES COQUES SURFACIQUES                        .
!.    ILS INTERVIENNENT PAR LEUR CONTRIBUTION A L'ECHANGE LATERAL .
!..................................................................
        else if (nomte.eq.'THCOSE3' .or. nomte.eq.'THCOSE2')&
    then
        call jevete('&INEL.'//nomte(1:8)//'.DEMR', ' ', mzr)
!
!
        long = (&
               zr(igeom+3)-zr(igeom))**2 + (zr(igeom+3+1)-zr(igeom+1) )**2 + (zr(igeom+3+2)-zr(ig&
               &eom+2)&
               )**2
!
        long = sqrt(long)/2.d0
!       EP  =EP/2.D0
!
! ---   DETERMINATION DE LA 'PART' DE RIGIDITE THERMIQUE DU A L'ECHANGE
! ---   AVEC L'EXTERIEUR POUR LES COQUES LINEIQUES
! ---   ON RAPPELLE QUE LE TERME GENERIQUE DE CETTE MATRICE A POUR
! ---   EXPRESSION :
! ---   B(I,J) = SOMME_VOLUME(H*NI(X,Y,Z)*NJ(X,Y,Z).DX.DY.DZ)
! ---   SOIT
! ---   B(I,J) = SOMME_LONGUEUR (H*NI(X,Y)*NJ(X,Y).DX.DY)
! ---           *SOMME_EPAISSEUR(PK(Z)*PL(Z).DZ)
! ---   OU LES PK ET PL SONT LES FONCTIONS D'INTERPOLATION DANS
! ---   L'EPAISSEUR
! ---   PLUS EXACTEMENT P1(Z), P2(Z), P3(Z) SONT LES POLYNOMES
! ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS A L'INTERPOLATION
! ---   DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
! ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
! ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
! ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
! ---   (I.E. T(X,Y,Z) =    P1(Z)*TMOY(X,Y)
! ---                     + P2(Z)*TINF(X,Y)
! ---                     + P3(Z)*TSUP(X,Y)) :
!       ------------------------------------
        call r8inir(9, zero, matp, 1)
        call r8inir(9, zero, matn, 1)
!
! ---   DETERMINATION DE LA MATRICE MATP DONT LE TERME GENERIQUE
! ---   EST MATP(I,J) = SOMME_EPAISSEUR(PI(Z)*PJ(Z).DZ) :
!       -----------------------------------------------
        do 260 kp = 1, npg1
            kq = (kp-1)*3
!
            poi1 = zr(mzr-1+12+kp)
!
            matp(1,1) = matp(1,1) + poi1*zr(mzr-1+kq+1)**2
            matp(1,2) = matp(1,2) + poi1*zr(mzr-1+kq+1)*zr(mzr-1+kq+2)
            matp(1,3) = matp(1,3) + poi1*zr(mzr-1+kq+1)*zr(mzr-1+kq+3)
            matp(2,1) = matp(1,2)
            matp(2,2) = matp(2,2) + poi1*zr(mzr-1+kq+2)**2
            matp(2,3) = matp(2,3) + poi1*zr(mzr-1+kq+2)*zr(mzr-1+kq+3)
            matp(3,1) = matp(1,3)
            matp(3,2) = matp(2,3)
            matp(3,3) = matp(3,3) + poi1*zr(mzr-1+kq+3)**2
260      continue
!
! ---   DETERMINATION DE LA MATRICE MATN DONT LE TERME GENERIQUE
! ---   EST MATN(I,J) = SOMME_LONGUEUR (H*NI(X,Y)*NJ(X,Y).DX.DY) :
!       --------------------------------------------------------
        do 280 kp = 1, npg1
            k = (kp-1)*nno
!
            poi2 = zr(ipoids-1+kp)
!
            if (icoehf .gt. 0) then
!
! ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
!       ------------------------------------------
                xgau = zero
                ygau = zero
                zgau = zero
                do 270 i = 1, nno
                    xgau = xgau + zr(igeom+3* (i-1))*zr(ivf+k+i-1)
                    ygau = ygau + zr(igeom+3* (i-1)+1)*zr(ivf+k+i-1)
                    zgau = zgau + zr(igeom+3* (i-1)+2)*zr(ivf+k+i-1)
270              continue
!
                valpar(2) = xgau
                valpar(3) = ygau
                valpar(4) = zgau
                nbvar = 4
!
                call fointe('FM', hfbord, nbvar, nompar, valpar,&
                            hbord, ier)
            endif
!
!
!      IMPORTANT: LAMB = CONV * EPAISSEUR
!
!          LAMB=LAMB*LONG*THETA*EP
            hbord = hbord*long*theta/2.d0
!
            matn(1,1) = poi2*hbord*zr(ivf-1+k+1)**2
            matn(1,2) = poi2*hbord*zr(ivf-1+k+1)*zr(ivf-1+k+2)
            matn(2,1) = matn(1,2)
            matn(2,2) = poi2*hbord*zr(ivf-1+k+2)**2
!
            if (nomte.eq.'THCOSE3') then
                matn(1,3) = poi2*hbord*zr(ivf-1+k+1)*zr(ivf-1+k+3)
                matn(2,3) = poi2*hbord*zr(ivf-1+k+2)*zr(ivf-1+k+3)
                matn(3,1) = matn(1,3)
                matn(3,2) = matn(2,3)
                matn(3,3) = poi2*hbord*zr(ivf-1+k+3)**2
            endif
!
            rigith(1,1) = rigith(1,1) + matn(1,1)*matp(1,1)
            rigith(1,2) = rigith(1,2) + matn(1,1)*matp(1,2)
            rigith(1,3) = rigith(1,3) + matn(1,1)*matp(1,3)
            rigith(1,4) = rigith(1,4) + matn(1,2)*matp(1,1)
            rigith(1,5) = rigith(1,5) + matn(1,2)*matp(1,2)
            rigith(1,6) = rigith(1,6) + matn(1,2)*matp(1,3)
!
            rigith(2,1) = rigith(1,2)
            rigith(2,2) = rigith(2,2) + matn(1,1)*matp(2,2)
            rigith(2,3) = rigith(2,3) + matn(1,1)*matp(2,3)
            rigith(2,4) = rigith(2,4) + matn(1,2)*matp(2,1)
            rigith(2,5) = rigith(2,5) + matn(1,2)*matp(2,2)
            rigith(2,6) = rigith(2,6) + matn(1,2)*matp(2,3)
!
            rigith(3,1) = rigith(1,3)
            rigith(3,2) = rigith(2,3)
            rigith(3,3) = rigith(3,3) + matn(1,1)*matp(3,3)
            rigith(3,4) = rigith(3,4) + matn(1,2)*matp(3,1)
            rigith(3,5) = rigith(3,5) + matn(1,2)*matp(3,2)
            rigith(3,6) = rigith(3,6) + matn(1,2)*matp(3,3)
!
            rigith(4,1) = rigith(1,4)
            rigith(4,2) = rigith(2,4)
            rigith(4,3) = rigith(3,4)
            rigith(4,4) = rigith(4,4) + matn(2,2)*matp(1,1)
            rigith(4,5) = rigith(4,5) + matn(2,2)*matp(1,2)
            rigith(4,6) = rigith(4,6) + matn(2,2)*matp(1,3)
!
            rigith(5,1) = rigith(1,5)
            rigith(5,2) = rigith(2,5)
            rigith(5,3) = rigith(3,5)
            rigith(5,4) = rigith(4,5)
            rigith(5,5) = rigith(5,5) + matn(2,2)*matp(2,2)
            rigith(5,6) = rigith(5,6) + matn(2,2)*matp(2,3)
!
            rigith(6,1) = rigith(1,6)
            rigith(6,2) = rigith(2,6)
            rigith(6,3) = rigith(3,6)
            rigith(6,4) = rigith(4,6)
            rigith(6,5) = rigith(5,6)
            rigith(6,6) = rigith(6,6) + matn(2,2)*matp(3,3)
!
            if (nomte.eq.'THCOSE3') then
!
                rigith(1,7) = rigith(1,7) + matn(1,3)*matp(1,1)
                rigith(1,8) = rigith(1,8) + matn(1,3)*matp(1,2)
                rigith(1,9) = rigith(1,9) + matn(1,3)*matp(1,3)
!
                rigith(2,7) = rigith(2,7) + matn(1,3)*matp(2,1)
                rigith(2,8) = rigith(2,8) + matn(1,3)*matp(2,2)
                rigith(2,9) = rigith(2,9) + matn(1,3)*matp(2,3)
!
                rigith(3,7) = rigith(3,7) + matn(1,3)*matp(3,1)
                rigith(3,8) = rigith(3,8) + matn(1,3)*matp(3,2)
                rigith(3,9) = rigith(3,9) + matn(1,3)*matp(3,3)
!
                rigith(4,7) = rigith(4,7) + matn(2,3)*matp(1,1)
                rigith(4,8) = rigith(4,8) + matn(2,3)*matp(1,2)
                rigith(4,9) = rigith(4,9) + matn(2,3)*matp(1,3)
!
                rigith(5,7) = rigith(5,7) + matn(2,3)*matp(2,1)
                rigith(5,8) = rigith(5,8) + matn(2,3)*matp(2,2)
                rigith(5,9) = rigith(5,9) + matn(2,3)*matp(2,3)
!
                rigith(6,7) = rigith(6,7) + matn(2,3)*matp(3,1)
                rigith(6,8) = rigith(6,8) + matn(2,3)*matp(3,2)
                rigith(6,9) = rigith(6,9) + matn(2,3)*matp(3,3)
!
                rigith(7,1) = rigith(1,7)
                rigith(7,2) = rigith(2,7)
                rigith(7,3) = rigith(3,7)
                rigith(7,4) = rigith(4,7)
                rigith(7,5) = rigith(5,7)
                rigith(7,6) = rigith(6,7)
                rigith(7,7) = rigith(7,7) + matn(3,3)*matp(1,1)
                rigith(7,8) = rigith(7,8) + matn(3,3)*matp(1,2)
                rigith(7,9) = rigith(7,9) + matn(3,3)*matp(1,3)
!
                rigith(8,1) = rigith(1,8)
                rigith(8,2) = rigith(2,8)
                rigith(8,3) = rigith(3,8)
                rigith(8,4) = rigith(4,8)
                rigith(8,5) = rigith(5,8)
                rigith(8,6) = rigith(6,8)
                rigith(8,7) = rigith(7,8)
                rigith(8,8) = rigith(8,8) + matn(3,3)*matp(2,2)
                rigith(8,9) = rigith(8,9) + matn(3,3)*matp(2,3)
!
                rigith(9,1) = rigith(1,9)
                rigith(9,2) = rigith(2,9)
                rigith(9,3) = rigith(3,9)
                rigith(9,4) = rigith(4,9)
                rigith(9,5) = rigith(5,9)
                rigith(9,6) = rigith(6,9)
                rigith(9,7) = rigith(7,9)
                rigith(9,8) = rigith(8,9)
                rigith(9,9) = rigith(9,9) + matn(3,3)*matp(3,3)
            endif
!
280      continue
!
!..................................................................
    endif
!..................................................................
!
! --- RECUPERATION DU VECTEUR SECOND MEMBRE EN SORTIE DE CHAR_THER_EVOL:
!     ----------------------------------------------------------------
    call jevech('PVECTTR', 'E', ivectt)
!
! --- AFFECTATION DU VECTEUR SECOND MEMBRE EN SORTIE DE
! --- CHAR_THER_EVOL :
!     --------------
    nbddl = 3*nno
    do 300 i = 1, nbddl
        do 290 j = 1, nbddl
            zr(ivectt+i-1) = zr(ivectt+i-1) + (masse(j,i)/deltat- (un- theta)*rigith(j,i))* zr(it&
                             &emp+j-1)
290      continue
300  end do
!
end subroutine
