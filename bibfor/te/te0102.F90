subroutine te0102(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'MASS_THER      '
!                          CAS COQUE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: i, icacoq, ind, j, jgano, nbddl, nbres
    integer :: nbv, nbvar, ndimax
    real(kind=8) :: rocp, un, undemi
!-----------------------------------------------------------------------
    parameter (ndimax=27)
    parameter (nbres=6)
    parameter (nbvar=2)
    integer :: icodre(nbres)
    character(len=2) :: num
    character(len=8) :: nomres(nbres), nompar(nbvar), fami, poum
    character(len=16) :: phenom
    real(kind=8) :: m(3, 3), h
    real(kind=8) :: valres(nbres)
    real(kind=8) :: coor2d(18)
    real(kind=8) :: dfdx(9), dfdy(9), poids, pm, deltat
    real(kind=8) :: mun, zero, deux, quatre
    real(kind=8) :: quinze, seize, cour, cosa, sina, r
    real(kind=8) :: valpar(nbvar), tempe, instan
    real(kind=8) :: masse(ndimax, ndimax)
    integer :: nno, kp, npg2, gi, pi, gj, pj, k, imattt
    integer :: ipoids, ivf, idfde, igeom, kpg, spt
    integer :: imate, itemps, nnos, ndim
!
!
    if (nomte .ne. 'THCPSE3 ' .and. nomte .ne. 'THCASE3 ') then
        call elref4(' ', 'MASS', ndim, nno, nnos,&
                    npg2, ipoids, ivf, idfde, jgano)
    else
        call elref4(' ', 'RIGI', ndim, nno, nnos,&
                    npg2, ipoids, ivf, idfde, jgano)
    endif
!
!
! --- INITIALISATIONS :
!     ---------------
    mun = -1.0d0
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
    deux = 2.0d0
    quatre = 4.0d0
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
    do 20 i = 1, 3
        do 10 j = 1, 3
            m(i,j) = zero
10      continue
20  end do
!
    do 40 i = 1, ndimax
        do 30 j = 1, ndimax
            masse(i,j) = zero
30      continue
40  end do
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
! --- RECUPERATION DE L'EPAISSEUR DE LA COQUE :
!     ---------------------------------------
    call jevech('PCACOQU', 'L', icacoq)
!
! --- RECUPERATION DE L'INSTANT DU CALCUL ET DU PAS DE TEMPS :
!     ------------------------------------------------------
    call jevech('PTEMPSR', 'L', itemps)
    valpar(1) = zr(itemps)
    deltat = zr(itemps+1)
!
! --- RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM :
!     -------------------------------------------------
    call rccoma(zi(imate), 'THER', 1, phenom, icodre(1))
!
! --- DETERMINATION DU TENSEUR DE CAPACITE THERMIQUE :
!     ==============================================
!
! --- CAS DES COQUES THERMIQUES MULTI-COUCHES :
!     ---------------------------------------
    if (phenom .eq. 'THER_COQMU') then
!
! ---   NOM DES COMPOSANTES DU TENSEUR DE CAPACITE
! ---   THERMIQUE HOMOGENEISE :
!       ---------------------
        do 50 i = 1, nbres
            call codent(i+24, 'G', num)
            nomres(i) = 'HOM_'//num
50      continue
!
! ---   INTERPOLATION DES TERMES DU TENSEUR DE CAPACITE THERMIQUE
! ---   EN FONCTION DU TEMPS ET DE LA TEMPERATURE
! ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
! ---    INACTIVE POUR LE MOMENT) :
!       -------------------------
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THER_COQMU', nbvar, nompar, valpar,&
                    nbres, nomres, valres, icodre, 1)
!
! ---   TENSEUR DE CAPACITE THERMIQUE :
!       -----------------------------
        m(1,1) = valres(1)
        m(2,1) = valres(2)
        m(3,1) = valres(3)
        m(2,2) = valres(4)
        m(3,2) = valres(5)
        m(3,3) = valres(6)
!
! --- CAS DES COQUES THERMIQUES ISOTROPES :
!     ===================================
    else if (phenom.eq.'THER') then
!
! ---   INTERPOLATION DE LA CAPACITE THERMIQUE EN FONCTION DU TEMPS
! ---   ET DE LA TEMPERATURE
! ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
! ---    INACTIVE POUR LE MOMENT) :
!       -------------------------
        nbv = 1
        nomres(1) = 'RHO_CP'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THER', nbvar, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
! ---   CAPACITE THERMIQUE :
!       ------------------
        rocp = valres(1)
!
! ---   DEMI-EPAISSEUR  :
!       --------------
        h = undemi*zr(icacoq)
!
! ---   TENSEUR DE CAPACITE THERMIQUE :
!       -----------------------------
        m(1,1) = seize*rocp*h/quinze
        m(2,1) = deux*rocp*h/quinze
        m(3,1) = deux*rocp*h/quinze
        m(2,2) = quatre*rocp*h/quinze
        m(3,2) = mun*rocp*h/quinze
        m(3,3) = quatre*rocp*h/quinze
!
! --- CAS DES COQUES THERMIQUES HETEROGENES :
!     -------------------------------------
    else if (phenom.eq.'THER_COQUE') then
!
! ---   NOM DES COMPOSANTES DU TENSEUR DE CAPACITE
! ---   THERMIQUE HOMOGENEISE
! ---   EN NOTANT RHOCP LA CAPACITE THERMIQUE EN CHAQUE POINT
! ---             P1(X3), P2(X3), P3(X3) LES POLYNOMES
! ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS A L'INTERPOLATION
! ---   DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
! ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
! ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
! ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
! ---   (I.E. T(X1,X2,X3) =    P1(X3)*TMOY(X1,X2)
! ---                        + P2(X3)*TINF(X1,X2)
! ---                        + P3(X3)*TSUP(X1,X2)
! ---   LES TERMES DU TENSEUR DE CAPACITE THERMIQUE HOMOGENEISE
! ---   SONT ALORS :
!       ----------
! ---   TERME SOMME_EPAISSEUR(RHOCP*P1(X3)*P1(X3).DX3) :
        nomres(1) = 'CMAS_MM'
! ---   TERME SOMME_EPAISSEUR(RHOCP*P1(X3)*P2(X3).DX3) :
        nomres(2) = 'CMAS_MP'
! ---   TERME SOMME_EPAISSEUR(RHOCP*P2(X3)*P2(X3).DX3) :
        nomres(3) = 'CMAS_PP'
! ---   TERME SOMME_EPAISSEUR(RHOCP*P2(X3)*P3(X3).DX3) :
        nomres(4) = 'CMAS_SI'
!
! ---   INTERPOLATION DES COMPOSANTES DU TENSEUR DE CAPACITE THERMIQUE
! ---   EN FONCTION DU TEMPS ET DE LA TEMPERATURE
! ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
! ---    INACTIVE POUR LE MOMENT) :
!       -------------------------
        nbv = 4
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', phenom, nbvar, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
        m(1,1) = valres(1)
        m(1,2) = valres(2)
        m(1,3) = valres(2)
        m(2,2) = valres(3)
        m(2,3) = valres(4)
        m(3,3) = valres(3)
        m(2,1) = m(1,2)
        m(3,1) = m(1,3)
        m(3,2) = m(2,3)
!
    else
        call utmess('F', 'ELEMENTS3_17', sk=phenom)
    endif
!
!
!===================================
! --- CALCUL DE LA MASSE THERMIQUE =
!===================================
!
! --- CAS DES COQUES SURFACIQUES :
!     --------------------------
    if (nomte.ne.'THCPSE3' .and. nomte.ne.'THCASE3') then
!
! --- DETERMINATION DES COORDONNEES COOR2D DES NOEUDS DE L'ELEMENT
! --- DANS LE REPERE DE L'ELEMENT :
!     ---------------------------
        call cq3d2d(nno, zr(igeom), un, zero, coor2d)
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
        do 100 kp = 1, npg2
            k = (kp-1)*nno
!
! ---   DERIVEES DES FONCTIONS DE FORME ET PRODUIT JACOBIEN*POIDS
! ---   (DANS POIDS)  SUR L'ELEMENT :
!       ---------------------------
            call dfdm2d(nno, kp, ipoids, idfde, coor2d,&
                        poids, dfdx, dfdy)
            do 90 gi = 1, nno
                do 80 gj = 1, gi
                    do 70 pi = 1, 3
                        do 60 pj = 1, pi
                            pm = m(pi,pj)*zr(ivf+k+gi-1)*zr(ivf+k+gj- 1)*poids
!
! ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
! ---     INFERIEURE DE LA SOUS-MATRICE :
!         -----------------------------
                            if ((pi.ne.pj) .and. (gi.ne.gj)) then
                                i = 3* (gi-1) + pj
                                j = 3* (gj-1) + pi
                                masse(i,j) = masse(i,j) + pm
                            endif
!
! ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
! ---     DE LA SOUS-MATRICE :
!         ------------------
                            i = 3* (gi-1) + pi
                            j = 3* (gj-1) + pj
                            masse(i,j) = masse(i,j) + pm
60                      continue
70                  continue
80              continue
90          continue
100      continue
!
    else
!
! --- CAS DES COQUES LINEIQUES :
!     ------------------------
!
! ---  BOUCLE SUR LES POINTS D'INTEGRATION :
!      -----------------------------------
        do 160 kp = 1, npg2
            k = (kp-1)*nno
            call dfdm1d(nno, zr(ipoids+kp-1), zr(idfde+k), zr(igeom), dfdx,&
                        cour, poids, cosa, sina)
!
            if (nomte .eq. 'THCASE3') then
                r = zero
                do 110 i = 1, nno
                    r = r + zr(igeom+2*i-2)*zr(ivf+k+i-1)
110              continue
                poids = poids*r
            endif
!
            do 150 gi = 1, nno
                do 140 gj = 1, gi
                    do 130 pi = 1, 3
                        do 120 pj = 1, pi
                            pm = m(pi,pj)*zr(ivf+k+gi-1)*zr(ivf+k+gj- 1)*poids
!
! ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
! ---     INFERIEURE DE LA SOUS-MATRICE :
!         -----------------------------
                            if ((pi.ne.pj) .and. (gi.ne.gj)) then
                                i = 3* (gi-1) + pj
                                j = 3* (gj-1) + pi
                                masse(i,j) = masse(i,j) + pm
                            endif
!
! ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
! ---     DE LA SOUS-MATRICE :
!         ------------------
                            i = 3* (gi-1) + pi
                            j = 3* (gj-1) + pj
                            masse(i,j) = masse(i,j) + pm
120                      continue
130                  continue
140              continue
150          continue
160      continue
!
    endif
!
! --- RECUPERATION DE LA MATRICE DE MASSE THERMIQUE EN SORTIE DU TE :
!     -------------------------------------------------------------
    call jevech('PMATTTR', 'E', imattt)
!
! --- AFFECTATION DE LA MATRICE DE MASSE THERMIQUE EN SORTIE DU TE :
!     ------------------------------------------------------------
    nbddl = 3*nno
    ind = 0
    do 180 i = 1, nbddl
        do 170 j = 1, i
            ind = ind + 1
            zr(imattt+ind-1) = masse(i,j)/deltat
170      continue
180  end do
!
end subroutine
