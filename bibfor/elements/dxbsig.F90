subroutine dxbsig(nomte, xyzl, pgl, sigma, bsigma,&
                  option)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/btsir.h"
#include "asterfort/btsig.h"
#include "asterfort/dxbmat.h"
#include "asterfort/gquad4.h"
#include "asterfort/gtria3.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvlg.h"
    character(len=16) :: nomte, option
    real(kind=8) :: xyzl(3, 1), pgl(3, 3)
    real(kind=8) :: sigma(1), bsigma(1)
!     ------------------------------------------------------------------
! --- CALCUL DES FORCES INTERNES B*SIGMA AUX NOEUDS DE L'ELEMENT
! --- DUES AU CHAMP DE CONTRAINTES SIGMA DEFINI AUX POINTS
! --- D'INTEGRATION POUR LES ELEMENTS : DST, DKT, DSQ, DKQ, Q4G
!     ------------------------------------------------------------------
!     IN  NOMTE         : NOM DU TYPE D'ELEMENT
!     IN  XYZL(3,NNO)   : COORDONNEES DES CONNECTIVITES DE L'ELEMENT
!                         DANS LE REPERE LOCAL DE L'ELEMENT
!     IN  PGL(3,3)      : MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE
!                         LOCAL
!     IN  SIGMA(1)      : CONTRAINTES GENERALISEES DEFINIES AUX POINTS
!                         D'INTEGRATION DE L'ELEMENT
!     OUT BSIGMA(1)     : FORCES INTERNES AUX NOEUDS DE L'ELEMENT
!-----------------------------------------------------------------------
    integer :: i, igau, j, lgligb, nbsig, nno, npg
!
    real(kind=8) :: bsivar, zero
!-----------------------------------------------------------------------
    parameter     (nbsig   = 8)
    parameter     (lgligb  = 24)
!
    real(kind=8) :: bmat(nbsig, lgligb)
    real(kind=8) :: bsiloc(lgligb), jacgau, cara(25)
!     ------------------------------------------------------------------
!
    zero = 0.0d0
!
    if (nomte .eq. 'MEDKTR3 ' .or. nomte .eq. 'MEDSTR3 ' .or. nomte .eq. 'MEDKTG3 ' .or.&
        nomte .eq. 'MET3GG3 ' .or. nomte .eq. 'MET3TR3 ') then
        npg = 3
        nno = 3
        if (nomte .eq. 'MET3GG3 ' .or. nomte .eq. 'MET3TR3 ') npg = 1
!
! ---- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE
!      -------------------------------------------------
        call gtria3(xyzl, cara)
!
        elseif (nomte .eq.'MEDKQU4 '.or.nomte .eq.'MEDSQU4 ' .or.nomte&
    .eq.'MEQ4QU4 '.or.nomte .eq.'MEDKQG4 ' .or.nomte .eq.'MEQ4GG4 ')&
    then
        npg = 4
        nno = 4
!
! ---- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE
!      ---------------------------------------------------
        call gquad4(xyzl, cara)
!
    else
        call utmess('F', 'ELEMENTS_14', sk=nomte(1:8))
    endif
!
! --- INITIALISATIONS :
!     -----------------
    do 10 i = 1, lgligb
        bsiloc(i) = zero
        bsigma(i) = zero
        do 20 j = 1, nbsig
            bmat(j,i) = zero
20      continue
10  end do
!
! --- CALCUL DE SOMME_ELEMENT(BT_SIGMA) :
! ---  BOUCLE SUR LES POINTS D'INTEGRATION
!      -----------------------------------
    do 30 igau = 1, npg
!
!  --      CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS DU
!  --      PREMIER ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION
!  --      COURANT : (EPS_1) = (B)*(UN)
!          ----------------------------
!
        call dxbmat(nomte, cara, xyzl, pgl, igau,&
                    jacgau, bmat)
!
!  --        CALCUL DU PRODUIT (BT)*(SIGMA)*JACOBIEN*POIDS
!          ---------------------------------------------
!
        if (option .eq. 'FORC_NODA') then
            call btsig(lgligb, nbsig, jacgau, bmat, sigma(1+8*(igau-1)),&
                       bsiloc)
        elseif (option .eq. 'REFE_FORC_NODA') then
            call btsir(lgligb, nbsig, jacgau, bmat, sigma(1+8*(igau-1)),&
                       bsiloc)
        else
            ASSERT(.false.)
        endif
30  end do
!
! --- PERMUTATION DES COMPOSANTES EN BETA_X ET  BETA_Y
! ---                             EN TETA_Y ET -TETA_X
!     -----------------------------------------------
    do 40 i = 1, nno
        bsivar = bsiloc(4+6*(i-1))
        bsiloc(4+6*(i-1)) = -bsiloc(5+6*(i-1))
        bsiloc(5+6*(i-1)) = bsivar
40  end do
!
! --- PASSAGE DU VECTEUR(BT_SIGMA) DU REPERE LOCAL AU REPERE GLOBAL :
! --- (LE RESULTAT EST ICI LE VECTEUR BSIGMA)
!      --------------------------------------
    call utpvlg(nno, 6, pgl, bsiloc, bsigma)
!
end subroutine
