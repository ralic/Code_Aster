subroutine dxbmat(nomte, cara, xyzl, pgl, igau,&
                  jacgau, bmat)
    implicit  none
#include "asterfort/dkqb.h"
#include "asterfort/dktb.h"
#include "asterfort/dsqb.h"
#include "asterfort/dstb.h"
#include "asterfort/q4gb.h"
#include "asterfort/t3gb.h"
#include "asterfort/u2mesk.h"
    integer :: igau
    real(kind=8) :: xyzl(3, *), pgl(3, *), bmat(8, *), jacgau, cara(*)
    character(len=16) :: nomte
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
!     ------------------------------------------------------------------
! --- CALCUL DE LA MATRICE (B) RELIANT LES DEFORMATIONS DU PREMIER
! --- ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION D'INDICE IGAU
! --- POUR LES ELEMENTS : DST, DKT, DSQ, DKQ, Q4G
! --- (I.E. (EPS_1) = (B)*(UN))
! --- D'AUTRE_PART, ON CALCULE LE PRODUIT NOTE JACGAU = JACOBIEN*POIDS
!     ------------------------------------------------------------------
!     IN  NOMTE         : NOM DU TYPE D'ELEMENT
!     IN  XYZL(3,NBNO)  : COORDONNEES DES CONNECTIVITES DE L'ELEMENT
!                         DANS LE REPERE LOCAL DE L'ELEMENT
!     IN  PGL(3,3)      : MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE
!                         LOCAL
!     IN  IGAU          : INDICE DU POINT D'INTEGRATION
!     OUT JACGAU        : PRODUIT JACOBIEN*POIDS AU POINT D'INTEGRATION
!                         COURANT
!     OUT BMAT(6,1)     : MATRICE (B) AU POINT D'INTEGRATION COURANT
!     ------------------------------------------------------------------
!
    if (nomte .eq. 'MEDKTR3 ' .or. nomte .eq. 'MEDKTG3 ') then
        call dktb(cara, igau, jacgau, bmat)
!
    else if (nomte .eq.'MEDSTR3 ') then
        call dstb(cara, pgl, igau, jacgau, bmat)
!
    else if (nomte .eq.'MEDKQU4 ' .or. nomte .eq.'MEDKQG4 ') then
        call dkqb(cara, xyzl, igau, jacgau, bmat)
!
    else if (nomte .eq.'MEDSQU4 ') then
        call dsqb(cara, xyzl, pgl, igau, jacgau,&
                  bmat)
!
    else if (nomte .eq.'MEQ4QU4 '.or.nomte .eq.'MEQ4GG4 ') then
        call q4gb(cara, xyzl, igau, jacgau, bmat)
!
    else if (nomte .eq.'MET3GG3 '.or.nomte .eq.'MET3TR3 ') then
        call t3gb(cara, xyzl, bmat)
        jacgau = cara(8)
!
    else
        call u2mesk('F', 'ELEMENTS_14', 1, nomte(1:8))
    endif
!
end subroutine
