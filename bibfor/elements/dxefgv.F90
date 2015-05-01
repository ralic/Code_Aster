subroutine dxefgv(nomte, option, xyzl, pgl, depl, effgt)
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
#include "asterfort/dxefg2.h"
#include "asterfort/dxefgm.h"
#include "asterfort/dxefgt.h"
#include "asterfort/dxefn2.h"
#include "asterfort/dxefnt.h"
    character(len=16) :: nomte
    character(len=*) :: option
    real(kind=8) :: xyzl(3, 1), pgl(3, 3)
    real(kind=8) :: depl(1)
    real(kind=8) :: effgt(1)
!     ------------------------------------------------------------------
! --- EFFORTS GENERALISES 'VRAIS' (I.E. EFFOR_MECA - EFFOR_THERM)
! --- AUX POINTS D'INTEGRATION POUR LES ELEMENTS COQUES A
! --- FACETTES PLANES : DST, DKT, DSQ, DKQ, Q4G
!     ------------------------------------------------------------------
!     IN  NOMTE        : NOM DU TYPE D'ELEMENT
!     IN  OPTION       : NOM DE L'OPTION
!     IN  XYZL(3,NNO)  : COORDONNEES DES CONNECTIVITES DE L'ELEMENT
!                        DANS LE REPERE LOCAL DE L'ELEMENT
!     IN  PGL(3,3)     : MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE
!                        LOCAL
!     IN  DEPL(1)      : VECTEUR DES DEPLACEMENTS AUX NOEUDS
!     IN  TSUP(1)      : TEMPERATURES AUX NOEUDS DU PLAN SUPERIEUR
!                        DE LA COQUE
!     IN  TINF(1)      : TEMPERATURES AUX NOEUDS DU PLAN INFERIEUR
!                        DE LA COQUE
!     IN  TMOY(1)      : TEMPERATURES AUX NOEUDS DU PLAN MOYEN
!                        DE LA COQUE
!     OUT EFFGT(1)     : EFFORTS  GENERALISES VRAIS AUX POINTS
!                        D'INTEGRATION (I.E.
!                           EFFORTS_MECA - EFFORTS_THERM)
!
    real(kind=8) :: sigth(32)
!
! --- CALCUL DES EFFORTS GENERALISES D'ORIGINE MECANIQUE
! --- AUX POINTS DE CALCUL
!     --------------------
!-----------------------------------------------------------------------
    integer :: i
    character(len=16) :: opti16
!
    opti16=option
!
    call dxefgm(nomte, opti16, xyzl, pgl, depl, effgt)
!
! --- CALCUL DES EFFORTS GENERALISES D'ORIGINE THERMIQUE
! --- AUX POINTS DE CALCUL
!     --------------------
! ---     POINTS D'INTEGRATION
    if (option(8:9) .eq. 'GA') then
        if (nomte .eq. 'MEDKQG4' .or. nomte .eq. 'MEDKTG3') then
            call dxefg2(pgl, sigth)
        else
            call dxefgt(pgl, sigth)
        endif
! ---     POINTS DE CALCUL
    else if (option(8:9).eq.'NO') then
        if (nomte .eq. 'MEDKQG4' .or. nomte .eq. 'MEDKTG3') then
            call dxefn2(nomte, pgl, sigth)
        else
            call dxefnt(nomte, pgl, sigth)
        endif
    endif
!
! --- CALCUL DES EFFORTS GENERALISES 'VRAIS'
! --- AUX POINTS DE CALCUL
!     --------------------
    do i = 1, 32
        effgt(i) = effgt(i) - sigth(i)
    end do
!
end subroutine
