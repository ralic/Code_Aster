subroutine dxefgm(nomte, option, xyzl, pgl, depl,&
                  effg)
    implicit  none
#include "asterfort/dkqedg.h"
#include "asterfort/dktedg.h"
#include "asterfort/dsqedg.h"
#include "asterfort/dstedg.h"
#include "asterfort/q4gedg.h"
#include "asterfort/t3gedg.h"
#include "asterfort/u2mesk.h"
    real(kind=8) :: xyzl(3, 1), pgl(3, 1), depl(1), effg(1)
    character(len=16) :: nomte, option
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
! --- EFFORTS GENERALISES D'ORIGINE MECANIQUE AUX POINTS DE CALCUL
! --- POUR LES ELEMENTS COQUES A FACETTES PLANES :
! --- DST, DKT, DSQ, DKQ, Q4G
!     ------------------------------------------------------------------
!     IN  NOMTE        : NOM DU TYPE D'ELEMENT
!     IN  OPTION       : NOM DE L'OPTION
!     IN  XYZL(3,NNO)  : COORDONNEES DES CONNECTIVITES DE L'ELEMENT
!                        DANS LE REPERE LOCAL DE L'ELEMENT
!     IN  PGL(3,3)     : MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE
!                        LOCAL
!     IN  DEPL(1)      : VECTEUR DES DEPLACEMENTS AUX NOEUDS
!     OUT EFFG(1)      : EFFORTS  GENERALISES D'ORIGINE MECANIQUE
!                        AUX POINTS DE CALCUL
!     ------------------------------------------------------------------
    integer :: multic
!     ------------------------------------------------------------------
!
    if (nomte .eq. 'MEDKTR3 ' .or. nomte .eq. 'MEDKTG3 ') then
        call dktedg(xyzl, option, pgl, depl, effg,&
                    multic)
!
    else if (nomte.eq.'MEDSTR3 ') then
        call dstedg(xyzl, option, pgl, depl, effg)
!
    else if (nomte.eq.'MEDKQU4 '.or. nomte.eq.'MEDKQG4 ') then
        call dkqedg(xyzl, option, pgl, depl, effg)
!
    else if (nomte.eq.'MEDSQU4 ') then
        call dsqedg(xyzl, option, pgl, depl, effg)
!
    else if (nomte.eq.'MEQ4QU4 '.or. nomte.eq.'MEQ4GG4') then
        call q4gedg(xyzl, option, pgl, depl, effg)
!
    else if (nomte.eq.'MET3TR3 '.or. nomte.eq.'MET3GG3') then
        call t3gedg(xyzl, option, pgl, depl, effg)
!
    else
        call u2mesk('F', 'ELEMENTS_14', 1, nomte(1:8))
    endif
!
end subroutine
