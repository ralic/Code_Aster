subroutine rc32rt(ze200, pi, pj, simpij)
    implicit   none
#include "jeveux.h"
#include "asterfort/jeveuo.h"
    real(kind=8) :: pi, pj, simpij
    aster_logical :: ze200
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     ROCHET THERMIQUE : CALCUL DE LA CONTRAINTE DE MEMBRANE DE PRESSION
!
!     ------------------------------------------------------------------
!
    real(kind=8) :: s1, s2, rayon, ep
    integer :: jvalin

! DEB ------------------------------------------------------------------
!
    simpij=0.d0
    s1=0.d0
    s2=0.d0
!
! --- CONTRAINTE MOYENNE DUE A LA PRESSION : partie ze200a
    if (ze200) then
        call jeveuo('&&RC3200.INDI', 'L', jvalin)
        rayon = zr(jvalin+6) 
        ep = zr(jvalin+7) 
        s1 = rayon*abs(pi)/ep
        s2 = rayon*abs(pj)/ep
    endif
!
    simpij = max(s1,s2)
!
end subroutine
