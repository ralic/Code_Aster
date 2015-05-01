subroutine vpri2d(sig, sigi)
! ......................................................................
! .  - FONCTION REALISEE:  CALCUL DU MAX DES VALEURS PROPRES D'UN      .
    implicit none
! .         TENSEUR DE TYPE CONTRAINTE/DEFORMATION 2D                  .
! .  - ARGUMENTS:                                                      .
! .      DONNEES:          SIG      -->                                .
! .      RESULTATS:       SIGI     <--                                .
! ......................................................................
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
#include "jeveux.h"
    real(kind=8) :: sig(4), sigi
    real(kind=8) :: s, sp, al1, al2, delta, sqd
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    s = sig(1)+sig(2)
    sp= sig(1)-sig(2)
    delta=sp**2+4.d0*sig(4)**2
    sqd = sqrt(delta)
    al1 = (s+sqd)/2.d0
    al2 = (s-sqd)/2.d0
    sigi = max(al1,al2,sig(3),0.d0)
end subroutine
