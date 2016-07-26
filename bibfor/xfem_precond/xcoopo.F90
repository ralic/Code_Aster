subroutine xcoopo(lsng, lstg, rg, tg, l_not_zero)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
!-----------------------------------------------------------------------
! BUT : CALCULER LES COORDONNEES CYLINDRIQUE GRACE AUX LEVEL-SETS
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!-----------------------------------------------------------------------
    real(kind=8) :: lsng, lstg, rg, tg
    aster_logical, optional :: l_not_zero
!-----------------------------------------------------------------------
!
    if (present(l_not_zero)) l_not_zero=.true.
    rg=sqrt(lsng**2+lstg**2)
!
    if (rg .gt. r8prem()) then
!       LE POINT N'EST PAS SUR LE FOND DE FISSURE
      tg = sign(1.d0,lsng) * abs(atan2(lsng,lstg))
    else
!       LE POINT EST SUR LE FOND DE FISSURE :
!       L'ANGLE N'EST PAS DÉFINI, ON LE MET À ZÉRO
!       ON NE FERA PAS LE CALCUL DES DÉRIVÉES
      tg=0.d0
      if (present(l_not_zero)) l_not_zero=.false.
    endif
!
end subroutine
