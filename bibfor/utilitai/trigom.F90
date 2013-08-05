function trigom(fonc, x)
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
! person_in_charge: jacques.pellet at edf.fr
! aslint: disable=
    implicit none
    real(kind=8) :: trigom
#include "asterfort/assert.h"
#include "asterfort/u2mess.h"
    character(len=4) :: fonc
! ----------------------------------------------------------------------
!  BUT : CALCULER ASIN(X) OU ACOS(X) SANS RISQUER DE SE PLANTER SI
!        X SORT LEGEREMENT DE L'INTERVALLE (-1,1) (TOLERANCE 1.D-12)
!
!    IN:
!       FONC    K4 : /'ASIN'   /'ACOS'   : FONCTION A EVALUER
!       X       R8 : NOMBRE DONT ON CHERCHE ARC-SINUS (OU ARC-COSINUS)
!    OUT:
!       TRIGOM  R8 : ARC-SINUS OU (ARC-COSINUS) DE X
!
! ----------------------------------------------------------------------
    real(kind=8) :: x, eps, x2
    eps = 1.d-12
!
    if ((x.gt.1.d0+eps) .or. (x.lt.-1.d0-eps)) call u2mess('F', 'UTILITAI5_50')
!
    x2 = x
    if (x .gt. 1.d0) x2 = 1.d0
    if (x .lt. -1.d0) x2 = -1.d0
!
    ASSERT(fonc.eq.'ASIN' .or. fonc.eq.'ACOS')
    if (fonc .eq. 'ASIN') then
        trigom = asin(x2)
    else
        trigom = acos(x2)
    endif
!
end function
