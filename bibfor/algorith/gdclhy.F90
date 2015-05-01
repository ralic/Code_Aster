subroutine gdclhy(je, e)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/gdsmhy.h"
    real(kind=8) :: je, e(6)
!
! ----------------------------------------------------------------------
!            GRANDES DEFORMATIONS CANO-LORENTZ
!         CORRECTION HYDROSTATIQUE DE LA DEFORMATION ELASTIQUE
! ----------------------------------------------------------------------
! IN  JE      DETERMINANT DE E CIBLE
! VAR E       DEFORMATION ELASTIQUE (XX,YY,ZZ,RAC2*XY,RAC2*XZ,RAC2*YZ)
! ----------------------------------------------------------------------
    call gdsmhy(je, e)
end subroutine
