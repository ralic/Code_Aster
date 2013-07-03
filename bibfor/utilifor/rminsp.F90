function rminsp(x1, x2, x3, x4, x5)
    implicit   none
#include "asterfort/assert.h"
    real(kind=8) :: rminsp, x1, x2, x3, x4, x5, xmin
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!      CALCULER LE "MIN" DE 5 VALEURS >=0. EN NE TENANT
!      COMPTE QUE DES VALEURS NON NULLES
!
! IN  : X1,X2,X3,X4,X5 : 5 REELS >= 0.D0
! OUT : RMINSP : LE "MIN" DES X1, ..., X5 NON NULS
! REMARQUE : X1 DOIT ETRE > 0
!     ------------------------------------------------------------------
!
! DEB------------------------------------------------------------------
    call assert(x1.gt.0.d0)
    call assert(x2.ge.0.d0)
    call assert(x3.ge.0.d0)
    call assert(x4.ge.0.d0)
    call assert(x5.ge.0.d0)
!
    xmin=x1
    if (x2 .gt. 0.d0 .and. x2 .lt. xmin) xmin=x2
    if (x3 .gt. 0.d0 .and. x3 .lt. xmin) xmin=x3
    if (x4 .gt. 0.d0 .and. x4 .lt. xmin) xmin=x4
    if (x5 .gt. 0.d0 .and. x5 .lt. xmin) xmin=x5
!
    rminsp=xmin
!
end function
