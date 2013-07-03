subroutine proqua(quat1, quat2, quat3)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/provec.h"
#include "blas/ddot.h"
    real(kind=8) :: quat1(4), quat2(4), quat3(4)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (QUATERNION)
!
! CALCULE LE PRODUIT DES QUATERNIONS QUAT1 PAR QUAT2 ET MET LE
!  RESULTAT DANS QUAT3
!
! ----------------------------------------------------------------------
!
!
! IN  QUAT1  : PREMIER QUATERNION
! IN  QUAT2  : SECOND QUATERNION
! OUT QUAT3  : RESULTAT DU PRODUIT DES DEUX QUATRERNIONS
!
!
! ------------------------------------------------------------------
!
    real(kind=8) :: prosca
    integer :: i
!
! ------------------------------------------------------------------
!
    call provec(quat1, quat2, quat3)
    prosca = ddot(3,quat1,1,quat2,1)
    quat3(4) = quat1(4) * quat2(4) - prosca
    do 1 i = 1, 3
        quat3(i) = quat3(i) + quat1(4)*quat2(i) + quat2(4)*quat1(i)
 1  end do
!
end subroutine
