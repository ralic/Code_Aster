subroutine mmtang(ndim, nno, coorma, dff, tau1,&
                  tau2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/assert.h"
    integer :: ndim, nno
    real(kind=8) :: coorma(27)
    real(kind=8) :: dff(2, 9)
    real(kind=8) :: tau1(3), tau2(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
!
! CALCULE LES VECTEURS TANGENTS LOCAUX SUR UNE MAILLE
!
! ----------------------------------------------------------------------
!
!
! IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
! IN  DFF    : DERIVEES PREMIERES DES FONCTIONS DE FORME
! I/O TAU1   : PREMIER VECTEUR TANGENT EN (KSI1,KSI2)
! I/O TAU2   : SECOND VECTEUR TANGENT EN (KSI1,KSI2)
!
! ----------------------------------------------------------------------
!
    integer :: idim, ino
!
! ----------------------------------------------------------------------
!
!
! --- VERIF CARACTERISTIQUES DE LA MAILLE
!
    if (nno .gt. 9) ASSERT(.false.)
    if (ndim .gt. 3) ASSERT(.false.)
    if (ndim .le. 1) ASSERT(.false.)
!
! --- CALCUL DES TANGENTES
!
    do 41 idim = 1, ndim
        do 31 ino = 1, nno
            tau1(idim) = coorma(3*(ino-1)+idim)*dff(1,ino) + tau1( idim)
            if (ndim .eq. 3) then
                tau2(idim) = coorma(3*(ino-1)+idim)*dff(2,ino) + tau2( idim)
            endif
31      continue
41  end do
!
!
end subroutine
