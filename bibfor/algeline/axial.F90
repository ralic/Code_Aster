subroutine axial(antisy, vecax)
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
!
! FONCTION: FORME LE VECTEUR AXIAL 'VECAX' CORRESPONDANT A LA MATRICE
!           ANTISYMETRIQUE 'ANTISY'.
!
!     IN  : ANTISY    : MATRICE ANTISYMETRIQUE D'ORDRE 3
!
!     OUT : VECAX     : VECTEUR D'ORDRE 3
! ------------------------------------------------------------------
    implicit none
    real(kind=8) :: antisy(3, 3), vecax(3)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    vecax(1) = antisy(3,2)
    vecax(2) = antisy(1,3)
    vecax(3) = antisy(2,1)
end subroutine
