subroutine dxqnim(qsi, eta, nmi)
    implicit  none
    real(kind=8) :: qsi, eta, nmi(4)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     FONCTIONS D'INTERPOLATION MEMBRANE POUR LES ELEMS DKQ, DSQ ET Q4G
!     ------------------------------------------------------------------
    real(kind=8) :: unquar, un
!     ------------------------------------------------------------------
    unquar = 0.25d0
    un = 1.0d0
!
    nmi(1) = unquar*(un-qsi)*(un-eta)
    nmi(2) = unquar*(un+qsi)*(un-eta)
    nmi(3) = unquar*(un+qsi)*(un+eta)
    nmi(4) = unquar*(un-qsi)*(un+eta)
!
end subroutine
