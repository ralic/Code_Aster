subroutine fin999()
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mathieu.courtois at edf.fr
    include 'asterc/chkmsg.h'
    include 'asterc/dllcls.h'
    include 'asterfort/ap9999.h'
    include 'asterfort/mpialr.h'
    integer :: ichk
!     ------------------------------------------------------------------
!     FERMETURE DE PETSC
    call ap9999()
!     LIBERE TOUS LES COMPOSANTS CHARGES DYNAMIQUEMENT
    call dllcls()
!     VERIFICATION DES ALARMES EN PARALLELE
    call mpialr()
!     TEST ERREUR E SANS ERREUR F
    call chkmsg(1, ichk)
!
end subroutine
