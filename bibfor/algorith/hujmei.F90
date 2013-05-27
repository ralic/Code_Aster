subroutine hujmei(vin)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   ------------------------------------------------------------------
!   ENREGISTREMENT DES VARIABLES MEMOIRE POUR LE MECANISME ISOTROPE
!   IN  MATER  :  COEFFICIENTS MATERIAU A T+DT
!       VIN    :  VARIABLES INTERNES
!       SIG    :  CONTRAINTE
!
!   OUT VIN    :  VARIABLES INTERNES MODIFIEES
!   ------------------------------------------------------------------
    real(kind=8) :: un, zero, vin(*)
! ----------------------------------------------------------------------
    data      zero, un  /0.d0, 1.d0/
!
! ==================================================================
! --- MISE A JOUR DES VARIABLES INTERNES DE MEMOIRE ----------------
! ==================================================================
!
    if (vin(21) .eq. zero) then
        vin(21) = vin(4)
    else
        vin(21)= vin(21)-vin(22)*vin(8)
    endif
!
    if (vin(22) .eq. zero) then
        vin(22) = un
    else
        vin(22) = - vin(22)
    endif
!
end subroutine
