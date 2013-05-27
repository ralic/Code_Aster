subroutine gcnco2(nomk8)
    implicit none
    include 'asterfort/codent.h'
    character(len=8) :: nomk8
!     ------------------------------------------------------------------
!     CALCUL D'UN NOUVEAU NOM PAR INCREMENTATION DU NOM DE +1
!     REMARQUE : ON IGNORE LE PREMIER CARACTERE DE NOMK8
!
! EXEMPLE D'UTILISATION :
!      CHARACTER*8 NEWNOM
!      NEWNOM='.0000123'
!      CALL GCNCO2(NEWNOM)
!      CALL GCNCO2(NEWNOM)
!      PRINT NEWNOM => '.0000125'
!
!     ------------------------------------------------------------------
! IN/OUT NOMK8 : K8 : NOM A INCREMENTER :
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ------------------------------------------------------------------
    integer :: num
!     ------------------------------------------------------------------
    read (nomk8(2:8),'(I7)') num
    call codent(num+1, 'D0', nomk8(2:8))
end subroutine
