subroutine nmdocp(ncomel, lcomel, txcp)
! person_in_charge: jean-michel.proix at edf.fr
    implicit none
    integer :: ncomel
    character(len=16) :: lcomel(5), txcp
!
! ----------------------------------------------------------------------
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
!    SAISIE ET VERIFICATION DE DEBORST
!
! IN/OUT  NCOMEL  : NOMBRE TOTAL DE COMPORTEMENTS ELEMENTAIRES
! IN/OUT  LCOMEL  : NOMS DES COMPORTEMENTS ELEMENTAIRES
! IN      TXCP    : TYPE DE CONTRAINTES PLANES : ANALYTIQUE OU DEBORST
! ----------------------------------------------------------------------
    if (txcp .eq. 'DEBORST') then
        ncomel=ncomel+1
        lcomel(ncomel)='DEBORST'
    endif
end subroutine
