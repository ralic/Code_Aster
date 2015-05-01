subroutine vicin0(compor, nbcin, numcin)
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
    integer :: nbcin, numcin(2)
    character(len=16) :: compor(*)
! ---------------------------------
!   ENTREE  :
!   NDIM    : DIMENSION DE L'ESPACE
!   COMPOR  : COMPORTEMENT
!   SORTIE:
!   NBCIN   : NOMBRE DE VARIABLES CINEMATIQUES
!   NUMCIN  : NUMEROS DES VARIABLES CINEMATIQUES
! ----------------------------
!
!   LE COMPORTEMENT EST-IL CINEMATIQUE ?
!
! -------------------------
    nbcin=0
!
    if (compor(1) .eq. 'VMIS_CINE_LINE') then
        nbcin=1
        numcin(1)=1
    else if (compor(1).eq.'VMIS_ECMI_LINE') then
        nbcin=1
        numcin(1)=3
    else if (compor(1).eq.'VMIS_ECMI_TRAC') then
        nbcin=1
        numcin(1)=3
    else if (compor(1).eq.'VMIS_CIN1_CHAB') then
        nbcin=1
        numcin(1)=3
    else if (compor(1).eq.'VMIS_CIN2_CHAB') then
        nbcin=2
        numcin(1)=3
        numcin(2)=9
    else if (compor(1).eq.'VMIS_CIN2_MEMO') then
        nbcin=2
        numcin(1)=3
        numcin(2)=9
    else if (compor(1).eq.'VISC_CIN1_CHAB') then
        nbcin=1
        numcin(1)=3
    else if (compor(1).eq.'VISC_CIN2_CHAB') then
        nbcin=2
        numcin(1)=3
        numcin(2)=9
    else if (compor(1).eq.'VISC_CIN2_MEMO') then
        nbcin=2
        numcin(1)=3
        numcin(2)=9
    endif
end subroutine
