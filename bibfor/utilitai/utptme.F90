subroutine utptme(nomarg, valarg, iret)
    implicit none
    character(len=8), intent(in) :: nomarg
    real(kind=8), intent(in) :: valarg
    integer, intent(out) :: iret
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
! person_in_charge: j-pierre.lefebvre at edf.fr
! ----------------------------------------------------------------------
!     Affecte la valeur associée au nom nomarg du paramètre mémoire en Mo
! in  nomarg  : nom du paramètre
! in  valarg  : valeur du paramètre
! out iret    : code retour
!                =0 la valeur a été affectée
!               !=0 la valeur est invalide
!
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2)
    real(kind=8) :: vmumps, vpetsc, rlqmem, vminit, vmjdc
    common /msolve/ vmumps,vpetsc,rlqmem,vminit,vmjdc
! ----------------------------------------------------------------------
    iret = 0
    if (nomarg .eq. 'MEM_TOTA') then
!
! --------- LIMITE MEMOIRE ALLOUEE LORS DE L'EXECUTION
!
        vmet = valarg*(1024*1024)
!
!
    else if (nomarg .eq. 'RLQ_MEM') then
!
! -------- RELIQUAT MEMOIRE (CONSOMMATION HORS JEVEUX ET SOLVEUR)
!
        rlqmem = valarg*(1024*1024)
!
    else if (nomarg .eq. 'MEM_MUMP') then
!
! --------- CONSOMMATION MEMOIRE DU SOLVEUR MUMPS
!
        vmumps = valarg*(1024*1024)
!
    else if (nomarg .eq. 'MEM_PETS') then
!
! --------- CONSOMMATION MEMOIRE DU SOLVEUR PETSC
!
        vpetsc = valarg*(1024*1024)
!
    else if (nomarg .eq. 'MEM_INIT') then
!
! --------- CONSOMMATION MEMOIRE DU JDC
!
        vminit = valarg*(1024*1024)
!
    else if (nomarg .eq. 'MEM_JDC') then
!
! --------- CONSOMMATION MEMOIRE DU JDC
!
        vmjdc = valarg*(1024*1024)
!
    else
        iret = 1
    endif
!
end subroutine
