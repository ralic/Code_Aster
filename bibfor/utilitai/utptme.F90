subroutine utptme(nbarg, nomarg, valarg, iret)
    implicit none
    include 'asterfort/assert.h'
    integer :: nbarg, iret
    character(len=8) :: nomarg(*)
    real(kind=8) :: valarg(*)
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
! person_in_charge: j-pierre.lefebvre at edf.fr
! ----------------------------------------------------------------------
!     AFFECTE LA OU LES VALEURS ASSOICEES AU(X) NOMS NOMARG DES
!     PARAMETRES MEMOIRE EN MEGA OCTETS
! IN  NBARG   : NOMBRE D'ARGUMENTS (>1)
! IN  NOMARG  : NOMS DES PARAMETRES
! IN  VALARG  : VALEURS DES PARAMETRES
! OUT IRET    : CODE RETOUR
!                =0 TOUTES LES VALEURS ONT ETE AFFECTEES
!               !=0 AU MOINS UNE VALEUR EST INVALIDE
!
! DEB ------------------------------------------------------------------
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2)
    real(kind=8) :: vmumps, vpetsc, rlqmem, vminit, vmjdc
    common /msolve/ vmumps,vpetsc,rlqmem,vminit,vmjdc
! ----------------------------------------------------------------------
    integer :: k
    character(len=8) :: nom
! ----------------------------------------------------------------------
    iret = 0
    if (nbarg .ge. 1) then
!
        do 100 k = 1, nbarg
!
            nom = nomarg(k)
            if (nom .eq. 'MEM_TOTA') then
!
! --------- LIMITE MEMOIRE ALLOUEE LORS DE L'EXECUTION
!
                vmet = valarg(k)*(1024*1024)
!
!
            else if (nom .eq. 'RLQ_MEM') then
!
! -------- RELIQUAT MEMOIRE (CONSOMMATION HORS JEVEUX ET SOLVEUR)
!
                rlqmem = valarg(k)*(1024*1024)
!
            else if (nom .eq. 'MEM_MUMP') then
!
! --------- CONSOMMATION MEMOIRE DU SOLVEUR MUMPS
!
                vmumps = valarg(k)*(1024*1024)
!
            else if (nom .eq. 'MEM_PETS') then
!
! --------- CONSOMMATION MEMOIRE DU SOLVEUR PETSC
!
                vpetsc = valarg(k)*(1024*1024)
!
            else if (nom .eq. 'MEM_INIT') then
!
! --------- CONSOMMATION MEMOIRE DU JDC
!
                vminit = valarg(k)*(1024*1024)
!
            else if (nom .eq. 'MEM_JDC') then
!
! --------- CONSOMMATION MEMOIRE DU JDC
!
                vmjdc = valarg(k)*(1024*1024)
!
            else
                iret = iret + 1
            endif
!
100      continue
    else
        call assert(.false.)
    endif
!
! FIN ------------------------------------------------------------------
end subroutine
