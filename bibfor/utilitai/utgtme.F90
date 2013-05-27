subroutine utgtme(nbarg, nomarg, valarg, iret)
    implicit none
    include 'asterc/loisem.h'
    include 'asterc/mempid.h'
    include 'asterfort/assert.h'
    integer :: nbarg, iret
    character(len=8) :: nomarg(*)
    real(kind=8) :: valarg(*)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     RENVOIE LA OU LES VALEURS ASSOCIEES AU(X) NOM(S) NOMARG DES
!     PARAMETRES MEMOIRE EN MEGA OCTETS
! IN  NBARG   : NOMBRE D'ARGUMENTS (>1)
! IN  NOMARG  : NOMS DES PARAMETRES
! OUT VALARG  : VALEURS DES PARAMETRES
! OUT IRET    : CODE RETOUR
!                =0 TOUTES LES VALEURS ONT ETE TROUVEES
!               !=0 AU MOINS UNE VALEUR EST INVALIDE
!
! DEB ------------------------------------------------------------------
    real(kind=8) :: svuse, smxuse
    common /statje/ svuse,smxuse
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2)
    real(kind=8) :: vmumps, vpetsc, rlqmem, vminit, vmjdc
    common /msolve/ vmumps,vpetsc,rlqmem,vminit,vmjdc
! ----------------------------------------------------------------------
    integer :: k, iv(4), ival, lois
    character(len=8) :: nom
! ----------------------------------------------------------------------
    iret = 0
    call assert(nbarg .ge.1)
    lois = loisem()
    iv(1) = 0
    iv(2) = 0
    iv(3) = 0
    iv(4) = 0
    ival = mempid(iv)
!     IV(1)=VmData IV(2)=VmSize IV(3)=VmPeak IV(4)=VmRSS
!
    do 100 k = 1, nbarg
!
        nom = nomarg(k)
        if (nom .eq. 'VMPEAK') then
!
! ----- PIC MEMOIRE TOTALE
!
            if (ival .ne. -1) then
                valarg(k) = dble(iv(3))/1024
            else
                iret = iret - 1
                valarg(k) = 0.d0
            endif
!
        else if (nom .eq. 'VMRSS') then
!
! ----- MEMOIRE RESIDENTE
!
            if (ival .ne. -1) then
                valarg(k) = dble(iv(4))/1024
            else
                iret = iret - 1
                valarg(k) = 0.d0
            endif
        else if (nom .eq. 'VMSIZE') then
!
! ----- MEMOIRE INSTANTANNEE
!
            if (ival .ne. -1) then
                valarg(k) = dble(iv(2))/1024
            else
                iret = iret - 1
                valarg(k) = 0.d0
            endif
!
        else if (nom .eq. 'LIMIT_JV') then
!
! ----- LIMITE MEMOIRE JEVEUX (MODIFIEE PAR JERMXD)
!
            valarg(k) = vmxdyn*lois/(1024*1024)
!
        else if (nom .eq. 'COUR_JV ') then
!
! ----- CONSOMMATION MEMOIRE JEVEUX COURANTE (CUMUL DES ALLOCATIONS)
!
            valarg(k) = mcdyn*lois/(1024*1024)
!
        else if (nom .eq. 'CMAX_JV ') then
!
! ----- CONSOMMATION MAXIMUM MEMOIRE JEVEUX (MAX DES CUMULS)
!
            valarg(k) = mxdyn/(1024*1024)
!
        else if (nom .eq. 'CMXU_JV ') then
!
! ----- CONSOMMATION MAXIMUM MEMOIRE JEVEUX
!       OBJETS JEVEUX UTILISES
!
            valarg(k) = (smxuse*lois)/(1024*1024)
!
        else if (nom .eq. 'CUSE_JV ') then
!
! ----- CONSOMMATION MEMOIRE COURANTE JEVEUX
!       OBJETS JEVEUX UTILISES
!
            valarg(k) = (svuse*lois)/(1024*1024)
!
        else if (nom .eq. 'MEM_TOTA') then
!
! ----- LIMITE MEMOIRE ALLOUEE LORS DE L'EXECUTION
!
            valarg(k) = vmet*lois/(1024*1024)
!
        else if (nom .eq. 'MEM_MUMP') then
!
! ----- CONSOMMATION MEMOIRE DU SOLVEUR MUMPS
!
            valarg(k) = vmumps/(1024*1024)
!
        else if (nom .eq. 'MEM_PETS') then
!
! ----- CONSOMMATION MEMOIRE DU SOLVEUR PETSC
!
            valarg(k) = vpetsc/(1024*1024)
!
        else if (nom .eq. 'MEM_INIT') then
!
! ----- CONSOMMATION MEMOIRE DU JDC
!
            valarg(k) = vminit/(1024*1024)
!
        else if (nom .eq. 'MEM_JDC') then
!
! ----- CONSOMMATION MEMOIRE DU JDC
!
            valarg(k) = vmjdc/(1024*1024)
!
        else if (nom .eq. 'RLQ_MEM') then
!
! ------ ESTIMATION DU RELIQUAT MEMOIRE
!
            valarg(k) = rlqmem/(1024*1024)
!
        else
            iret = iret - 1
        endif
!
100  end do
!
! FIN ------------------------------------------------------------------
end subroutine
