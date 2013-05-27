subroutine cazofm(char, motfac, iform, nzoco)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/cazouu.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: char
    integer :: nzoco, iform
    character(len=16) :: motfac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! AFFECTATION FORMULATION/METHODE DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  METHOD : METHODE DE CONTACT
! IN  IFORM  : TYPE DE FORMULATION DE CONTACT
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
!
!
!
!
    integer :: noc
    character(len=16) :: algoc, algof, formul, frott
    integer :: icont, ifrot, izone
    logical :: lfrot, lmunul
    character(len=24) :: defico
    character(len=24) :: paraci
    integer :: jparci
    character(len=16) :: valk(3)
    real(kind=8) :: coefff
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    icont = 0
    ifrot = 0
    lfrot = .false.
    formul = ' '
    algoc = ' '
    algof = ' '
    if (iform .eq. 1) then
        formul = 'DISCRETE'
    else if (iform.eq.2) then
        formul = 'CONTINUE'
    else if (iform.eq.3) then
        formul = 'XFEM'
    else
        call assert(.false.)
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    paraci = defico(1:16)//'.PARACI'
    call jeveuo(paraci, 'E', jparci)
!
! --- LA FORMULATION (UNIQUE !)
!
    zi(jparci+4-1) = iform
!
! --- FROTTEMENT ?
!
    call getvtx(' ', 'FROTTEMENT', 1, iarg, 1,&
                frott, noc)
    lfrot = frott.eq.'COULOMB'
!
! --- RECUPERATION DES METHODES
!
    call getvtx(motfac, 'ALGO_CONT', 1, iarg, 1,&
                algoc, noc)
!
    valk(1) = formul
    valk(2) = algoc
    if (lfrot) then
        call getvtx(motfac, 'ALGO_FROT', 1, iarg, 1,&
                    algof, noc)
        valk(3) = algof
    endif
!
    if (iform .eq. 1) then
        call cazouu(motfac, nzoco, 'ALGO_CONT')
!
        if (lfrot) then
            call cazouu(motfac, nzoco, 'ALGO_FROT')
            if (algof(1:12) .eq. 'PENALISATION') then
                ifrot = 1
!
                if (algoc(1:12) .eq. 'PENALISATION') then
                    icont = 4
!
                else if (algoc(1:8) .eq. 'LAGRANGI') then
                    icont = 5
!
                else
                    call u2mesk('F', 'CONTACT3_3', 3, valk)
                endif
!
            else if (algof(1:10) .eq. 'LAGRANGIEN') then
                ifrot = 2
!
                if (algoc(1:8) .eq. 'LAGRANGI') then
                    icont = 5
!
                else
                    call u2mesk('F', 'CONTACT3_3', 3, valk)
                endif
            else
                call assert(.false.)
            endif
        else
            ifrot = 0
!
            if (algoc(1:3) .eq. 'GCP') then
                icont = 2
!
            else if (algoc(1:8) .eq. 'CONTRAIN') then
                icont = 1
!
            else if (algoc(1:8) .eq. 'PENALISA') then
                icont = 4
!
            else if (algoc(1:8) .eq. 'LAGRANGI') then
                icont = 5
!
            else
                call assert(.false.)
            endif
!
        endif
!
    else if (iform.eq.2) then
        icont = 6
!
! ----- LE COEFFICIENT DE COULOMB EST-IL NON NUL SUR AU MOINS UNE ZONE ?
!
        if (lfrot) then
            lmunul = .false.
            do 10 izone = 1, nzoco
                call getvr8(motfac, 'COULOMB', izone, iarg, 1,&
                            coefff, noc)
                lmunul = lmunul.or.(coefff.ne.0.d0)
10          continue
            if (.not.lmunul) then
                call u2mess('A', 'CONTACT3_1')
                lfrot = .false.
            endif
        endif
!
        if (lfrot) then
            ifrot = 6
        else
            ifrot = 0
        endif
!
    else if (iform.eq.3) then
        icont = 7
!
        if (lfrot) then
            ifrot = 7
        else
            ifrot = 0
        endif
!
    else
        call assert(.false.)
    endif
!
! --- SAUVEGARDE METHODE CONTACT ET FROTTEMENT
!
    zi(jparci+17-1) = icont
    zi(jparci+18-1) = ifrot
!
    call jedema()
!
end subroutine
