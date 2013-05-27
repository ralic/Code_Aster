subroutine ndpred(sddyna, valinc, solalg)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/copisd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ndynin.h'
    include 'asterfort/ndynre.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/nmdebg.h'
    include 'asterfort/vtaxpy.h'
    include 'asterfort/vtzero.h'
    character(len=19) :: sddyna
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! CALCUL DES PREDICTEURS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
!
!
!
    integer :: jcfsc
    character(len=24) :: cfsc
    logical :: ldepl, lvite, lacce
    integer :: n
    real(kind=8) :: coefd(3), coefv(3), coefa(3)
    character(len=24) :: vect(3)
    character(len=19) :: depdel, vitdel, accdel
    character(len=19) :: depkm1, vitkm1, acckm1, romkm1, romk
    character(len=19) :: depplu, vitplu, accplu
    character(len=19) :: depmoi, vitmoi, accmoi
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CALCUL PREDICTEURS'
    endif
!
! --- ACCES SD DYNA
!
    cfsc = sddyna(1:15)//'.COEF_SCH'
    call jeveuo(cfsc, 'L', jcfsc)
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPKM1', depkm1)
    call nmchex(valinc, 'VALINC', 'VITKM1', vitkm1)
    call nmchex(valinc, 'VALINC', 'ACCKM1', acckm1)
    call nmchex(valinc, 'VALINC', 'ROMKM1', romkm1)
    call nmchex(valinc, 'VALINC', 'ROMK  ', romk)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(solalg, 'SOLALG', 'VITDEL', vitdel)
    call nmchex(solalg, 'SOLALG', 'ACCDEL', accdel)
!
! --- TYPE DE FORMULATION SCHEMA DYNAMIQUE GENERAL
!
    ldepl = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.1
    lvite = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.2
    lacce = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.3
!
! --- COEFFICIENTS POUR PREDICTEURS
!
    coefd(1) = ndynre(sddyna,'COEF_DEPL_DEPL')
    coefd(2) = ndynre(sddyna,'COEF_DEPL_VITE')
    coefd(3) = ndynre(sddyna,'COEF_DEPL_ACCE')
    coefv(1) = ndynre(sddyna,'COEF_VITE_DEPL')
    coefv(2) = ndynre(sddyna,'COEF_VITE_VITE')
    coefv(3) = ndynre(sddyna,'COEF_VITE_ACCE')
    coefa(1) = ndynre(sddyna,'COEF_ACCE_DEPL')
    coefa(2) = ndynre(sddyna,'COEF_ACCE_VITE')
    coefa(3) = ndynre(sddyna,'COEF_ACCE_ACCE')
!
! --- MISE A JOUR CHAMPS GRANDES ROTATIONS
!
    call vtzero(romkm1)
    call vtzero(romk)
!
! --- CALCUL DES PREDICTEURS
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
    call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
    call copisd('CHAMP_GD', 'V', depmoi, depkm1)
    call copisd('CHAMP_GD', 'V', vitmoi, vitkm1)
    call copisd('CHAMP_GD', 'V', accmoi, acckm1)
    vect(1) = depkm1
    vect(2) = vitkm1
    vect(3) = acckm1
    if (ldepl) then
        call vtzero(vitplu)
        call vtzero(accplu)
        do 10 n = 1, 3
            call vtaxpy(coefv(n), vect(n), vitplu)
            call vtaxpy(coefa(n), vect(n), accplu)
10      continue
    else if (lvite) then
        call vtzero(depplu)
        call vtzero(accplu)
        call copisd('CHAMP_GD', 'V', vitkm1, vitplu)
        do 11 n = 1, 3
            call vtaxpy(coefd(n), vect(n), depplu)
            call vtaxpy(coefa(n), vect(n), accplu)
11      continue
    else if (lacce) then
        call vtzero(vitplu)
        call vtzero(depplu)
        do 12 n = 1, 3
            call vtaxpy(coefv(n), vect(n), vitplu)
            call vtaxpy(coefd(n), vect(n), depplu)
12      continue
        call copisd('CHAMP_GD', 'V', acckm1, accplu)
    else
        call assert(.false.)
    endif
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... PRED. DEPL. '
        call nmdebg('VECT', depplu, ifm)
        write (ifm,*) '<MECANONLINE> ...... PRED. VITE. '
        call nmdebg('VECT', vitplu, ifm)
        write (ifm,*) '<MECANONLINE> ...... PRED. ACCE. '
        call nmdebg('VECT', accplu, ifm)
    endif
!
! --- INITIALISATION DE L'INCREMENT DE DEPLACEMENT DEPDEL
!
    call vtzero(depdel)
    if (lacce) then
        do 14 n = 1, 3
            call vtaxpy(coefd(n), vect(n), depdel)
14      continue
        call vtaxpy(-1.d0, depkm1, depdel)
    endif
!
! --- INITIALISATION DE L'INCREMENT DE VITESSE/ACCELERATION
!
    call vtzero(vitdel)
    call vtzero(accdel)
!
    call jedema()
!
end subroutine
