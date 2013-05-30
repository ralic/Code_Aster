subroutine nmfocc(phase, modele, mate, numedd, fonact,&
                  defico, resoco, sdstat, sdtime, solalg,&
                  valinc, veelem, veasse)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'asterfort/assvec.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/nmdebg.h'
    include 'asterfort/nmelcv.h'
    include 'asterfort/nmrinc.h'
    include 'asterfort/nmtime.h'
    include 'asterfort/vtaxpy.h'
    character(len=10) :: phase
    character(len=24) :: defico, resoco
    integer :: fonact(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: veelem(*), veasse(*)
    character(len=24) :: mate, numedd
    character(len=24) :: modele, sdstat, sdtime
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL DU SECOND MEMBRE POUR CONTACT/XFEM
!
! ----------------------------------------------------------------------
!
!
! IN  PHASE  : PHASE DE CALCUL
!               'PREDICTION' - PHASE DE PREDICTION
!               'CONVERGENC' - PHASE DE CONVERGENCE
!               'CORRECTION' - PHASE DE CORRECTION
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  MATE   : CHAMP MATERIAU
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  SDSTAT : SD STATISTIQUES
! IN  SDTIME : SD TIMER
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    logical :: leltc, leltf, lallv, lnewtc, lnewtg
    character(len=19) :: veeltc, veeltf
    character(len=19) :: cneltf, cneltc, cnfint
    character(len=19) :: depmoi, depdel, vitmoi, accmoi, vitplu
    character(len=8) :: noma
    integer :: iret, ibid
!
! ----------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> CALCUL DU SECOND MEMBRE'
    endif
!
! --- INITIALISATIONS
!
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma, iret)
!
! --- FONCTIONNALITES ACTIVEES
!
    leltc = isfonc(fonact,'ELT_CONTACT')
    leltf = isfonc(fonact,'ELT_FROTTEMENT')
    lallv = isfonc(fonact,'CONT_ALL_VERIF')
    lnewtc = isfonc(fonact,'CONT_NEWTON')
    lnewtg = isfonc(fonact,'GEOM_NEWTON')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
    call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(veelem, 'VEELEM', 'CNELTC', veeltc)
    call nmchex(veelem, 'VEELEM', 'CNELTF', veeltf)
    call nmchex(veasse, 'VEASSE', 'CNELTC', cneltc)
    call nmchex(veasse, 'VEASSE', 'CNELTF', cneltf)
!
! --- PAS DE CALCUL EN NEWTON (IL FAUT RE-EVALUER LES STATUTS D'ABORD)
!
    if ((phase.eq.'CONVERGENC') .and. (lnewtc.or.lnewtg)) then
        goto 999
    endif
!
! --- ON ENLEVE
!
    if (phase .eq. 'CORRECTION') then
        if (leltc .and. (.not.lallv)) call vtaxpy(-1.d0, cneltc, cnfint)
        if (leltf .and. (.not.lallv)) call vtaxpy(-1.d0, cneltf, cnfint)
    endif
!
! --- FORCES ELEMENTS DE CONTACT (XFEM+CONTINU)
!
    if (leltc .and. (.not.lallv)) then
        call nmtime(sdtime, 'INI', 'CTCC_VECT')
        call nmtime(sdtime, 'RUN', 'CTCC_VECT')
        call nmelcv('CONT', modele, defico, resoco, mate,&
                    depmoi, depdel, vitmoi, vitplu, accmoi,&
                    veeltc)
        call assvec('V', cneltc, 1, veeltc, 1.d0,&
                    numedd, ' ', 'ZERO', 1)
        call nmtime(sdtime, 'END', 'CTCC_VECT')
        call nmrinc(sdstat, 'CTCC_VECT')
        if (niv .eq. 2) then
            call nmdebg('VECT', cneltc, ifm)
        endif
    endif
!
! --- FORCES ELEMENTS DE FROTTEMENT (XFEM+CONTINU)
!
    if (leltf .and. (.not.lallv)) then
        call nmtime(sdtime, 'INI', 'CTCC_VECT')
        call nmtime(sdtime, 'RUN', 'CTCC_VECT')
        call nmelcv('FROT', modele, defico, resoco, mate,&
                    depmoi, depdel, vitmoi, vitplu, accmoi,&
                    veeltf)
        call assvec('V', cneltf, 1, veeltf, 1.d0,&
                    numedd, ' ', 'ZERO', 1)
        call nmtime(sdtime, 'END', 'CTCC_VECT')
        call nmrinc(sdstat, 'CTCC_VECT')
        if (niv .eq. 2) then
            call nmdebg('VECT', cneltf, ifm)
        endif
    endif
!
! --- ON REMET
!
    if (phase .eq. 'CORRECTION') then
        if (leltc .and. (.not.lallv)) call vtaxpy(1.d0, cneltc, cnfint)
        if (leltf .and. (.not.lallv)) call vtaxpy(1.d0, cneltf, cnfint)
    endif
!
999  continue
!
end subroutine
