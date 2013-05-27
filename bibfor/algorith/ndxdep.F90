subroutine ndxdep(numedd, fonact, numins, sddisc, sddyna,&
                  sdnume, valinc, solalg, veasse)
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
    include 'asterfort/copisd.h'
    include 'asterfort/diinst.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/nmdebg.h'
    include 'asterfort/nmfext.h'
    include 'asterfort/nmmajc.h'
    include 'asterfort/nmsolu.h'
    integer :: fonact(*)
    integer :: numins
    character(len=19) :: sddisc, sdnume, sddyna
    character(len=24) :: numedd
    character(len=19) :: veasse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DE L'INCREMENT DE DEPLACEMENT A PARTIR DE(S) DIRECTION(S)
! DE DESCENTE
!
! CAS EXPLICITE
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  NUMINS : NUMERO D'INSTANT
! IN  SDNUME : SD NUMEROTATION
! IN  SDDISC : SD DISCRETISATION
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
!
!
!
    real(kind=8) :: instam, instap, deltat
    character(len=19) :: cnfext
    character(len=19) :: ddepla, deppr1
    character(len=19) :: dvitla, vitpr1
    character(len=19) :: daccla, accpr1
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
        write (ifm,*) '<MECANONLINE> CORRECTION INCR. DEPL.'
    endif
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(solalg, 'SOLALG', 'DEPPR1', deppr1)
    call nmchex(solalg, 'SOLALG', 'DVITLA', dvitla)
    call nmchex(solalg, 'SOLALG', 'VITPR1', vitpr1)
    call nmchex(solalg, 'SOLALG', 'DACCLA', daccla)
    call nmchex(solalg, 'SOLALG', 'ACCPR1', accpr1)
!
! --- INITIALISATIONS
!
    instam = diinst(sddisc,numins-1)
    instap = diinst(sddisc,numins)
    deltat = instap - instam
!
! --- CALCUL DE LA RESULTANTE DES EFFORTS EXTERIEURS
!
    call nmchex(veasse, 'VEASSE', 'CNFEXT', cnfext)
    call nmfext(0.d0, fonact, sddyna, veasse, cnfext)
!
! --- CONVERSION RESULTAT dU VENANT DE K.dU = F SUIVANT SCHEMAS
!
    call nmsolu(sddyna, solalg)
!
! --- AJUSTEMENT DE LA DIRECTION DE DESCENTE (AVEC ETA, RHO ET OFFSET)
!
    call copisd('CHAMP_GD', 'V', deppr1, ddepla)
    call copisd('CHAMP_GD', 'V', vitpr1, dvitla)
    call copisd('CHAMP_GD', 'V', accpr1, daccla)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... DEPL. PRED. (1) : '
        call nmdebg('VECT', deppr1, ifm)
        write (ifm,*) '<MECANONLINE> ... DEPL. SOLU.     : '
        call nmdebg('VECT', ddepla, ifm)
        write (ifm,*) '<MECANONLINE> ... VITE. PRED. (1) : '
        call nmdebg('VECT', vitpr1, ifm)
        write (ifm,*) '<MECANONLINE> ... VITE. SOLU.     : '
        call nmdebg('VECT', dvitla, ifm)
        write (ifm,*) '<MECANONLINE> ... ACCE. PRED. (1) : '
        call nmdebg('VECT', accpr1, ifm)
        write (ifm,*) '<MECANONLINE> ... ACCE. SOLU.     : '
        call nmdebg('VECT', daccla, ifm)
    endif
!
! --- ACTUALISATION DES CHAMPS SOLUTIONS
!
    call nmmajc(fonact, sddyna, sdnume, deltat, numedd,&
                valinc, solalg)
!
    call jedema()
end subroutine
