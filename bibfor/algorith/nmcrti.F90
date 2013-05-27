subroutine nmcrti(sdtime)
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
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmtime.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: sdtime
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - INITIALISATIONS)
!
! CREATION DE LA SD TIMER
!
! ----------------------------------------------------------------------
!
!
! IN  SDTIME : SD TIMER
!
!
!
!
    integer :: nbtime, nbmesu
    parameter    (nbtime=6,nbmesu=17)
!
    integer :: ifm, niv
    character(len=24) :: timpas, timite, timarc, timpst
    integer :: jtpas, jtite, jtarc, jtpst
    character(len=24) :: timdeb
    integer :: jtdeb
    character(len=24) :: timtm1, timtm2
    integer :: jtmp1, jtmp2
    character(len=24) :: timet, timep, timen
    integer :: jtimet, jtimep, jtimen
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION SD TIMER'
    endif
!
! --- TIMERS: PAS/ITERATION/ARCHIVAGE/AUTRES
!
    timtm1 = sdtime(1:19)//'.TMP1'
    timtm2 = sdtime(1:19)//'.TMP2'
    timpas = sdtime(1:19)//'.TPAS'
    timite = sdtime(1:19)//'.TITE'
    timarc = sdtime(1:19)//'.TARC'
    timpst = sdtime(1:19)//'.TPST'
    call wkvect(timtm1, 'V V R', 4, jtmp1)
    call wkvect(timtm2, 'V V R', 4, jtmp2)
    call wkvect(timpas, 'V V R', 4, jtpas)
    call wkvect(timite, 'V V R', 4, jtite)
    call wkvect(timarc, 'V V R', 4, jtarc)
    call wkvect(timpst, 'V V R', 4, jtpst)
!
! --- STOCKAGE TEMPS INITIAL POUR LES TIMERS
!
    timdeb = sdtime(1:19)//'.TDEB'
    call wkvect(timdeb, 'V V R', nbtime, jtdeb)
!
! --- STOCKAGE MESURES - ITERATION DE NEWTON
!
    timen = sdtime(1:19)//'.TIMN'
    call wkvect(timen, 'V V R', nbmesu, jtimen)
!
! --- STOCKAGE MESURES - PAS DE TEMPS
!
    timep = sdtime(1:19)//'.TIMP'
    call wkvect(timep, 'V V R', nbmesu, jtimep)
!
! --- STOCKAGE MESURES - TOTAL TRANSITOIRE
!
    timet = sdtime(1:19)//'.TIMT'
    call wkvect(timet, 'V V R', nbmesu, jtimet)
!
! --- INITIALISATION DE TOUS LES TIMERS
!
    call nmtime(sdtime, 'INI', 'ALL')
!
    call jedema()
end subroutine
