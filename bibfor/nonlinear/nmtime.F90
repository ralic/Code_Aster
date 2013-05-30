subroutine nmtime(sdtime, phasez, timerz)
!
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
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nmrtim.h'
    include 'asterfort/uttcpr.h'
    include 'asterfort/uttcpu.h'
    character(len=24) :: sdtime
    character(len=*) :: phasez
    character(len=*) :: timerz
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! GESTION DES TIMERS
!
! ----------------------------------------------------------------------
!
!
! IN  SDTIME : SD TIMER
! IN  PHASE  : TYPE D'ACTION
!              'INI'               INITIALISATION DU TIMER
!              'RUN'               LANCEMENT DU TIMER
!              'END'               ARRET DU TIMER
! IN  TIMER  : NOM DU TIMER
!              'PAS'               TIMER PAS DE TEMPS
!              'ITE'               TIMER ITERATION DE NEWTON
!              'ARC'               TIMER ARCHIVAGE
!              'POST_TRAITEMENT'   TIMER POST_PROCESSING
!              'FACTOR'            TIMER FACTORISATION
!              'SOLVE'             TIMER RESOLUTION
!              'INTEGRATION'       TIMER INTEG. LDC
!              'ASSE_MATR'         TIMER ASSEMBLAGE MATRICES
!              'CONT_GEOM'         TIMER APPARIEMENT CONTACT
!              'CTCD_ALGO'         TIMER RESOLUTION CONTACT DISCRET
!              'CTCC_CONT'         TIMER RESOLUTION CONTACT CONTINU
!              'CTCC_FROT'         TIMER RESOLUTION FROTTEMENT CONTINU
!              'CTCC_MATR'         TIMER CALCUL MATRICE CONTINU
!              'SECO_MEMB'         TIMER CALCUL SECOND MEMBRE
!
!
!
!
    character(len=24) :: timpas, timite, timarc, timpst
    integer :: jtpas, jtite, jtarc, jtpst
    character(len=24) :: timdeb
    integer :: jtdeb
    character(len=24) :: timtm1, timtm2
    integer :: jtmp1, jtmp2
    character(len=24) :: phase, timer
    real(kind=8) :: time
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    phase = phasez
    timer = timerz
!
! --- ACCES SD TIMER
!
    timtm1 = sdtime(1:19)//'.TMP1'
    timtm2 = sdtime(1:19)//'.TMP2'
    timpas = sdtime(1:19)//'.TPAS'
    timite = sdtime(1:19)//'.TITE'
    timpst = sdtime(1:19)//'.TPST'
    timarc = sdtime(1:19)//'.TARC'
    call jeveuo(timtm1, 'E', jtmp1)
    call jeveuo(timtm2, 'E', jtmp2)
    call jeveuo(timpas, 'E', jtpas)
    call jeveuo(timite, 'E', jtite)
    call jeveuo(timpst, 'E', jtpst)
    call jeveuo(timarc, 'E', jtarc)
!
    timdeb = sdtime(1:19)//'.TDEB'
    call jeveuo(timdeb, 'E', jtdeb)
!
! --- INTERROGATION SD
!
    if (phase .eq. 'INI') then
!
! ----- INITIALISATIONS DES TIMERS
!
        if (timer .eq. 'ALL') then
            call uttcpu('CPU.NMTIME.PAS', 'INIT', ' ')
            call uttcpu('CPU.NMTIME.ITE', 'INIT', ' ')
            call uttcpu('CPU.NMTIME.ARC', 'INIT', ' ')
            call uttcpu('CPU.NMTIME.PST', 'INIT', ' ')
            call uttcpu('CPU.NMTIME.TM1', 'INIT', ' ')
            call uttcpu('CPU.NMTIME.TM2', 'INIT', ' ')
            zr(jtdeb+1-1) = 0.d0
            zr(jtdeb+2-1) = 0.d0
            zr(jtdeb+3-1) = 0.d0
            zr(jtdeb+4-1) = 0.d0
            zr(jtdeb+5-1) = 0.d0
            zr(jtdeb+6-1) = 0.d0
        else if (timer.eq.'POST_TRAITEMENT') then
            call uttcpu('CPU.NMTIME.PST', 'INIT', ' ')
            zr(jtdeb+4-1) = 0.d0
            elseif ((timer.eq.'INTEGRATION').or. (timer.eq.'FACTOR').or.&
        (timer.eq.'SECO_MEMB').or. (timer.eq.'SOLVE').or. (&
        timer.eq.'CONT_GEOM').or. (timer.eq.'CTCD_ALGO').or. (&
        timer.eq.'CTCC_PREP').or. (timer.eq.'CTCC_MATR').or. (&
        timer.eq.'CTCC_VECT').or. (timer.eq.'CTCC_CONT').or. (&
        timer.eq.'CTCC_FROT')) then
            call uttcpu('CPU.NMTIME.TM1', 'INIT', ' ')
            zr(jtdeb+5-1) = 0.d0
        else if (timer.eq.'ASSE_MATR') then
            call uttcpu('CPU.NMTIME.TM2', 'INIT', ' ')
            zr(jtdeb+6-1) = 0.d0
        else
            call assert(.false.)
        endif
!
    else if (phase.eq.'RUN') then
!
! ----- LANCEMENT DES TIMERS
!
        if (timer .eq. 'PAS') then
            call uttcpu('CPU.NMTIME.PAS', 'DEBUT', ' ')
        else if (timer.eq.'ITE') then
            call uttcpu('CPU.NMTIME.ITE', 'DEBUT', ' ')
        else if (timer.eq.'ARC') then
            call uttcpu('CPU.NMTIME.ARC', 'DEBUT', ' ')
        else if (timer.eq.'POST_TRAITEMENT') then
            call uttcpu('CPU.NMTIME.PST', 'DEBUT', ' ')
            elseif ((timer.eq.'INTEGRATION').or. (timer.eq.'FACTOR').or.&
        (timer.eq.'SECO_MEMB').or. (timer.eq.'SOLVE').or. (&
        timer.eq.'CONT_GEOM').or. (timer.eq.'CTCD_ALGO').or. (&
        timer.eq.'CTCC_PREP').or. (timer.eq.'CTCC_MATR').or. (&
        timer.eq.'CTCC_VECT').or. (timer.eq.'CTCC_CONT').or. (&
        timer.eq.'CTCC_FROT')) then
            call uttcpu('CPU.NMTIME.TM1', 'DEBUT', ' ')
        else if (timer.eq.'ASSE_MATR') then
            call uttcpu('CPU.NMTIME.TM2', 'DEBUT', ' ')
        else
            call assert(.false.)
        endif
!
    else if (phase.eq.'END') then
!
! ----- ARRET DES TIMERS
!
        if (timer .eq. 'PAS') then
            call uttcpu('CPU.NMTIME.PAS', 'FIN', ' ')
            call uttcpr('CPU.NMTIME.PAS', 4, zr(jtpas))
            time = zr(jtpas+3-1) - zr(jtdeb+1-1)
            zr(jtdeb+1-1) = zr(jtpas+3-1)
            call nmrtim(sdtime, timer, time)
        else if (timer.eq.'ITE') then
            call uttcpu('CPU.NMTIME.ITE', 'FIN', ' ')
            call uttcpr('CPU.NMTIME.ITE', 4, zr(jtite))
            time = zr(jtite+3-1) - zr(jtdeb+2-1)
            zr(jtdeb+2-1) = zr(jtite+3-1)
            call nmrtim(sdtime, timer, time)
        else if (timer.eq.'ARC') then
            call uttcpu('CPU.NMTIME.ARC', 'FIN', ' ')
            call uttcpr('CPU.NMTIME.ARC', 4, zr(jtarc))
            time = zr(jtarc+3-1) - zr(jtdeb+3-1)
            zr(jtdeb+3-1) = zr(jtarc+3-1)
            call nmrtim(sdtime, timer, time)
        else if (timer.eq.'POST_TRAITEMENT') then
            call uttcpu('CPU.NMTIME.PST', 'FIN', ' ')
            call uttcpr('CPU.NMTIME.PST', 4, zr(jtpst))
            time = zr(jtpst+3-1) - zr(jtdeb+4-1)
            zr(jtdeb+4-1) = zr(jtpst+3-1)
            call nmrtim(sdtime, timer, time)
            elseif ((timer.eq.'INTEGRATION').or. (timer.eq.'FACTOR').or.&
        (timer.eq.'SECO_MEMB').or. (timer.eq.'SOLVE').or. (&
        timer.eq.'CONT_GEOM').or. (timer.eq.'CTCD_ALGO').or. (&
        timer.eq.'CTCC_PREP').or. (timer.eq.'CTCC_MATR').or. (&
        timer.eq.'CTCC_VECT').or. (timer.eq.'CTCC_CONT').or. (&
        timer.eq.'CTCC_FROT')) then
            call uttcpu('CPU.NMTIME.TM1', 'FIN', ' ')
            call uttcpr('CPU.NMTIME.TM1', 4, zr(jtmp1))
            time = zr(jtmp1+3-1) - zr(jtdeb+5-1)
            zr(jtdeb+5-1) = 0.d0
            call nmrtim(sdtime, timer, time)
        else if (timer.eq.'ASSE_MATR') then
            call uttcpu('CPU.NMTIME.TM2', 'FIN', ' ')
            call uttcpr('CPU.NMTIME.TM2', 4, zr(jtmp2))
            time = zr(jtmp2+3-1) - zr(jtdeb+6-1)
            zr(jtdeb+6-1) = 0.d0
            call nmrtim(sdtime, timer, time)
        else
            call assert(.false.)
        endif
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
