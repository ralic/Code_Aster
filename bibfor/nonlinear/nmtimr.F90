subroutine nmtimr(sdtime, timerz, phase, valr)
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
    character(len=24) :: sdtime
    character(len=*) :: timerz
    character(len=1) :: phase
    real(kind=8) :: valr
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! GESTION DES TIMERS - LECTURE INFORMATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  SDTIME : SD TIMER
! IN  TIMER  : NOM DU TIMER
!              'PAS'               TIMER PAS DE TEMPS
!              'ITE'               TIMER ITERATION DE NEWTON
!              'ARC'               TIMER ARCHIVAGE
!              'INTEGRATION'       TIMER INTEG. LDC
!              'ASSE_MATR'         TIMER ASSEMBLAGE MATRICES
!              'FACTOR'            TIMER FACTORISATION
!              'SECO_MEMB'         TIMER CALCUL SECOND MEMBRE
!              'SOLVE'             TIMER RESOLUTION
!              'CONT_GEOM'         TIMER APPARIEMENT CONTACT
!              'CTCD_ALGO'         TIMER RESOLUTION CONTACT DISCRET
!              'CTCC_PREP'         TIMER PREPARATION CONTACT CONTINU
!              'CTCC_MATR'         TIMER CALCUL MATRICE CONTINU
!              'CTCC_VECT'         TIMER CALCUL VECTEUR CONTINU
!              'CTCC_CONT'         TIMER RESOLUTION CONTACT CONTINU
!              'CTCC_FROT'         TIMER RESOLUTION FROTTEMENT CONTINU
!              'POST_TRAITEMENT'   TIMER POST_PROCESSING
!              'PAS_LOST'          TEMPS PERDU DANS PAS (DECOUPE)
! IN  PHASE  : TYPE DE VALEUR EN LECTURE
!              'N' SUR L'ITERATION DE NEWTON COURANTE
!              'P' SUR LE PAS COURANT
!              'T' SUR TOUT LE TRANSITOIRE
! OUT VALR   : TEMPS MESURE
!
!
!
!
    character(len=24) :: timet, timep, timen
    integer :: jtimet, jtimep, jtimen
    character(len=24) :: timer
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    timer = timerz
!
! --- ACCES SDTIME: TOTAL/PAS/NEWTON
!
    timet = sdtime(1:19)//'.TIMT'
    timep = sdtime(1:19)//'.TIMP'
    timen = sdtime(1:19)//'.TIMN'
    call jeveuo(timet, 'L', jtimet)
    call jeveuo(timep, 'L', jtimep)
    call jeveuo(timen, 'L', jtimen)
!
    if (timer .eq. 'TEMPS_PHASE') then
        if (phase .eq. 'T') valr = zr(jtimet-1+1)
        if (phase .eq. 'P') valr = zr(jtimep-1+1)
        if (phase .eq. 'N') valr = zr(jtimen-1+2)
!
    else if (timer.eq.'INTEGRATION') then
        if (phase .eq. 'T') valr = zr(jtimet-1+4)
        if (phase .eq. 'P') valr = zr(jtimep-1+4)
        if (phase .eq. 'N') valr = zr(jtimen-1+4)
!
    else if (timer.eq.'ASSE_MATR') then
        if (phase .eq. 'T') valr = zr(jtimet-1+5)
        if (phase .eq. 'P') valr = zr(jtimep-1+5)
        if (phase .eq. 'N') valr = zr(jtimen-1+5)
!
    else if (timer.eq.'FACTOR') then
        if (phase .eq. 'T') valr = zr(jtimet-1+6)
        if (phase .eq. 'P') valr = zr(jtimep-1+6)
        if (phase .eq. 'N') valr = zr(jtimen-1+6)
!
    else if (timer.eq.'SECO_MEMB') then
        if (phase .eq. 'T') valr = zr(jtimet-1+7)
        if (phase .eq. 'P') valr = zr(jtimep-1+7)
        if (phase .eq. 'N') valr = zr(jtimen-1+7)
!
    else if (timer.eq.'SOLVE') then
        if (phase .eq. 'T') valr = zr(jtimet-1+8)
        if (phase .eq. 'P') valr = zr(jtimep-1+8)
        if (phase .eq. 'N') valr = zr(jtimen-1+8)
!
    else if (timer.eq.'CONT_GEOM') then
        if (phase .eq. 'T') valr = zr(jtimet-1+9)
        if (phase .eq. 'P') valr = zr(jtimep-1+9)
        if (phase .eq. 'N') valr = zr(jtimen-1+9)
!
    else if (timer.eq.'CTCD_ALGO') then
        if (phase .eq. 'T') valr = zr(jtimet-1+10)
        if (phase .eq. 'P') valr = zr(jtimep-1+10)
        if (phase .eq. 'N') valr = zr(jtimen-1+10)
!
    else if (timer.eq.'CTCC_PREP') then
        if (phase .eq. 'T') valr = zr(jtimet-1+11)
        if (phase .eq. 'P') valr = zr(jtimep-1+11)
        if (phase .eq. 'N') valr = zr(jtimen-1+11)
!
    else if (timer.eq.'CTCC_MATR') then
        if (phase .eq. 'T') valr = zr(jtimet-1+12)
        if (phase .eq. 'P') valr = zr(jtimep-1+12)
        if (phase .eq. 'N') valr = zr(jtimen-1+12)
!
    else if (timer.eq.'CTCC_VECT') then
        if (phase .eq. 'T') valr = zr(jtimet-1+13)
        if (phase .eq. 'P') valr = zr(jtimep-1+13)
        if (phase .eq. 'N') valr = zr(jtimen-1+13)
!
    else if (timer.eq.'CTCC_CONT') then
        if (phase .eq. 'T') valr = zr(jtimet-1+14)
        if (phase .eq. 'P') valr = zr(jtimep-1+14)
        if (phase .eq. 'N') valr = zr(jtimen-1+14)
!
    else if (timer.eq.'CTCC_FROT') then
        if (phase .eq. 'T') valr = zr(jtimet-1+15)
        if (phase .eq. 'P') valr = zr(jtimep-1+15)
        if (phase .eq. 'N') valr = zr(jtimen-1+15)
!
    else if (timer.eq.'POST_TRAITEMENT') then
        if (phase .eq. 'T') valr = zr(jtimet-1+16)
        if (phase .eq. 'P') valr = zr(jtimep-1+16)
        if (phase .eq. 'N') valr = zr(jtimen-1+16)
!
    else if (timer.eq.'PAS_LOST') then
        if (phase .eq. 'T') valr = zr(jtimet-1+17)
        if (phase .eq. 'P') call assert(.false.)
        if (phase .eq. 'N') call assert(.false.)
!
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
