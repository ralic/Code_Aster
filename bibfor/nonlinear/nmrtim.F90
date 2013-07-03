subroutine nmrtim(sdtime, timerz, time)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: timerz
    character(len=24) :: sdtime
    real(kind=8) :: time
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MESURE DE STATISTIQUES - SAUVEGARDE DES TEMPS
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
! IN  TIME   : VALEUR DU TEMPS MESURE
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
    call jeveuo(timet, 'E', jtimet)
    call jeveuo(timep, 'E', jtimep)
    call jeveuo(timen, 'E', jtimen)
!
! --- ENREGISTREMENT DES TEMPS
!
    if (timer .eq. 'PAS') then
        zr(jtimet-1+1) = zr(jtimet-1+1) + time
        zr(jtimep-1+1) = zr(jtimep-1+1) + time
!
    else if (timer.eq.'ITE') then
        zr(jtimet-1+2) = zr(jtimet-1+2) + time
        zr(jtimep-1+2) = zr(jtimep-1+2) + time
        zr(jtimen-1+2) = zr(jtimen-1+2) + time
!
    else if (timer.eq.'ARC') then
        zr(jtimet-1+3) = zr(jtimet-1+3) + time
        zr(jtimep-1+3) = zr(jtimep-1+3) + time
        zr(jtimen-1+3) = zr(jtimen-1+3) + time
!
    else if (timer.eq.'INTEGRATION') then
        zr(jtimet-1+4) = zr(jtimet-1+4) + time
        zr(jtimep-1+4) = zr(jtimep-1+4) + time
        zr(jtimen-1+4) = zr(jtimen-1+4) + time
!
    else if (timer.eq.'ASSE_MATR') then
        zr(jtimet-1+5) = zr(jtimet-1+5) + time
        zr(jtimep-1+5) = zr(jtimep-1+5) + time
        zr(jtimen-1+5) = zr(jtimen-1+5) + time
!
    else if (timer.eq.'FACTOR') then
        zr(jtimet-1+6) = zr(jtimet-1+6) + time
        zr(jtimep-1+6) = zr(jtimep-1+6) + time
        zr(jtimen-1+6) = zr(jtimen-1+6) + time
!
    else if (timer.eq.'SECO_MEMB') then
        zr(jtimet-1+7) = zr(jtimet-1+7) + time
        zr(jtimep-1+7) = zr(jtimep-1+7) + time
        zr(jtimen-1+7) = zr(jtimen-1+7) + time
!
    else if (timer.eq.'SOLVE') then
        zr(jtimet-1+8) = zr(jtimet-1+8) + time
        zr(jtimep-1+8) = zr(jtimep-1+8) + time
        zr(jtimen-1+8) = zr(jtimen-1+8) + time
!
    else if (timer.eq.'CONT_GEOM') then
        zr(jtimet-1+9) = zr(jtimet-1+9) + time
        zr(jtimep-1+9) = zr(jtimep-1+9) + time
        zr(jtimen-1+9) = zr(jtimen-1+9) + time
!
    else if (timer.eq.'CTCD_ALGO') then
        zr(jtimet-1+10) = zr(jtimet-1+10) + time
        zr(jtimep-1+10) = zr(jtimep-1+10) + time
        zr(jtimen-1+10) = zr(jtimen-1+10) + time
!
    else if (timer.eq.'CTCC_PREP') then
        zr(jtimet-1+11) = zr(jtimet-1+11) + time
        zr(jtimep-1+11) = zr(jtimep-1+11) + time
        zr(jtimen-1+11) = zr(jtimen-1+11) + time
!
    else if (timer.eq.'CTCC_MATR') then
        zr(jtimet-1+12) = zr(jtimet-1+12) + time
        zr(jtimep-1+12) = zr(jtimep-1+12) + time
        zr(jtimen-1+12) = zr(jtimen-1+12) + time
!
    else if (timer.eq.'CTCC_VECT') then
        zr(jtimet-1+13) = zr(jtimet-1+13) + time
        zr(jtimep-1+13) = zr(jtimep-1+13) + time
        zr(jtimen-1+13) = zr(jtimen-1+13) + time
!
    else if (timer.eq.'CTCC_CONT') then
        zr(jtimet-1+14) = zr(jtimet-1+14) + time
        zr(jtimep-1+14) = zr(jtimep-1+14) + time
        zr(jtimen-1+14) = zr(jtimen-1+14) + time
!
    else if (timer.eq.'CTCC_FROT') then
        zr(jtimet-1+15) = zr(jtimet-1+15) + time
        zr(jtimep-1+15) = zr(jtimep-1+15) + time
        zr(jtimen-1+15) = zr(jtimen-1+15) + time
!
    else if (timer.eq.'POST_TRAITEMENT') then
        zr(jtimet-1+16) = zr(jtimet-1+16) + time
        zr(jtimep-1+16) = zr(jtimep-1+16) + time
        zr(jtimen-1+16) = zr(jtimen-1+16) + time
!
    else if (timer.eq.'PAS_LOST') then
        zr(jtimet-1+17) = zr(jtimet-1+17) + time
        zr(jtimep-1+17) = zr(jtimep-1+17) + time
        zr(jtimen-1+17) = zr(jtimen-1+17) + time
!
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
