subroutine nmrini(sdtime, sdstat, phase)
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
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: sdtime, sdstat
    character(len=1) :: phase
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MESURE DE STATISTIQUES - REMISE A ZERO
!
! ----------------------------------------------------------------------
!
!
! IN  SDSTAT : SD STATISTIQUES
! IN  SDTIME : SD TIMER
! IN  PHASE  : PHASE
!               'N' SUR L'ITERATION DE NEWTON COURANTE
!               'P' SUR LE PAS COURANT
!               'T' SUR TOUT LE TRANSITOIRE
!
!
!
!
    character(len=24) :: timet, timep, timen
    integer :: jtimet, jtimep, jtimen
    character(len=24) :: stvip, stvit, stvin
    integer :: jstvip, jstvit, jstvin
    integer :: i, nmaxt, nmaxs
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SDTIME
!
    timet = sdtime(1:19)//'.TIMT'
    timep = sdtime(1:19)//'.TIMP'
    timen = sdtime(1:19)//'.TIMN'
    call jeveuo(timet, 'E', jtimet)
    call jeveuo(timep, 'E', jtimep)
    call jeveuo(timen, 'E', jtimen)
    call jelira(timet, 'LONMAX', nmaxt, k8bid)
!
! --- ACCES SDSTAT
!
    stvip = sdstat(1:19)//'.VLIP'
    stvit = sdstat(1:19)//'.VLIT'
    stvin = sdstat(1:19)//'.VLIN'
    call jeveuo(stvip, 'E', jstvip)
    call jeveuo(stvit, 'E', jstvit)
    call jeveuo(stvin, 'E', jstvin)
    call jelira(stvit, 'LONMAX', nmaxs, k8bid)
!
! --- ENREGISTREMENT DES TEMPS
!
    if (phase .eq. 'T') then
        do 10 i = 1, nmaxt
            zr(jtimet+i-1) = 0.d0
            zr(jtimep+i-1) = 0.d0
            zr(jtimen+i-1) = 0.d0
10      continue
        do 20 i = 1, nmaxs
            zi(jstvit+i-1) = 0
            zi(jstvip+i-1) = 0
            zi(jstvin+i-1) = 0
20      continue
    else if (phase.eq.'P') then
        do 11 i = 1, nmaxt
            zr(jtimep+i-1) = 0.d0
            zr(jtimen+i-1) = 0.d0
11      continue
        do 21 i = 1, nmaxs
            zi(jstvip+i-1) = 0
            zi(jstvin+i-1) = 0
21      continue
    else if (phase.eq.'N') then
        do 12 i = 1, nmaxt
            zr(jtimen+i-1) = 0.d0
12      continue
        do 22 i = 1, nmaxs
            zi(jstvin+i-1) = 0
22      continue
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
