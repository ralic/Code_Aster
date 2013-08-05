subroutine nmdcax(sddisc, insref, numins, durdec, deltac)
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
    implicit     none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/diinst.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesr.h"
    character(len=19) :: sddisc
    integer :: numins
    real(kind=8) :: durdec, insref, deltac
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
!
! EXTENSION DE LA DECOUPE AUX INSTANTS SUIVANTS - AUTOMATIQUE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  INSREF : INSTANT AU-DELA DUQUEL ON ETEND LA DECOUPE
! IN  DURDEC : DUREEE DE L'EXTENSION DE LA DECOUPE
! IN  DELTAC : INCREMENT DE TEMPS CIBLE
!
!
!
!
    real(kind=8) :: valr(2)
    character(len=24) :: tpsext, tpsdit
    integer :: jtpsex
    real(kind=8) :: instap, instam, inst
    real(kind=8) :: insfin
    real(kind=8) :: oldref
    integer :: nummax, nbrpas
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- PROCHAIN INSTANT INSTAP
!
    tpsdit = sddisc(1:19)//'.DITR'
    call jelira(tpsdit, 'LONMAX', nummax, k8bid)
    instam = diinst(sddisc,numins)
    if (numins .eq. nummax) then
        instap = instam+deltac
    else
        instap = diinst(sddisc,numins+1)
    endif
!
! --- VALEURS STOCKEES
!
    tpsext = sddisc(1:19)//'.AEXT'
    call jeveuo(tpsext, 'E', jtpsex)
    ASSERT(durdec.gt.0.d0)
    oldref = zr(jtpsex-1+1)
!
! --- PREMIERE EXTENSION
!
    if (oldref .eq. r8vide()) then
        nbrpas = 1
        insfin = insref+durdec
!
! ----- RECHERCHE DE LA "VRAIE FIN"
!
11      continue
        inst = insref + nbrpas*deltac
        if (inst .gt. insfin) then
            nbrpas = nbrpas - 1
            goto 12
        else
            nbrpas = nbrpas + 1
        endif
        goto 11
!
12      continue
        insfin = insref+deltac*nbrpas
    endif
!
! --- EXTENSION
!
    if (oldref .ne. r8vide()) then
        insfin = zr(jtpsex-1+3)
        if (instap .le. insfin) then
            insref = instap
        endif
    endif
!
! --- SAUVEGARDE
!
    zr(jtpsex-1+1) = insref
    zr(jtpsex-1+2) = deltac
    zr(jtpsex-1+3) = insfin
!
! --- AFFICHAGE
!
    valr(1) = insref
    valr(2) = durdec
    call u2mesr('I', 'SUBDIVISE_18', 2, valr)
!
    call jedema()
end subroutine
