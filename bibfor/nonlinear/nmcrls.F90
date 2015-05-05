subroutine nmcrls(sddisc, provli, numini, numfin, linsti,&
                  instin, nbtemp, dtmin)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: numini, numfin, nbtemp
    aster_logical :: linsti
    real(kind=8) :: instin
    character(len=19) :: provli
    character(len=19) :: sddisc
    real(kind=8) :: dtmin
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - DISCRETISATION)
!
! RETAILLAGE DE LA LISTE D'INSTANT PROVISOIRE
!
! ----------------------------------------------------------------------
!
! RETAILLE PROVLI SUIVANT [NUMINI,NUMFIN]
!
! IN  PROVLI : NOM DE LA LISTE D'INSTANT PROVISOIRE
! In  sddisc           : datastructure for time discretization
! IN  NUMINI : PREMIER INSTANT
! IN  NUMFIN : DERNIER INSTANT
! IN  LINSTI : .TRUE. SI L'INSTANT INITIAL N'EXISTAIT PAS
! IN  INSTIN : INSTANT INITIAL QUAND ETAT_INIT
! OUT NBTEMP : NOMBE D'INSTANTS DE LA LISTE RETAILLEE
! OUT DTMIN  : INTERVALLE DE TEMPS MINIMUM DE LA LISTE RETAILLEE
!
!
!
!
    integer :: pos, i, nb_inst
    real(kind=8) :: deltat, valr(2)
    character(len=24) :: tpsdit
    integer :: jtemps, jinst
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call utdidt('L', sddisc, 'LIST', 'NBINST',&
                vali_ = nb_inst)
!
! --- NOMBRE FINAL D'INSTANTS
!
    nbtemp = (numfin-numini) + 1
    ASSERT(nbtemp.le.nb_inst)
!
! --- NOMS SD_DISC
!
    tpsdit = sddisc(1:19)//'.DITR'
!
! --- ACCES LISTE D'INSTANTS PROVISOIRE
!
    call jeveuo(provli, 'L', jinst)
!
! --- CREATION DE LA LISTE D'INSTANTS
!
    call wkvect(tpsdit, 'V V R', nbtemp, jtemps)
    pos = 0
    do i = numini, numfin
        zr(jtemps+pos) = zr(jinst+i)
        pos = pos+1
    end do
!
! --- NOUVEL INTERVALLE DE TEMPS MINIMAL : DTMIN
!
    dtmin = r8maem()
    do i = 1, nbtemp-1
        deltat = zr(jtemps-1+i+1) - zr(jtemps-1+i)
        dtmin = min(deltat,dtmin)
    end do
!
! --- SI L'INSTANT INITIAL N'EXISTAIT PAS DANS LA LISTE D'INSTANTS
! --- ON A PRIS PLUS HAUT L'INSTANT LE PLUS PROCHE PRECEDENT : ICI
! --- ON MET LA BONNE VALEUR COMME INSTANT INITIAL
!
    if (linsti) then
        zr(jtemps) = instin
        if (instin .ge. zr(jtemps+1)) then
            valr(1) = instin
            valr(2) = zr(jtemps+1)
            call utmess('F', 'DISCRETISATION_2', nr=2, valr=valr)
        endif
    endif
!
    call jedema()
!
end subroutine
