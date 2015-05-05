subroutine nmdecv(sddisc, nume_inst, i_event_acti, dtmin, retdec)
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
#include "asterc/r8prem.h"
#include "asterfort/dinins.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
    character(len=19) :: sddisc
    integer :: nume_inst, i_event_acti, retdec
    real(kind=8) :: dtmin
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
!
! VERIFICATIONS DE LA DECOUPE
!
! ----------------------------------------------------------------------
!
!
! In  sddisc           : datastructure for time discretization
! IN  NUMINS : NUMERO DE L'INSTANT COURANT
! In  i_event_acti     : index of active event
! IN  DTMIN  : INTERVALLE DE TEMPS MINIMAL SUR LA LISTE CREEE
! OUT RETDEC : CODE RETOUR DECOUPE
!               0 ECHEC DE LA DECOUPE
!               1 ON A DECOUPE
!               2 PAS DE DECOUPE
!
! ----------------------------------------------------------------------
!
    integer :: nbnivo, lenivo
    real(kind=8) :: pasmin
!
! ----------------------------------------------------------------------
!
!
! --- NIVEAU DE REDECOUPAGE ACTUEL
!
    lenivo = dinins(sddisc,nume_inst)
!
! --- NIVEAU MAXI DE SUBDIVISION
!
    call utdidt('L', sddisc, 'ECHE', 'SUBD_NIVEAU', index_ = i_event_acti, &
                vali_ = nbnivo)
!
! --- PAS MINIMUM
!
    call utdidt('L', sddisc, 'ECHE', 'SUBD_PAS_MINI', index_ = i_event_acti, &
                valr_ = pasmin)
!
! --- TAILLE DE PAS MINIMALE ATTEINTE PENDANT LA SUBDIVISION
!
    if ((dtmin .lt. pasmin) .or. (dtmin.le.r8prem())) then
        retdec = 0
        call utmess('I', 'SUBDIVISE_16', sr=pasmin)
        goto 999
    else
        retdec = 1
    endif
!
! --- NIVEAU MAXIMUM DE REDECOUPAGE ATTEINT
!
    if (( nbnivo .gt. 1 ) .and. (lenivo.eq.nbnivo)) then
        call utmess('I', 'SUBDIVISE_17', si=lenivo)
        retdec = 0
    else
        retdec = 1
    endif
!
999 continue

end subroutine
