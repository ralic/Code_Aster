subroutine nmtima(sdtime, timer, vali)
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
    character(len=24), intent(in) :: sdtime
    character(len=3), intent(in) :: timer
    integer, intent(out) :: vali
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! GESTION DES TIMERS - MESURE DU TEMPS RESTANT
!
! ----------------------------------------------------------------------
!
!
! IN  TIMER  : NOM DU TIMER
!                'PAS'   TIMER POUR UN PAS DE TEMPS
!                'ITE'   TIMER POUR UNE ITERATION DE NEWTON
! IN  SDTIME : SD TIMER
! OUT VALI   : RESULTAT DE L'ACTION
!               0 ON CONTINUE
!               1 ON S'ARRETE
!
!
    character(len=24) :: timpas, timite, timarc
    integer :: jtpas, jtite, jtarc
    real(kind=8) :: tpsrst, moyarc, moyite, moypas
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    vali = 0
!
! --- ACCES SD TIMER
!
    timpas = sdtime(1:19)//'.TPAS'
    timite = sdtime(1:19)//'.TITE'
    timarc = sdtime(1:19)//'.TARC'
    call jeveuo(timpas, 'E', jtpas)
    call jeveuo(timite, 'E', jtite)
    call jeveuo(timarc, 'E', jtarc)
!
! --- TEMPS MOYENS
!
    moyarc = zr(jtarc+4-1)
    moyite = zr(jtite+4-1)
    moypas = zr(jtpas+4-1)
!
! --- MESURE DES TEMPS RESTANTS
!
    if (timer .eq. 'ITE') then
        tpsrst = zr(jtite+1-1)
        if ((2.d0*moyite) .le. (0.95d0*tpsrst-moyarc)) then
            vali = 0
        else
            vali = 1
        endif
    else if (timer.eq.'PAS') then
        tpsrst = zr(jtpas+1-1)
        if (moypas .le. 0.90d0*tpsrst) then
            vali = 0
        else
            vali = 1
        endif
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
