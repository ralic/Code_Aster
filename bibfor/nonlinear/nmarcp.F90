subroutine nmarcp(typost, sdpost, vecmod, freqr, imode)
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
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmlesd.h"
    character(len=4) :: typost
    character(len=19) :: sdpost
    character(len=19) :: vecmod
    real(kind=8) :: freqr
    integer :: imode
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - ARCHIVAGE)
!
! VECTEUR DE DEPL. POUR LE MODE
!
! ----------------------------------------------------------------------
!
!
! IN  TYPOST : TYPE DE POST-TRAITEMENTS (STABILITE OU MODE_VIBR)
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! OUT VECMOD : VECTEUR DE DEPLACEMENT POUR LE MODE
! OUT FREQR  : FREQUENCE ATTACHEE AU MODE
! OUT IMODE  : VAUT ZEOR S'IL N'Y A PAS DE MODE
!
!
!
!
    integer :: ibid, numord
    character(len=24) :: k24bid
    real(kind=8) :: r8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- MODE SELECTIONNE: INFOS DANS SDPOST
!
    if (typost .eq. 'VIBR') then
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_FREQ_VIBR', ibid, freqr,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_NUME_VIBR', numord, r8bid,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_VIBR', ibid, r8bid,&
                    vecmod)
    else if (typost .eq. 'FLAM') then
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_FREQ_FLAM', ibid, freqr,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_NUME_FLAM', numord, r8bid,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_FLAM', ibid, r8bid,&
                    vecmod)
    else if (typost .eq. 'STAB') then
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_FREQ_STAB', ibid, freqr,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_NUME_STAB', numord, r8bid,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_STAB', ibid, r8bid,&
                    vecmod)
    else
        ASSERT(.false.)
    endif
!
! --- EXTRACTION DU MODE
!
    if (freqr .eq. r8vide()) then
        imode = 0
    else
        imode = 1
    endif
!
    call jedema()
end subroutine
