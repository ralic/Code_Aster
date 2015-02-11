subroutine te0580(nomopt, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/utmess.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/assert.h"
!     ------------------------------------------------------------------
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
    character(len=16) :: nomte, nomopt

!-----------------------------------------------------------------------
    character(len=1) :: code
    integer :: jad, itab(8), nbv, iret, k
    character(len=24) :: valk(2)
!-----------------------------------------------------------------------
! Cette routine realise les calculs elementaires "triviaux" qui ne sont pas
! encore programmes par les elements.
! Par exemple les chargements de Neuman nuls.
!-----------------------------------------------------------------------

    if (nomopt.eq.'CHAR_MECA_PRES_R') then
        call tecach('OOO', 'PPRESSR', 'L', iret, nval=8, itab=itab)
        ASSERT(iret.eq.0)
        jad=itab(1)
        nbv=itab(2)
        do k=1,nbv
            if (zr(jad-1+k).ne.0.d0) goto 998
        enddo

    elseif (nomopt.eq.'CHAR_MECA_PRES_F') then
        call tecach('OOO', 'PPRESSF', 'L', iret, nval=8, itab=itab)
        ASSERT(iret.eq.0)
        jad=itab(1)
        nbv=itab(2)
        do k=1,nbv
            if (zk8(jad-1+k).ne.'&FOZERO') goto 998
        enddo

    else
        ASSERT(.false.)
    endif
    goto 999


!   -- erreur :
998 continue
    valk(1)=nomte
    valk(2)=nomopt
    code='F'

!   -- le bloc if suivant sera a retirer apres la correction de issue23503
    if (nomopt(1:14).eq.'CHAR_MECA_PRES') then
        if (nomte.eq.'HM_J_AXSE3'.or.nomte.eq.'HM_J_DPSE3') code='A'
    endif
    call utmess(code, 'CALCULEL_44',2,valk=valk)


!   -- sortie normale :
999 continue

end subroutine
