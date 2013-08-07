subroutine eclau1(nomte, famil, elrefa, fapg)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elref2.h"
#include "asterfort/indk32.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=8) :: famil, elrefa, fapg
    character(len=16) :: nomte
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! -----------------------------------------------------------
! IN  : NOMTE  : NOM DU TYPE D'ELEMENT (K16)
!       FAMIL  : NOM (LOCAL) DE LA FAMILLE DE POINTS DE GAUSS :
!                'RIGI', 'MASS', ...
! OUT : ELREFA : NOM DE L'ELREFA   (OU ' ')
!       FAPG   : FAMILLE DE POINT DE GAUSS  (OU ' ')
! -----------------------------------------------------------
!
    integer :: ntrou, jpnlfp, jnolfp, nblfpg, nuflpg, nufgpg
    character(len=8) :: lielrf(10)
    character(len=16) :: nofgpg
    character(len=32) :: noflpg
! DEB ------------------------------------------------------------------
!
    fapg = ' '
    elrefa = ' '
!
    call elref2(nomte, 10, lielrf, ntrou)
    if (ntrou .eq. 0) goto 9999
    elrefa = lielrf(1)
!
    noflpg = nomte//elrefa//famil
!
    call jeveuo('&CATA.TE.PNLOCFPG', 'L', jpnlfp)
    call jeveuo('&CATA.TE.NOLOCFPG', 'L', jnolfp)
    call jelira('&CATA.TE.NOLOCFPG', 'LONMAX', nblfpg)
!
    nuflpg = indk32(zk32(jpnlfp),noflpg,1,nblfpg)
    if (nuflpg .gt. 0) then
        nufgpg = zi(jnolfp-1+nuflpg)
!
!        -- POUR LES FAMILLES "LISTE" : ON NE SAIT PAS FAIRE => FAPG=' '
        if (nufgpg .eq. 0) goto 9999
!
        call jenuno(jexnum('&CATA.TM.NOFPG', nufgpg), nofgpg)
        fapg = nofgpg(9:16)
!
    else
        ASSERT(.false.)
    endif
!
9999  continue
end subroutine
