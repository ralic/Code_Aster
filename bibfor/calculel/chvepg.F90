subroutine chvepg(chel1, chel2)
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
! person_in_charge: jacques.pellet at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celfpg.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
    character(len=19) :: chel1, chel2
!
! ----------------------------------------------------------------------
!
! VERIFIER LA COHERENCE DES FAMILLES DE POINTS DE GAUSS ENTRE DEUX
! CHAM_ELEM
!
! ----------------------------------------------------------------------
!
! IN  CHEL1  : PREMIER CHAM_ELEM
! IN  CHEL2  : PREMIER CHAM_ELEM
!
! ----------------------------------------------------------------------
!
    character(len=24) :: valk(3)
    character(len=8) :: noma, nommai
    integer :: nbma, ima
    integer :: iret1, iret2, ibid
    character(len=16) :: fpg1, fpg2
    character(len=16), pointer :: fapg1(:) => null()
    character(len=16), pointer :: fapg2(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- MAILLAGE ATTACHE
!
    call dismoi('NOM_MAILLA', chel1, 'CHAM_ELEM', repk=noma)
!
! --- EXTRAIRE DE CHEL1 LE SCHEMA DE POINT DE GAUSS DES MAILLES
!
    call celfpg(chel1, '&&CHVEPG.FAPG1', ibid)
    call jeexin('&&CHVEPG.FAPG1', iret1)
    ASSERT(iret1.gt.0)
!
! --- EXTRAIRE DE CHEL2 LE SCHEMA DE POINT DE GAUSS DES MAILLES
!
    call celfpg(chel2, '&&CHVEPG.FAPG2', ibid)
    call jeexin('&&CHVEPG.FAPG2', iret2)
    ASSERT(iret2.gt.0)
!
! --- VERIFIER LA COHERENCE DES FAMILLES DE POINTS DE GAUSS
!
    call jeveuo('&&CHVEPG.FAPG1', 'L', vk16=fapg1)
    call jeveuo('&&CHVEPG.FAPG2', 'L', vk16=fapg2)
    call jelira('&&CHVEPG.FAPG1', 'LONMAX', nbma)
    do ima = 1, nbma
        fpg1 = fapg1(ima)
        fpg2 = fapg2(ima)
        if ((fpg1.ne.' ') .and. (fpg2.ne.' ') .and. (fpg2.ne.fpg1)) then
            call jenuno(jexnum(noma//'.NOMMAI', ima), nommai)
            valk(1) = nommai
            valk(2) = fpg1
            valk(3) = fpg2
            call utmess('F', 'CALCULEL_91', nk=3, valk=valk)
        endif
    end do
!
    call jedetr('&&CHVEPG.FAPG1')
    call jedetr('&&CHVEPG.FAPG2')
    call jedema()
end subroutine
