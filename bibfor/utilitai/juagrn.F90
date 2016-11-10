subroutine juagrn(nom, long)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nom
    integer :: long
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     Redimensionnement d'un repertoire de nom
!     ------------------------------------------------------------------
! IN  NOM  : K24 : NOM DE L'OBJET A REDIMENSIONNER
! IN  LONG : I   : NOUVELLE LONGUEUR DU VECTEUR
!     ------------------------------------------------------------------
!
!
    character(len=8) :: type, no1
    character(len=16) :: no2
    character(len=24) :: no3
    character(len=32) :: no4
    character(len=80) :: no5
!
    integer :: lonma2, lonmax, iaux, jadr, ltyp
!-----------------------------------------------------------------------
    call jemarq()
    call jelira(nom, 'NOMMAX', lonmax)
    call jelira(nom, 'TYPE  ', cval=type)
    if (type(1:1) .ne. 'K') then
        ASSERT(.false.)
    else
        call jelira(nom, 'LTYP', ltyp)
        call codent(ltyp, 'G', type(2:))
        call wkvect('&&JEAGRN.TMP', 'V V '//type, long, jadr)
    endif
    lonma2 = min(lonmax, long)
    if (ltyp .eq. 8) then
        do iaux = 1, lonma2
            call jenuno(jexnum(nom, iaux), no1)
            zk8(jadr+iaux-1) = no1
        enddo
    else if (ltyp .eq. 16) then
        do iaux = 1, lonma2
            call jenuno(jexnum(nom, iaux), no2)
            zk16(jadr+iaux-1) = no2
        enddo
    else if (ltyp .eq. 24) then
        do iaux = 1, lonma2
            call jenuno(jexnum(nom, iaux), no3)
            zk24(jadr+iaux-1) = no3
        enddo
    else if (ltyp .eq. 32) then
        do iaux = 1, lonma2
            call jenuno(jexnum(nom, iaux), no4)
            zk32(jadr+iaux-1) = no4
        enddo
    else if (ltyp .eq. 80) then
        do iaux = 1, lonma2
            call jenuno(jexnum(nom, iaux), no5)
            zk80(jadr+iaux-1) = no5
        enddo
    else
        ASSERT(.false.)
    endif
!
    call jedetr(nom)
    call jecreo(nom, 'V N '//type)
    call jeecra(nom, 'NOMMAX', ival=long)
!
    if (ltyp .eq. 8) then
        do iaux = 1, lonma2
            call jecroc(jexnom(nom, zk8(jadr+iaux-1)))
        enddo
    else if (ltyp .eq. 16) then
        do iaux = 1, lonma2
            call jecroc(jexnom(nom, zk16(jadr+iaux-1)))
        enddo
    else if (ltyp .eq. 24) then
        do iaux = 1, lonma2
            call jecroc(jexnom(nom, zk24(jadr+iaux-1)))
        enddo
    else if (ltyp .eq. 32) then
        do iaux = 1, lonma2
            call jecroc(jexnom(nom, zk32(jadr+iaux-1)))
        enddo
    else if (ltyp .eq. 80) then
        do iaux = 1, lonma2
            call jecroc(jexnom(nom, zk80(jadr+iaux-1)))
        enddo
    endif
!
    call jedetr('&&JEAGRN.TMP')
    call jedema()
end subroutine
