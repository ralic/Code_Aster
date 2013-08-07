subroutine cpclma(nomain, nomaou, typcol, base)
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
    character(len=1) :: base
    character(len=8) :: nomain, nomaou, typcol
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
! person_in_charge: nicolas.sellenet at edf.fr
!
#include "jeveux.h"
!
    integer :: nbgmai, igroup, jnuma1, jnuma2, nbmail, jmaill
    integer :: codret
!
    character(len=24) :: grmain, grmaou, ptnmou, nomgrp
!
!     RECOPIE DES COLLECTIONS .GROUPEMA ET .GROUPENO
!
    call jemarq()
!
    if (typcol .eq. 'GROUPEMA') then
        grmain = nomain//'.GROUPEMA'
        grmaou = nomaou//'.GROUPEMA'
        ptnmou = nomaou//'.PTRNOMMAI'
    else if (typcol.eq.'GROUPENO') then
        grmain = nomain//'.GROUPENO'
        grmaou = nomaou//'.GROUPENO'
        ptnmou = nomaou//'.PTRNOMNOE'
    else
        ASSERT(.false.)
    endif
!
    call jedetr(grmaou)
    call jedetr(ptnmou)
!
    call jeexin(grmain, codret)
    if (codret .eq. 0) goto 9999
    call jelira(grmain, 'NOMUTI', nbgmai)
    call jecreo(ptnmou, base//' N K24')
    call jeecra(ptnmou, 'NOMMAX', nbgmai)
    call jecrec(grmaou, base//' V I', 'NO '//ptnmou, 'DISPERSE', 'VARIABLE',&
                nbgmai)
!
    do 10 igroup = 1, nbgmai
        call jenuno(jexnum(grmain, igroup), nomgrp)
        call jecroc(jexnom(grmaou, nomgrp))
        call jeveuo(jexnum(grmain, igroup), 'L', jnuma1)
        call jelira(jexnum(grmain, igroup), 'LONUTI', nbmail)
        call jeecra(jexnom(grmaou, nomgrp), 'LONMAX', max(nbmail,1))
        call jeecra(jexnom(grmaou, nomgrp), 'LONUTI', nbmail)
        call jeveuo(jexnom(grmaou, nomgrp), 'E', jnuma2)
        do 20 jmaill = 0, nbmail-1
            zi(jnuma2+jmaill) = zi(jnuma1+jmaill)
20      continue
10  end do
!
9999  continue
!
    call jedema()
!
end subroutine
