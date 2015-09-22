subroutine gtgrma(main, nmgrma, lima, nbma)
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

    implicit none
#include "jeveux.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/jelira.h"
#include "asterfort/as_allocate.h"

    character(len=8), intent(in) :: main
    character(len=24), intent(in) :: nmgrma
    integer, pointer :: lima(:)
    integer, intent(out) :: nbma
! -------------------------------------------------------------------------------------------------
!   RECUPERATION DE LA LISTE DES NUMERO DE MAILLE DU GROUP NMGRMA
! -------------------------------------------------------------------------------------------------
    integer :: jgrma, inc
! -------------------------------------------------------------------------------------------------
    call jemarq()
!

        call jelira(jexnom(main//'.GROUPEMA', nmgrma),'LONUTI',nbma)
        call jeveuo(jexnom(main//'.GROUPEMA', nmgrma),'L',jgrma)
        AS_ALLOCATE(vi=lima, size=nbma)
        do inc=1, nbma
            lima(inc)=zi(jgrma+inc-1)
        end do

!
    call jedema()
end subroutine
