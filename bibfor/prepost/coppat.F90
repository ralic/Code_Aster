subroutine coppat(main, maout, nbma, nbpain)
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
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/jenonu.h"
#include "asterfort/jelira.h"

    character(len=8), intent(in) :: main
    character(len=8), intent(in) :: maout
    integer, intent(in) :: nbma
    integer, intent(out) :: nbpain
! -------------------------------------------------------------------------------------------------
!   RECUPERATION ET MISE A JOUR DE LA COLLECTION .PATH
! -------------------------------------------------------------------------------------------------
    integer :: nbpatch, nbzone, nbinfo, patch
    integer :: nwpath, odpath
    integer :: inc1, inc2

! -------------------------------------------------------------------------------------------------
    call jemarq()
!
    call jedetr(maout//'.PATCH')
    call jelira(main//'.PATCH','NUTIOC',nbpatch)
    nbpain = nbpatch-1
    call jelira(jexnum(main//'.PATCH', 1),'LONUTI' ,nbzone)
    call jecrec(maout//'.PATCH','G V I', 'NU', 'DISPERSE', 'VARIABLE', nbpatch+nbma)
    call jecroc(jexnum(maout//'.PATCH',1))
    call jeecra(jexnum(maout//'.PATCH',1), 'LONMAX', ival=nbzone+2)
    call jeecra(jexnum(maout//'.PATCH',1), 'LONUTI', ival=nbzone+2)
    call jeveuo(jexnum(maout//'.PATCH',1), 'E', patch)
    call jeveuo(jexnum(main//'.PATCH', 1), 'L', odpath)    
    call jelira(jexnum(main//'.PATCH', 1), 'LONUTI', nbinfo)
    do inc2=1,nbinfo
        zi(patch+inc2-1)=zi(odpath+inc2-1)
    end do
    zi(patch+nbzone+1-1)=nbpatch+1
    zi(patch+nbzone+2-1)=nbma
    do inc1=1, nbpatch-1      
        call jeveuo(jexnum(main//'.PATCH', inc1+1), 'L', odpath)    
        call jelira(jexnum(main//'.PATCH', inc1+1), 'LONUTI', nbinfo)
        call jecroc(jexnum(maout//'.PATCH', inc1+1))
        call jeecra(jexnum(maout//'.PATCH', inc1+1), 'LONMAX', nbinfo)
        call jeecra(jexnum(maout//'.PATCH', inc1+1), 'LONUTI', nbinfo)
        call jeveuo(jexnum(maout//'.PATCH', inc1+1), 'E', nwpath)
        do inc2=1, nbinfo 
            zi(nwpath+inc2-1) = zi(odpath+inc2-1)
        end do
    end do
!
    call jedema()
end subroutine
