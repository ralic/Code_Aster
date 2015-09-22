subroutine cpqu08(maout , inc   , jcnnpa, conloc,&
                  limane, jmacou, jmacsu, macou ,&
                  macsu , ind   , ind1  )
!
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
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/cpclma.h"

!
    character(len=8), intent(in) :: maout
    integer, intent(in) :: inc 
    integer, intent(in) :: jcnnpa
    character(len=24), intent(in) :: conloc
    character(len=24), intent(in) :: limane
    integer, intent(in) :: jmacou
    integer, intent(in) :: jmacsu
    integer, intent(in) :: macou
    integer, intent(in) :: macsu
    integer, intent(out) :: ind 
    integer, intent(out) :: ind1    
! -------------------------------------------------------------------------------------------------
!        CREATION DES NOUVEAUS NOUEDS ET NOUVELLE MAILLE CAS QUAD8/SEG3
! -------------------------------------------------------------------------------------------------
! -------------------------------------------------------------------------------------------------
    integer :: patch, ino
    integer :: jlimane, jconloc
! -------------------------------------------------------------------------------------------------
    call jemarq()
!
    call jecroc(jexnum(maout//'.PATCH',inc+1))
    call jeecra(jexnum(maout//'.PATCH',inc+1), 'LONMAX', ival=2)
    call jeecra(jexnum(maout//'.PATCH',inc+1), 'LONUTI', ival=2)
    call jeveuo(jexnum(maout//'.PATCH',inc+1), 'E', patch)
! ----- Type de maille du patch ------------------------------------------------------------------ 
    zi(patch-1+1) = 14
! ----- DDL interne ------------------------------------------------------------------------------
    zi(patch-1+2) = zi(jmacou-1+3)
! ----- .CONOPA ----------------------------------------------------------------------------------
    zi(jcnnpa+zi(jmacou-1+3)-1) = inc
! --- NOUVEAUX ELEMENTS DE PEAU
    call jeecra(jexnum(conloc,ind), 'LONMAX', ival=3)
    call jeecra(jexnum(conloc,ind), 'LONUTI', ival=3)
    call jeveuo(jexnum(conloc,ind), 'E', jconloc)
    do ino = 1, 3
        zi(jconloc+ino-1)=zi(jmacou+ino-1)
    end do 
    
! --- NOUVEAUX ELEMENTS DE CORPS
    call jeecra(jexnum(conloc,ind+1), 'LONMAX', ival=8)
    call jeecra(jexnum(conloc,ind+1), 'LONUTI', ival=8)
    call jeveuo(jexnum(conloc,ind+1), 'E', jconloc)
    do ino = 1, 8
        zi(jconloc+ino-1)=zi(jmacsu+ino-1)
    end do  
! --- CONNECTIVITE ANCIENS NOUVEAUX ELEMENTS (Peau)
    call jeveuo(jexnum(limane, macou), 'E', jlimane)
    zi(jlimane+1-1)=ind
! --- INFO PATCH LIE
    zi(jlimane+2-1)=inc
! --- CONNECTIVITE ANCIENS NOUVEAUX ELEMENTS (Volume)
    call jeveuo(jexnum(limane, macsu), 'E', jlimane)
    zi(jlimane+1-1)=ind+1
! --- Nettoyage / mis à jour
    ind=ind+2
    ind1=ind1+0
!
    call jedema()
end subroutine
