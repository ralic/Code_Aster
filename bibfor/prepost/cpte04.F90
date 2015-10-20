subroutine cpte04(main  , maout , inc   , jcoor , jcnnpa, conloc,&
                  limane, nomnoe, nbno  , jmacou, jmacsu, macou ,&
                  macsu , ind   , ind1)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
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
#include "asterfort/cnmpmc.h"
#include "asterfort/cpnno.h"
#include "asterfort/jelira.h"
#include "asterfort/utlisi.h"
#include "asterfort/cpclma.h"
#include "asterfort/jenonu.h"
!
    character(len=8), intent(in) :: main
    character(len=8), intent(in) :: maout
    integer, intent(in) :: inc 
    integer, intent(in) :: jcoor
    integer, intent(in) :: jcnnpa
    character(len=24), intent(in) :: conloc
    character(len=24), intent(in) :: limane
    character(len=24), intent(in) :: nomnoe
    integer, intent(in) :: nbno 
    integer, intent(in) :: jmacou
    integer, intent(in) :: jmacsu
    integer, intent(in) :: macou
    integer, intent(in) :: macsu
    integer, intent(out) :: ind 
    integer, intent(out) :: ind1      
! -------------------------------------------------------------------------------------------------
!        CREATION DES NOUVEAUS NOUEDS ET NOUVELLE MAILLE CAS TETRA 10
! -------------------------------------------------------------------------------------------------
! -------------------------------------------------------------------------------------------------
    integer :: patch, linop(3), linoc(1), ntrou
    integer :: jlimane, jconloc
! -------------------------------------------------------------------------------------------------
    call jemarq()
!
    call jecroc(jexnum(maout//'.PATCH',inc+1))
    call jeecra(jexnum(maout//'.PATCH',inc+1), 'LONMAX', ival=2)
    call jeecra(jexnum(maout//'.PATCH',inc+1), 'LONUTI', ival=2)
    call jeveuo(jexnum(maout//'.PATCH',inc+1), 'E', patch)
! --- TYPE DE MAILLE PATCH
    zi(patch-1+1) = 18
! --- DDL INTERNE
    zi(patch-1+2)=nbno+ind1
    zi(jcnnpa+nbno+ind1-1) = inc 
! --- CREATION DU NOEUD DDL INTERNE      
    call cpnno(main,macou,zr(jcoor),ind1,nbno,nomnoe)
! --- NOUVEAUX ELEMENTS DE PEAU
    call jeecra(jexnum(conloc,ind), 'LONMAX', ival=3)
    call jeecra(jexnum(conloc,ind), 'LONUTI', ival=3)
    call jeveuo(jexnum(conloc,ind), 'E', jconloc)
    zi(jconloc+1-1)=zi(jmacou+1-1)
    zi(jconloc+2-1)=zi(jmacou+2-1)
    zi(jconloc+3-1)=nbno+ind1
    call jeecra(jexnum(conloc,ind+1), 'LONMAX', ival=3)
    call jeecra(jexnum(conloc,ind+1), 'LONUTI', ival=3)
    call jeveuo(jexnum(conloc,ind+1), 'E', jconloc)
    zi(jconloc+1-1)=zi(jmacou+2-1)
    zi(jconloc+2-1)=zi(jmacou+3-1)
    zi(jconloc+3-1)=nbno+ind1
    call jeecra(jexnum(conloc,ind+2), 'LONMAX', ival=3)
    call jeecra(jexnum(conloc,ind+2), 'LONUTI', ival=3)
    call jeveuo(jexnum(conloc,ind+2), 'E', jconloc)
    zi(jconloc+1-1)=zi(jmacou+3-1)
    zi(jconloc+2-1)=zi(jmacou+1-1)
    zi(jconloc+3-1)=nbno+ind1
! --- NOUVEAUX ELEMENTS DE CORPS
    call utlisi('INTER', zi(jmacou),3, zi(jmacsu), 4,&
                linop,3, ntrou)
    call utlisi('DIFFE', zi(jmacsu), 4, zi(jmacou),3,&
                linoc,1, ntrou)
!
    call jeecra(jexnum(conloc,ind+3), 'LONMAX', ival=4)
    call jeecra(jexnum(conloc,ind+3), 'LONUTI', ival=4)
    call jeveuo(jexnum(conloc,ind+3), 'E', jconloc)
    zi(jconloc+2-1)=linop(1)
    zi(jconloc+3-1)=linop(2)
    zi(jconloc+4-1)=nbno+ind1
    zi(jconloc+1-1)=linoc(1)
    call jeecra(jexnum(conloc,ind+4), 'LONMAX', ival=4)
    call jeecra(jexnum(conloc,ind+4), 'LONUTI', ival=4)
    call jeveuo(jexnum(conloc,ind+4), 'E', jconloc)
    zi(jconloc+2-1)=linop(2)
    zi(jconloc+3-1)=linop(3)
    zi(jconloc+4-1)=nbno+ind1
    zi(jconloc+1-1)=linoc(1)
    call jeecra(jexnum(conloc,ind+5), 'LONMAX', ival=4)
    call jeecra(jexnum(conloc,ind+5), 'LONUTI', ival=4)
    call jeveuo(jexnum(conloc,ind+5), 'E', jconloc)
    zi(jconloc+2-1)=linop(3)
    zi(jconloc+3-1)=linop(1)
    zi(jconloc+4-1)=nbno+ind1
    zi(jconloc+1-1)=linoc(1)
! --- CONNECTIVITE ANCIENS NOUVEAUX ELEMENTS

    call jeveuo(jexnum(limane, macou), 'E', jlimane)
    zi(jlimane+1-1)=ind
    zi(jlimane+2-1)=ind+1
    zi(jlimane+3-1)=ind+2
! ----- INFO PATCH LIE
    zi(jlimane+4-1)=inc
!

    call jeveuo(jexnum(limane, macsu), 'E', jlimane)
    zi(jlimane+1-1)=ind+3
    zi(jlimane+2-1)=ind+4
    zi(jlimane+3-1)=ind+5
    
    ind=ind+6
    ind1=ind1+1
!
    call jedema()
end subroutine
