subroutine cptr03(main  , maout , inc   , jcoor , jcnnpa, conloc,&
                  limane, nomnoe, nbno  , jmacou, jmacsu, macou ,&
                  macsu , ind   , ind1  )
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
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/jelira.h"
#include "asterfort/cpclma.h"
#include "asterfort/jenonu.h"
#include "asterfort/cpnptr3.h"
#include "asterfort/cpmptr3.h"
#include "asterfort/cpmctr3.h"
#include "asterfort/cnpc.h"
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
    integer :: patch
    integer :: jlimane
    integer :: jconneo    
    character(len=24) :: conneo
! -------------------------------------------------------------------------------------------------
    call jemarq()
!
    call jecroc(jexnum(maout//'.PATCH',inc+1))
    call jeecra(jexnum(maout//'.PATCH',inc+1), 'LONMAX', ival=2)
    call jeecra(jexnum(maout//'.PATCH',inc+1), 'LONUTI', ival=2)
    call jeveuo(jexnum(maout//'.PATCH',inc+1), 'E', patch)
! --- TYPE DE MAILLE PATCH
    zi(patch-1+1) = 7
! --- DDL INTERNE
     zi(patch-1+2)=nbno+ind1
    zi(jcnnpa+nbno+ind1-1) = inc
! --- CREATION DES NOEUDS DDL INTERNE      
    call cpnptr3(main,macou,zr(jcoor),nbno+ind1,nomnoe)
! --- NOUVEAUX ELEMENTS DE PEAU
    call cpmptr3(conloc, jmacou, nbno+ind1, ind)
! --- NOUVEAUX ELEMENTS DE CORPS
    conneo='&&CPTR3.CNORD'
    call cnpc(main, macou, macsu, conneo)
    call jeveuo(conneo,'L',jconneo)
    call cpmctr3(conloc, jmacsu, nbno+ind1, ind+2, zi(jconneo))        
! --- CONNECTIVITE ANCIENS NOUVEAUX ELEMENTS (Peau)

    call jeveuo(jexnum(limane, macou), 'E', jlimane)
    zi(jlimane+1-1)=ind
    zi(jlimane+2-1)=ind+1
! --- INFO PATCH LIE
    zi(jlimane+3-1)=inc
! --- CONNECTIVITE ANCIENS NOUVEAUX ELEMENTS (Volume)

    call jeveuo(jexnum(limane, macsu), 'E', jlimane)
    zi(jlimane+1-1)=ind+2
    zi(jlimane+2-1)=ind+3
! --- Nettoyage / mis à jour
    ind=ind+4
    ind1=ind1+1
    call jedetr(conneo)  
!
    call jedema()
end subroutine
