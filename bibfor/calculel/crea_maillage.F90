subroutine crea_maillage(noma, noma2, base, nbno, lino)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/cargeo.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/codent.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: noma, noma2
    character(len=1) :: base
    integer :: nbno, lino(*)

! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

! ======================================================================
!  But: creer un "petit" maillage contenant une liste de noeuds

!  noma   : in  : maillage de depart
!  noma2  : out : maillage a creer
!  base   : in  : 'G' ou 'V' : base pour la creation de noma2
!  nbno   : in  : nombre de noeuds de la liste lino
!  lima   : in  : liste des numeros des noeuds
! ======================================================================

    integer ::  nbnoin,ino, jdim,jcorou,iad, ntgeo, nbnoou
    integer ::  ino2, typpoi,jadou,itypou, k
    character(len=4) :: docu
    character(len=8) ::  nomno, nom
    character(len=24) ::  nomnoe, cooval, cooref, coodsc
    character(len=24) ::  dimin, dimou, connex, typmai, nommai
    real(kind=8), pointer :: vale(:) => null()
!------------------------------------------------------------------------------

    call jemarq()

    ASSERT(noma.ne.noma2)
    ASSERT(base.eq.'V' .or. base.eq.'G')


! -1- PRELIMINAIRES
!     ============
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoin)
    nbnoou=nbno


! -2- CREATION DU NOUVEAU MAILLAGE
!     ============================
    nomnoe=noma2//'.NOMNOE'
    cooval=noma2//'.COORDO    .VALE'
    coodsc=noma2//'.COORDO    .DESC'
    cooref=noma2//'.COORDO    .REFE'

! --- OBJET .DIME
    dimin=noma//'.DIME'
    dimou=noma2//'.DIME'
    call jedupo(dimin, base, dimou, .false._1)
    call jeveuo(dimou, 'E', jdim)
    zi(jdim-1+1)=nbnoou
    zi(jdim-1+3)=0

! --- OBJET .NOMNOE
    call jecreo(nomnoe, base//' N K8')
    call jeecra(nomnoe, 'NOMMAX', nbnoou)
    do ino = 1, nbnoou
        call jenuno(jexnum(noma//'.NOMNOE', lino(ino)), nomno)
        call jecroc(jexnom(nomnoe, nomno))
    end do

! --- OBJET .COORDO.VALE
    call wkvect(cooval, base//' V R', nbnoou*3, jcorou)
    call jelira(noma//'.COORDO    .VALE', 'DOCU', cval=docu)
    call jeecra(cooval, 'DOCU', cval=docu)
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
    do ino2 = 1, nbnoou
        ino=lino(ino2)
        zr(jcorou+3*(ino2-1))=vale(1+3*(ino-1))
        zr(jcorou+3*(ino2-1)+1)=vale(1+3*(ino-1)+1)
        zr(jcorou+3*(ino2-1)+2)=vale(1+3*(ino-1)+2)
    end do


! --- OBJET COORDO.DESC
    call jecreo(coodsc, base//' V I')
    call jeecra(coodsc, 'LONMAX', 3)
    call jeecra(coodsc, 'DOCU', cval='CHNO')
    call jeveuo(coodsc, 'E', iad)
    call jenonu(jexnom('&CATA.GD.NOMGD', 'GEOM_R'), ntgeo)
    zi(iad)=ntgeo
    zi(iad+1)=-3
    zi(iad+2)=14


! --- OBJET COORDO.REFE
    call wkvect(cooref, base//' V K24', 4, iad)
    zk24(iad)=noma2


! --- Pour qu'on puisse voire le maillage de noeuds avec salome,
!     on ajoute des POI1 sur tous les noeuds
!----------------------------------------------------
    nommai=noma2//'.NOMMAI'
    connex=noma2//'.CONNEX'
    typmai=noma2//'.TYPMAIL'

! --- OBJET .NOMMAI
    call jecreo(nommai, base//' N K8')
    call jeecra(nommai, 'NOMMAX', nbnoou)
    nom='M'
    do k=1,nbnoou
        call codent(k, 'G', nom(2:8))
        call jecroc(jexnom(nommai, nom))
    enddo

! --- OBJET .TYPMAIL
    call wkvect(typmai, base//' V I', nbnoou, itypou)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1' ), typpoi)
    do k=1,nbnoou
        zi(itypou-1+k)=typpoi
    enddo

! --- OBJET .CONNEX
    call jecrec(connex, base//' V I', 'NU', 'CONTIG', 'VARIABLE',nbnoou)
    call jeecra(connex, 'LONT', nbnoou, ' ')
    do k=1,nbnoou
        call jeecra(jexnum(connex, k), 'LONMAX', 1)
        call jeveuo(jexnum(connex, k), 'E', jadou)
        zi(jadou+1-1)=k
    enddo


    call cargeo(noma2)

    call jedema()

end subroutine
