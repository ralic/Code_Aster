subroutine arlmaf(mail  ,mailar,dime  ,ngrma  ,ima   , &
                  connex,loncum,imail ,nummai,cxcumu)

! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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

! ROUTINE ARLEQUIN

! CREATION DU MAILLAGE INTERNE ARLEQUIN
! RECOPIE MAILLE

! ----------------------------------------------------------------------


! IN  MAIL   : NOM DU MAILLAGE
! IN  MAILAR : NOM DU PSEUDO-MAILLAGE ARLEQUIN
! IN  DIME   : DIMENSION DU PROBLEME
! IN  NGRMA  : NOM DU GROUPE ARLEQUIN DE LA MAILLE
! IN  IMA    : INDEX MAILLE COURANTE DANS NGRMA
! IN  CONNEX : CONNEXITE DES MAILLES
! IN  LONCUM : LONGUEUR CUMULEE DE CONNEX
! OUT NUMMAI : NUMERO ABSOLU DE LA MAILLE DANS LE MAILLAGE
! IN  IMAIL  : NUMERO MAILLE COURANTE DANS PSEUDO-MAILLAGE
! I/O CXCUMU : LONGUEUR CUMULEE DE LA CONNEXITE

   implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/codent.h"
#include "asterfort/arlgrm.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterfort/jecroc.h"
#include "asterfort/utmess.h"
#include "asterfort/jelira.h"
#include "asterfort/assert.h"
#include "asterfort/jeecra.h"
#include "asterfort/jexnum.h"
#include "asterfort/jedema.h"

!     ARGUMENTS:
!     ----------
    character(len=8) :: mail,mailar
    integer :: dime
    integer :: connex(*),loncum(*)
    character(len=19) :: ngrma
    integer :: imail
    integer :: ima,nummai,cxcumu
    integer :: itypma,iret
    integer :: cxno(27),nbno,ino,cxmax
    integer :: jgcnx,jtypm
    character(len=8) :: nomel ,nommai,k8bid
    character(len=24) :: mnomm ,mconn ,mtypm

! ----------------------------------------------------------------------

    call jemarq()

! --- NOMS POUR ACCES AU PSEUDO MAILLAGE

    mnomm = mailar(1:8)//'.NOMMAI         '
    mconn = mailar(1:8)//'.CONNEX         '
    mtypm = mailar(1:8)//'.TYPMAIL        '

! --- GENERATION NOM DE LA MAILLE

    if (imail > 99999) then
        ASSERT(.false.)
    endif
    nomel(1:8) = 'm       '
    call codent(imail,'D0',nomel(2:8))

! --- RECUPERATION INFOS

    call arlgrm(mail,ngrma,dime,ima,connex,loncum,nummai,nommai,&
                itypma,nbno,cxno)

! --- RECOPIE DU TYPE

    call jeveuo(mtypm,'E',jtypm)
    zi(jtypm+nummai-1)  = itypma

! --- CREATION DU NOM DE LA MAILLE

    call jeexin(jexnom(mnomm,nomel),iret)
    if (iret == 0) then
        call jecroc(jexnom(mnomm,nomel))
    else
        call utmess('F','MODELISA7_10',1,nomel)
    endif

! --- RECOPIE DE LA CONNECTIVITE

    cxcumu = cxcumu + nbno
    call jelira(mconn,'LONT',cxmax,k8bid)
    if (cxcumu > cxmax) then
        ASSERT(.false.)
    endif
    call jeecra(jexnum(mconn,imail),'LONMAX',nbno,' ')
    call jeveuo(jexnum(mconn,imail),'E',jgcnx)
    do 51 ino = 1,nbno
        zi(jgcnx+ino-1) = cxno(ino)
    51 end do

    call jedema()

end subroutine
