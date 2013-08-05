subroutine lctel3()
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
! ----------------------------------------------------------------------
!     BUT: CREER APOSTERIORI L'OBJET &CATA.TE.DIM_GEOM
!          QUI CONTIENT LA DIMENSION GEOMETRIQUE DES TYPE_ELEM
!          0 : LE TYPE_ELEM N'UTILISE PAS LA GRANDEUR "GEOM_R"
!          1 : LE TYPE_ELEM UTILISE LA CMP "X"
!          2 : LE TYPE_ELEM UTILISE LA CMP "Y"
!          3 : LE TYPE_ELEM UTILISE LA CMP "Z"
!
! ----------------------------------------------------------------------
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    integer :: dg
    character(len=8) :: k8bid
    character(len=16) :: nomte
    character(len=24) :: nomolo
!
!
!
!-----------------------------------------------------------------------
    integer :: iadige, iamolo, icode, ier, igd, igdgeo, iml
    integer :: inocmp, ite, ix, iy, iz, k, nbcmp
    integer :: nbdg, nbec, nbml, nbpt, nbte
!-----------------------------------------------------------------------
    call jemarq()
    call jelira('&CATA.TE.NOMTE', 'NOMMAX', nbte, k8bid)
    call wkvect('&CATA.TE.DIM_GEOM', 'G V I', nbte, iadige)
    call jenonu(jexnom('&CATA.GD.NOMGD', 'GEOM_R'), igdgeo)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', 'GEOM_R'), 'L', inocmp)
    call jelira(jexnom('&CATA.GD.NOMCMP', 'GEOM_R'), 'LONMAX', nbcmp, k8bid)
    ix=indik8(zk8(inocmp),'X',1,nbcmp)
    iy=indik8(zk8(inocmp),'Y',1,nbcmp)
    iz=indik8(zk8(inocmp),'Z',1,nbcmp)
    call dismoi('F', 'NB_EC', 'GEOM_R', 'GRANDEUR', nbec,&
                k8bid, ier)
    ASSERT(nbec.le.1)
!
!     - BOUCLE SUR TOUS LES MODES LOCAUX DES CATALOGUES :
    call jelira('&CATA.TE.NOMMOLOC', 'NOMMAX', nbml, k8bid)
    do 1, iml=1,nbml
    call jeveuo(jexnum('&CATA.TE.MODELOC', iml), 'L', iamolo)
    icode=zi(iamolo-1+1)
    igd=zi(iamolo-1+2)
    if (igd .ne. igdgeo) goto 1
    if (icode .gt. 3) goto 1
!
    call jenuno(jexnum('&CATA.TE.NOMMOLOC', iml), nomolo)
    nomte=nomolo(1:16)
    call jenonu(jexnom('&CATA.TE.NOMTE', nomte), ite)
!
    nbpt=zi(iamolo-1+4)
    if (nbpt .ge. 10000) then
        nbdg=nbpt-10000
    else
        nbdg=1
    endif
!
    do 2, k=1,nbdg
    dg=zi(iamolo-1+4+k)
    if (exisdg(dg,ix)) zi(iadige-1+ite)=max(1,zi(iadige-1+ite) )
    if (exisdg(dg,iy)) zi(iadige-1+ite)=max(2,zi(iadige-1+ite) )
    if (exisdg(dg,iz)) zi(iadige-1+ite)=max(3,zi(iadige-1+ite) )
 2  continue
 1  continue
!
!
    call jedema()
end subroutine
