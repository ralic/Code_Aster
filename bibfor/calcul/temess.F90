subroutine temess(typ)
use module_calcul, only : ca_option_, ca_nomte_, ca_icaelk_, ca_ialiel_,&
    ca_illiel_, ca_igr_, ca_iel_, ca_nomtm_, ca_iamaco_, ca_ilmaco_

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
! person_in_charge: jacques.pellet at edf.fr
!
! But : Cette routine complete le message d'erreur emis par utmess
!       pendant un calcul elementaire :
!         * nom de l'option calculee
!         * nom de la maille
!         * coordonnees des noeuds
!         * nom du maillage
!         * noms de GROUP_MA contenant la maille
!         ...
!-----------------------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/utmess_core.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenuno.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
!
    character(len=1), intent(in) :: typ
!
    integer :: ima,iexi,k,n1,k1,jgrma,nbgrma,nbgrmt
    integer :: nno,ino,nuno,jcoor
    character(len=24) :: valkc(9), ligrma(4),nomgrm,grpmav
    character(len=8) :: ma,nomail
    character(len=256) :: ufname  = ' '
    real(kind=8) :: valrc(3)
!-----------------------------------------------------------------------------------
    call jemarq()

    ma = zk24(ca_icaelk_-1+1)(1:8)

    nbgrma = 0
    nno = 0
    nomail='XXX'
    ima = zi(ca_ialiel_-1+zi(ca_illiel_+ca_igr_-1)+ca_iel_-1)
    if (ima .gt. 0) then
        call jenuno(jexnum(ma//'.NOMMAI', ima), nomail)
        nno = zi(ca_ilmaco_-1+ima+1) - zi(ca_ilmaco_-1+ima)
    endif

!   -- recherche des 4 premiers GROUP_MA qui contiennent ima :
    if (ima.gt.0) then
        grpmav = ma//'.GROUPEMA'
        call jeexin(grpmav, iexi)
        if (iexi.gt.0) then
             call jelira(grpmav, 'NMAXOC', nbgrmt)
             do k=1,nbgrmt
                 call jeexin(jexnum(grpmav, k), iexi)
                 if (iexi.eq.0) cycle
                 call jenuno(jexnum(grpmav, k), nomgrm)
                 call jelira(jexnum(grpmav, k), 'LONUTI', n1)
                 call jeveuo(jexnum(grpmav, k), 'L', jgrma)
                 do k1=1,n1
                     if (zi(jgrma-1+k1).eq.ima) then
                         nbgrma=nbgrma+1
                         ligrma(nbgrma)=nomgrm
                         if (nbgrma.eq.4) goto 100
                     endif
                 enddo
             enddo
        endif
100     continue

!       -- calcul du centre de gravite de la maille :
        valrc(:)=0.d0
        call jeveuo(ma//'.COORDO    .VALE','L',jcoor)
        do ino = 1,nno
            nuno = zi(ca_iamaco_-1+zi(ca_ilmaco_+ima-1)+ino-1)
            valrc(1)=valrc(1)+zr(jcoor-1+3*(nuno-1)+1)/nno
            valrc(2)=valrc(2)+zr(jcoor-1+3*(nuno-1)+2)/nno
            valrc(3)=valrc(3)+zr(jcoor-1+3*(nuno-1)+3)/nno
        enddo
    endif


    valkc(:)=' '
    valkc(1)=ca_option_
    valkc(2)=ca_nomte_
    valkc(3)=ma
    valkc(4)=nomail
    valkc(5)=ca_nomtm_
    do k=1,nbgrma
        valkc(5+k)=ligrma(k)
    enddo
    if (nbgrma.eq.4)  valkc(5+4)='...'

    call utmess_core(typ, 'CALCUL1_6', 9, valkc, 0,&
                         [0], 3, valrc, ufname)

    call jedema()
end subroutine temess
