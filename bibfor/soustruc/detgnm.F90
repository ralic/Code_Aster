subroutine detgnm(ma)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/cpclma.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: ma
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
!     ------------------------------------------------------------------
!     BUT: - TRAITE LES MOTS CLES FACTEURS DETR_GROUP_MA ET
!            DETR_GROUP_NO DE L'OPERATEUR DEFI_GROUP.
!          - PERMET DE DETRUIRE DES GROUPES (NOEUDS OU MAILLES)
!
!     IN : MA : NOM DU MAILLAGE
!     ------------------------------------------------------------------
!
!
    integer :: n1, iocc, maxval, nbval,  iret, nbtgp, numgm, nbgmde
    integer :: nbgp, nbmagp, jgm, i, j, j1, j2, ngp, ig
    parameter(ngp=2)
    character(len=8) :: k8b
    character(len=16) :: detr(2), group(2), ptrn(2)
    character(len=24) :: grp, gpptnm, nomgp
    character(len=24), pointer :: group_detr(:) => null()
    data detr  / 'DETR_GROUP_MA','DETR_GROUP_NO'/
    data group / '.GROUPEMA','.GROUPENO'/
    data ptrn  / '.PTRNOMMAI','.PTRNOMNOE'/
!
    call jemarq()
!
    do 100 ig = 1, ngp
        call getfac(detr(ig), n1)
        grp='&&DETGNM'//group(ig)
        gpptnm = '&&DETGNM'//ptrn(ig)
        if (n1 .ne. 0) then
            call jeexin(ma//group(ig), iret)
            if (iret .eq. 0) goto 100
            call jelira(ma//group(ig), 'NUTIOC', nbtgp)
            call wkvect('&&DETGNM.GROUP', 'V V I', nbtgp, jgm)
            do 5 i = 1, nbtgp
                zi(jgm+i-1)=0
 5          continue
            do 10 iocc = 1, n1
                maxval=0
                call getvtx(detr(ig), 'NOM', iocc=iocc, nbval=maxval, vect=k8b,&
                            nbret=nbval)
                nbval=-nbval
                AS_ALLOCATE(vk24=group_detr, size=nbval)
                call getvtx(detr(ig), 'NOM', iocc=iocc, nbval=nbval, vect=group_detr,&
                            nbret=iret)
!              ON RECUPERE LES NUMEROS DES GROUPES A DETRUIRE
                do 15 i = 1, nbval
                    call jenonu(jexnom(ma//group(ig), group_detr(i)), numgm)
                    if (numgm .ne. 0) then
                        zi(jgm+numgm-1)=numgm
                    endif
15              continue
                AS_DEALLOCATE(vk24=group_detr)
10          continue
!           ON COMPTE LE NOMBRE DE GROUPES A DETRUIRE
            nbgmde=0
            do 20 i = 1, nbtgp
                if (zi(jgm+i-1) .ne. 0) then
                    nbgmde=nbgmde+1
                endif
20          continue
!           REACTUALISATION DE L'OBJET .GROUPEMA (OU .GROUPENO)
            nbgp=nbtgp-nbgmde
            if (nbgp .eq. 0) then
                call jedetr(ma//group(ig))
                goto 100
            endif
            call jecreo(gpptnm, 'V N K24')
            call jeecra(gpptnm, 'NOMMAX', nbgp)
            call jecrec(grp, 'V V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                        nbgp)
            do 25 i = 1, nbtgp
                if (zi(jgm+i-1) .eq. 0) then
                    call jenuno(jexnum(ma//group(ig), i), nomgp)
                    call jecroc(jexnom(grp, nomgp))
                    call jelira(jexnom(ma//group(ig), nomgp), 'LONMAX', nbmagp)
                    call jeecra(jexnom(grp, nomgp), 'LONMAX', max(1, nbmagp))
                    call jeecra(jexnom(grp, nomgp), 'LONUTI', nbmagp)
                    call jeveuo(jexnom(grp, nomgp), 'E', j2)
                    call jeveuo(jexnom(ma//group(ig), nomgp), 'L', j1)
                    do 30 j = 1, nbmagp
                        zi(j2+j-1)=zi(j1+j-1)
30                  continue
                endif
25          continue
            call jedetr(ma//group(ig))
            call jedetr(ma//ptrn(ig))
            call cpclma('&&DETGNM', ma, group(ig)(2:9), 'G')
        endif
        call jedetr('&&DETGNM.GROUP')
        call jedetr(grp)
        call jedetr(gpptnm)
100  end do
!
    call jedema()
!
end subroutine
