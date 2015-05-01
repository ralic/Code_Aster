subroutine mavegr(nomu)
    implicit none
#include "jeveux.h"
#include "asterfort/cpclma.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: nomu
! ----------------------------------------------------------------------
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
!
!     SUPPRESSION DES GROUPES DE NOEUDS OU MAILLES DE NOM '      '
! ----------------------------------------------------------------------
!
    integer :: iret, i, j, nbgrma, nbgrmt, nbgrno, nbgrnt, nbma, nbno, jvg, jgg
    character(len=24) :: grpnoe, grpnov, grpmai, grpmav, gpptnn, gpptnm
    character(len=24) :: nomg, blanc
! ----------------------------------------------------------------------
    call jemarq()
!
    grpnoe = nomu//'.GROUPENO'
    grpnov = '&&MAVEGR.GROUPENO'
    gpptnn = nomu//'.PTRNOMNOE'
    grpmai = nomu//'.GROUPEMA'
    grpmav = '&&MAVEGR.GROUPEMA'
    gpptnm = nomu//'.PTRNOMMAI'
    blanc = ' '
!
! --- TRAITEMENT DES GROUP_MA
!
    call jeexin(grpmai, iret)
    if (iret .gt. 0) then
        call jelira(grpmai, 'NMAXOC', nbgrma)
        nbgrmt = nbgrma
        do 100 i = 1, nbgrma
            call jeexin(jexnum ( grpmai, i ), iret)
            if (iret .eq. 0) goto 100
            call jenuno(jexnum ( grpmai, i ), nomg)
            if (nomg .eq. blanc) then
                nbgrmt = nbgrmt - 1
                call utmess('A', 'MODELISA5_36')
            endif
100      continue
        if (nbgrmt .eq. 0) then
            call jedetr(grpmai)
        else if (nbgrmt .ne. nbgrma) then
            call cpclma(nomu, '&&MAVEGR', 'GROUPEMA', 'V')
            call jedetr(grpmai)
            call jedetr(gpptnm)
            call jecreo(gpptnm, 'G N K24')
            call jeecra(gpptnm, 'NOMMAX', nbgrmt)
            call jecrec(grpmai, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                        nbgrmt)
            do 110 i = 1, nbgrma
                call jeexin(jexnum ( grpmav, i ), iret)
                if (iret .eq. 0) goto 110
                call jenuno(jexnum ( grpmav, i ), nomg)
                if (nomg .eq. blanc) goto 110
                call jecroc(jexnom ( grpmai, nomg ))
                call jeveuo(jexnum(grpmav, i), 'L', jvg)
                call jelira(jexnum(grpmav, i), 'LONUTI', nbma)
                call jeecra(jexnom(grpmai, nomg), 'LONMAX', max(1, nbma))
                call jeecra(jexnom(grpmai, nomg), 'LONUTI', nbma)
                call jeveuo(jexnom(grpmai, nomg), 'E', jgg)
                do 112 j = 0, nbma-1
                    zi(jgg+j) = zi(jvg+j)
112              continue
110          continue
            call jedetr(grpmav)
        endif
    endif
!
! --- TRAITEMENT DES GROUP_NO
!
    call jeexin(grpnoe, iret)
    if (iret .gt. 0) then
        call jelira(grpnoe, 'NMAXOC', nbgrno)
        nbgrnt = nbgrno
        do 200 i = 1, nbgrno
            call jeexin(jexnum ( grpnoe, i ), iret)
            if (iret .eq. 0) goto 200
            call jenuno(jexnum ( grpnoe, i ), nomg)
            if (nomg .eq. blanc) then
                nbgrnt = nbgrnt - 1
                call utmess('A', 'MODELISA5_37')
            endif
200      continue
        if (nbgrnt .eq. 0) then
            call jedetr(grpnoe)
        else if (nbgrnt .ne. nbgrno) then
            call cpclma(nomu, '&&MAVEGR', 'GROUPENO', 'V')
            call jedetr(grpnoe)
            call jedetr(gpptnn)
            call jecreo(gpptnn, 'G N K24')
            call jeecra(gpptnn, 'NOMMAX', nbgrnt)
            call jecrec(grpnoe, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE',&
                        nbgrnt)
            do 210 i = 1, nbgrno
                call jeexin(jexnum ( grpnov, i ), iret)
                if (iret .eq. 0) goto 210
                call jenuno(jexnum ( grpnov, i ), nomg)
                if (nomg .eq. blanc) goto 210
                call jecroc(jexnom ( grpnoe, nomg ))
                call jeveuo(jexnum(grpnov, i), 'L', jvg)
                call jelira(jexnum(grpnov, i), 'LONUTI', nbno)
                call jeecra(jexnom(grpnoe, nomg), 'LONMAX', max(1, nbno))
                call jeecra(jexnom(grpnoe, nomg), 'LONUTI', nbno)
                call jeveuo(jexnom(grpnoe, nomg), 'E', jgg)
                do 212 j = 0, nbno-1
                    zi(jgg+j) = zi(jvg+j)
212              continue
210          continue
            call jedetr(grpnov)
        endif
    endif
!
    call jedema()
!
end subroutine
