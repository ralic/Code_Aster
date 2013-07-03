subroutine porefd(trange, noeu, cmp, nomrez)
    implicit   none
#include "jeveux.h"
#include "asterfort/foc1ma.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: trange, noeu, cmp, nomrez
! ----------------------------------------------------------------------
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
!
!     POST-TRAITEMENT DE "RELA_EFFO_DEPL"
!
! ----------------------------------------------------------------------
    integer :: jdesc, jredn, jredc, jredd, jinst, nbinst, nbred, inume, jdepl
    integer :: jnlin, jvar, jfon, i, nbmax, ii, ic, imax, nbpara
    parameter    ( nbpara = 8 )
    real(kind=8) :: para(nbpara), xmax, temd, temf, temm
    complex(kind=8) :: c16b
    character(len=8) :: k8b, nomres, typara(nbpara), valek(3)
    character(len=16) :: nopara(nbpara)
    character(len=19) :: nomk19
    character(len=24) :: nomk24
!
    data nopara / 'RELATION' , 'NOEUD'      , 'CMP',&
     &              'PHASE'    , 'INST_INIT'  , 'INST_FIN',&
     &              'MAXI'     , 'INST_MAXI'  /
    data typara / 'K8' , 'K8' , 'K8' , 'I' , 'R' , 'R' , 'R' , 'R' /
!     ------------------------------------------------------------------
!
    call jemarq()
    nomk19 = ' '
    nomk19(1:8) = trange
    nomk24 = ' '
    nomk24(1:8) = noeu
    nomk24(9:16) = cmp
    nomres = nomrez
!
    call tbcrsd(nomres, 'G')
    call tbajpa(nomres, nbpara, nopara, typara)
!
    call jeveuo(nomk19//'.DESC', 'L', jdesc)
    call jeveuo(nomk19//'.REDN', 'L', jredn)
    call jeveuo(nomk19//'.REDC', 'L', jredc)
    call jeveuo(nomk19//'.REDD', 'L', jredd)
    call jeveuo(nomk19//'.DISC', 'L', jinst)
    call jelira(nomk19//'.DISC', 'LONUTI', nbinst, k8b)
    nbred = zi(jdesc+3)
!
    do 10 inume = 0, nbred-1
        if (zk24(jredn+inume)(1:16) .eq. nomk24) goto 12
10  end do
    call u2mess('F', 'PREPOST4_57')
!
12  continue
    valek(1) = zk24(jredn+inume)(17:24)
    valek(2) = noeu
    valek(3) = cmp
!
!     --- RECHERCHE DU MAXIMUM DE LA FONCTION ---
    call wkvect('&&POREFD.DEPL', 'V V R', nbinst, jdepl)
    call wkvect('&&POREFD.NLIN', 'V V I', nbinst, jnlin)
    call wkvect('&&POREFD.INSTMAX', 'V V R', nbinst, jvar)
    call wkvect('&&POREFD.DEPLMAX', 'V V R', nbinst, jfon)
    do 14 i = 0, nbinst-1
        zr(jdepl+i) = zr(jredd+inume+nbred*i)
        zi(jnlin+i) = zi(jredc+inume+nbred*i)
14  end do
    call foc1ma(nbinst, zr(jinst), zr(jdepl), nbmax, zr(jvar),&
                zr(jfon))
!
!     --- RECHERCHE DES PHASES NON-LINEAIRE ---
    do 18 i = 0, nbinst-1
        if (zi(jnlin+i) .eq. 1) goto 20
18  end do
    goto 500
!
20  continue
!
    ii = 0
    ic = 0
    do 30 i = 0, nbinst-1
        if (zi(jnlin+i) .eq. 1 .and. ic .eq. 0) then
            xmax = zr(jdepl+i)
            imax = i
            ic = 1
            ii = ii + 1
            temd = zr(jinst+i)
        else if (zi(jnlin+i) .eq. 1) then
            if (abs(zr(jdepl+i)) .gt. abs(xmax)) then
                xmax = zr(jdepl+i)
                imax = i
            endif
        else if (zi(jnlin+i).eq.0 .and. ic.eq.1) then
            ic = 0
            temf = zr(jinst+i-1)
            temm = zr(jinst+imax)
            para(1) = temd
            para(2) = temf
            para(3) = xmax
            para(4) = temm
            call tbajli(nomres, nbpara, nopara, ii, para,&
                        c16b, valek, 0)
        endif
30  end do
    if (ic .eq. 1) then
        temf = zr(jinst+nbinst-1)
        temm = zr(jinst+imax)
        para(1) = temd
        para(2) = temf
        para(3) = xmax
        para(4) = temm
        call tbajli(nomres, nbpara, nopara, ii, para,&
                    c16b, valek, 0)
    endif
!
500  continue
    call jedetr('&&POREFD.DEPL')
    call jedetr('&&POREFD.NLIN')
    call jedetr('&&POREFD.INSTMAX')
    call jedetr('&&POREFD.DEPLMAX')
!
    call jedema()
end subroutine
