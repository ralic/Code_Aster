subroutine rc32th()
    implicit   none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     LECTURE DU MOT CLE FACTEUR "RESU_THER"
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/rc32my.h"
#include "asterfort/rctres.h"
#include "asterfort/rcver1.h"
#include "asterfort/rcveri.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbexv1.h"
#include "asterfort/tbliva.h"
#include "asterfort/trace.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
    integer :: ibid, n1, iocc, nbther, nume, nbinst, jinst, i, j, k, l, ndim
    integer :: nbabsc, jabsc, jorig, jextr, ncmp, jcont, iret, kk
    parameter  ( ncmp = 6 )
    real(kind=8) :: prec(2), momen0, momen1, vale(2)
    real(kind=8) :: sigth(1000*ncmp), tresc(2), r3(2)
    real(kind=8) :: tremin(2), tremax(2), tmin(2), tmax(2)
    complex(kind=8) :: cbid
    logical :: exist
    character(len=8) :: k8b, crit(2), nocmp(ncmp), table, knume, tbpres
    character(len=16) :: motclf, valek(2), motcl2
    character(len=24) :: instan, abscur, jvorig, jvextr
    character(len=24) :: valk(7)
    integer :: iarg
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'RESU_THER'
    call getfac(motclf, nbther)
    if (nbther .eq. 0) goto 9999
!
    nocmp(1) = 'SIXX'
    nocmp(2) = 'SIYY'
    nocmp(3) = 'SIZZ'
    nocmp(4) = 'SIXY'
    nocmp(5) = 'SIXZ'
    nocmp(6) = 'SIYZ'
!
    valek(1) = 'INST            '
    valek(2) = 'ABSC_CURV       '
!
    prec(1) = 1.0d-06
    prec(2) = 1.0d-06
    crit(1) = 'RELATIF'
    crit(2) = 'RELATIF'
!
    jvorig = '&&RC3200.THER_UNIT .ORIG'
    jvextr = '&&RC3200.THER_UNIT .EXTR'
    call jecrec(jvorig, 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nbther)
    call jecrec(jvextr, 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nbther)
!
    motcl2 = 'RESU_MECA_UNIT'
    call getvid(motcl2, 'TABL_PRES', 1, iarg, 1,&
                tbpres, n1)
    ASSERT(n1.ne.0)
!
    do 10, iocc = 1, nbther, 1
!
    call getvis(motclf, 'NUME_RESU_THER', iocc, iarg, 1,&
                nume, n1)
    knume = 'T       '
    call codent(nume, 'D0', knume(2:8))
!
    call getvid(motclf, 'TABL_RESU_THER', iocc, iarg, 1,&
                table, n1)
!
! ------ ON VERIFIE L'ORDRE DES NOEUDS
    call rcveri(table)
!
! ------ ON VERIFIE LA COHERENCE AVEC LA TABLE TBPRES
    call rcver1('THERMIQUE', tbpres, table)
!
! ------ ON RECUPERE LES INSTANTS DANS LA TABLE
!
    call tbexip(table, valek(1), exist, k8b)
    if (.not. exist) then
        valk (1) = table
        valk (2) = valek(1)
        call u2mesg('F', 'POSTRCCM_1', 2, valk, 0,&
                    0, 0, 0.d0)
    endif
    instan = '&&RC32TH.INSTANT'
    call tbexv1(table, valek(1), instan, 'V', nbinst,&
                k8b)
    call jeveuo(instan, 'L', jinst)
!
! ------ ON RECUPERE L'ABSC_CURV DANS LA TABLE
!
    call tbexip(table, valek(2), exist, k8b)
    if (.not. exist) then
        valk (1) = table
        valk (2) = valek(2)
        call u2mesg('F', 'POSTRCCM_1', 2, valk, 0,&
                    0, 0, 0.d0)
    endif
    abscur = '&&RC32TH.ABSC_CURV'
    call tbexv1(table, valek(2), abscur, 'V', nbabsc,&
                k8b)
    call jeveuo(abscur, 'L', jabsc)
!
    call wkvect('&&RC32TH.CONTRAINTES', 'V V R', nbabsc, jcont)
!
    ndim = 4 * ncmp * 2
    call jecroc(jexnom(jvorig, knume))
    call jeecra(jexnom(jvorig, knume), 'LONMAX', ndim)
    call jeecra(jexnom(jvorig, knume), 'LONUTI', ndim)
    call jeveuo(jexnom(jvorig, knume), 'E', jorig)
!
    call jecroc(jexnom(jvextr, knume))
    call jeecra(jexnom(jvextr, knume), 'LONMAX', ndim)
    call jeecra(jexnom(jvextr, knume), 'LONUTI', ndim)
    call jeveuo(jexnom(jvextr, knume), 'E', jextr)
!
    do 116 k = 1, nbabsc*ncmp
        sigth(k) = 0.d0
116  continue
    do 1116 k = 1, 2
        tresc(k) = 0.d0
        r3(k) = 0.d0
        tmin(k) = 0.d0
        tremin(k) = 0.d0
        tmax(k) = 0.d0
        tremax(k) = 0.d0
1116  continue
!
    do 12 i = 1, nbinst
        vale(1) = zr(jinst+i-1)
!
        do 14 j = 1, ncmp
!
            kk = 0
            do 16 k = 1, nbabsc, nbabsc-1
                kk = kk + 1
                vale(2) = zr(jabsc+k-1)
!
                call tbliva(table, 2, valek, ibid, vale,&
                            cbid, k8b, crit, prec, nocmp(j),&
                            k8b, ibid, zr( jcont+k-1), cbid, k8b,&
                            iret)
                if (iret .ne. 0) then
                    valk (1) = table
                    valk (2) = nocmp(j)
                    valk (3) = valek(1)
                    valk (4) = valek(2)
                    call u2mesg('F', 'POSTRCCM_2', 4, valk, 0,&
                                0, 2, vale)
                endif
                sigth((k-1)*ncmp+j) = zr(jcont+k-1)
                if (j .eq. ncmp) then
                    call rctres(sigth((k-1)*ncmp+1), tresc(kk))
!
                    if (trace(3,sigth((k-1)*ncmp+1)) .lt. 0.d0) then
                        r3(kk) = tresc(kk)*(-1.0d0)
                    else
                        r3(kk) = tresc(kk)
                    endif
                    if (i .eq. 1) then
                        tremin(kk) = r3(kk)
                        tremax(kk) = r3(kk)
                        tmin(kk) = vale(1)
                        tmax(kk) = vale(1)
                    else
                        if (r3(kk) .lt. tremin(kk)) then
                            tremin(kk) = r3(kk)
                            tmin(kk) = vale(1)
                        endif
                        if (r3(kk) .gt. tremax(kk)) then
                            tremax(kk) = r3(kk)
                            tmax(kk) = vale(1)
                        endif
                    endif
                endif
!
16          continue
!
14      continue
!
12  continue
!
!   POUR LA VALEUR MINIMALE
    do 66 j = 1, ncmp
        vale(1) = tmin(1)
        do 166 k = 1, nbabsc
            vale(2) = zr(jabsc+k-1)
!
            call tbliva(table, 2, valek, ibid, vale,&
                        cbid, k8b, crit, prec, nocmp(j),&
                        k8b, ibid, zr(jcont+k-1), cbid, k8b,&
                        iret)
166      continue
        zr(jorig-1+j) = zr(jcont)
        call rc32my(nbabsc, zr(jabsc), zr(jcont), momen0, momen1)
!
        l = ncmp*2 + j
        zr(jorig-1+l) = momen0 - 0.5d0*momen1
!
        l = ncmp*4 + j
        zr(jorig-1+l) = momen0
!
        vale(1) = tmin(2)
        do 266 k = 1, nbabsc
            vale(2) = zr(jabsc+k-1)
!
            call tbliva(table, 2, valek, ibid, vale,&
                        cbid, k8b, crit, prec, nocmp(j),&
                        k8b, ibid, zr(jcont+k-1), cbid, k8b,&
                        iret)
266      continue
        zr(jextr-1+j) = zr(jcont+nbabsc-1)
!
        call rc32my(nbabsc, zr(jabsc), zr(jcont), momen0, momen1)
!
        l = ncmp*2 + j
        zr(jextr-1+l) = momen0 + 0.5d0*momen1
!
        l = ncmp*4 + j
        zr(jextr-1+l) = momen0
!
        l = ncmp*6 + j
        zr(jextr-1+l) = 0.5d0*momen1
!
!   POUR LA VALEUR MAXIMALE
        vale(1) = tmax(1)
        do 366 k = 1, nbabsc
            vale(2) = zr(jabsc+k-1)
!
            call tbliva(table, 2, valek, ibid, vale,&
                        cbid, k8b, crit, prec, nocmp(j),&
                        k8b, ibid, zr(jcont+k-1), cbid, k8b,&
                        iret)
366      continue
        l = ncmp + j
        zr(jorig-1+l) = zr(jcont)
!
        call rc32my(nbabsc, zr(jabsc), zr(jcont), momen0, momen1)
!
        l = ncmp*3 + j
        zr(jorig-1+l) = momen0 - 0.5d0*momen1
!
        l = ncmp*5 + j
        zr(jorig-1+l) = momen0
!
        l = ncmp*7 + j
        zr(jorig-1+l) = 0.5d0*momen1
!
        vale(1) = tmax(2)
        do 466 k = 1, nbabsc
            vale(2) = zr(jabsc+k-1)
!
            call tbliva(table, 2, valek, ibid, vale,&
                        cbid, k8b, crit, prec, nocmp(j),&
                        k8b, ibid, zr(jcont+k-1), cbid, k8b,&
                        iret)
466      continue
        l = ncmp + j
        zr(jextr-1+l) = zr(jcont+nbabsc-1)
!
        call rc32my(nbabsc, zr(jabsc), zr(jcont), momen0, momen1)
!
        l = ncmp*3 + j
        zr(jextr-1+l) = momen0 + 0.5d0*momen1
!
        l = ncmp*5 + j
        zr(jextr-1+l) = momen0
!
        l = ncmp*7 + j
        zr(jextr-1+l) = 0.5d0*momen1
66  continue
    call jedetr(instan)
    call jedetr(abscur)
    call jedetr('&&RC32TH.CONTRAINTES')
!
    10 end do
!
9999  continue
    call jedema()
end subroutine
