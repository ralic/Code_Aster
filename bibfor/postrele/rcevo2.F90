subroutine rcevo2(nbinti, kinti, csigm, cinst, csiex,&
                  kemixt, cstex, csmex, lfatig, flexio,&
                  lrocht, cnoc, cresu, cpres)
    implicit      none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8vide.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rc32my.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbextb.h"
#include "asterfort/tbexv1.h"
#include "asterfort/tbliva.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    integer :: nbinti
    logical :: lfatig, flexio, lrocht, kemixt
    character(len=16) :: kinti
    character(len=24) :: csigm, cinst, csiex, cnoc, cresu, cpres, cstex, csmex
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
!     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
!     LECTURE DU MOT CLE FACTEUR "TRANSITOIRE"
!
!     ------------------------------------------------------------------
!
    integer :: ibid, n1, nbinst, kinst, jcont, jcofl, ncmpr, i, j, k, l, ndim
    integer :: nbabsc, jabsc, jsigm, jinst, ncmp, iret, nbtran, jsioe, iocc
    integer :: nbins0, jnocc, ii, jresu, nbcycl, jcopr, jresp, jstoe, lo, le
    integer :: jsmoe
    parameter  ( ncmp = 6 )
    real(kind=8) :: r8b, prec(2), momen0, momen1, vale(2)
    complex(kind=8) :: cbid
    logical :: exist, cfait, flexii
    character(len=8) :: k8b, crit(2), nocmp(ncmp)
    character(len=16) :: motclf, valek(2), table, tabl0, tabfle, tabfl0, tabpre
    character(len=16) :: tabpr0
    character(len=19) :: nomf
    character(len=24) :: instan, abscur
    character(len=24) :: valk(7)
    integer :: iarg
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'TRANSITOIRE'
    call getfac(motclf, nbtran)
    if (nbtran .eq. 0) goto 9999
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
    instan = '&&RCEVO2.INSTANT'
    abscur = '&&RCEVO2.ABSC_CURV'
!
! --- RECHERCHE DU NOMBRE D'INSTANTS A COMBINER
!
    nbinst = 0
    flexio = .false.
    do 10, iocc = 1, nbtran, 1
    cfait = .false.
!
    call getvid(motclf, 'TABL_RESU_MECA', iocc, iarg, 1,&
                tabl0, n1)
    call getvid(motclf, 'TABL_SIGM_THER', iocc, iarg, 1,&
                tabfl0, n1)
    if (n1 .ne. 0) flexio = .true.
    call getvid(motclf, 'TABL_RESU_PRES', iocc, iarg, 1,&
                tabpr0, n1)
    if (n1 .ne. 0) lrocht = .true.
!
    nbins0 = 0
    call getvr8(motclf, 'INST', iocc, iarg, 0,&
                r8b, n1)
    if (n1 .ne. 0) then
        nbins0 = -n1
    else
        call getvid(motclf, 'LIST_INST', iocc, iarg, 1,&
                    nomf, n1)
        if (n1 .ne. 0) then
            call jelira(nomf//'.VALE', 'LONMAX', nbins0)
        else
            if (nbinti .eq. 1) then
                table = tabl0
                tabfle = tabfl0
                tabpre = tabpr0
            else
                cfait = .true.
                table = '&&RCEVO2.RESU_ME'
                tabfle = '&&RCEVO2.SIGM_TH'
                tabpre = '&&RCEVO2.RESU_PR'
                call tbextb(tabl0, 'V', table, 1, 'INTITULE',&
                            'EQ', ibid, r8b, cbid, kinti,&
                            r8b, k8b, iret)
                if (iret .eq. 10) then
                    valk(1) = 'INTITULE'
                    valk(2) = tabl0
                    call u2mesk('F', 'UTILITAI7_1', 2, valk)
                else if (iret .eq. 20) then
                    valk(1) = tabl0
                    valk(2) = 'INTITULE'
                    call u2mesk('F', 'UTILITAI7_3', 2, valk)
                endif
                if (flexio) then
                    call tbextb(tabfl0, 'V', tabfle, 1, 'INTITULE',&
                                'EQ', ibid, r8b, cbid, kinti,&
                                r8b, k8b, iret)
                    if (iret .eq. 10) then
                        valk(1) = 'INTITULE'
                        valk(2) = tabfl0
                        call u2mesk('F', 'UTILITAI7_1', 2, valk)
                    else if (iret .eq. 20) then
                        valk(1) = tabfl0
                        valk(2) = 'INTITULE'
                        call u2mesk('F', 'UTILITAI7_3', 2, valk)
                    endif
                endif
                if (lrocht) then
                    call tbextb(tabpr0, 'V', tabpre, 1, 'INTITULE',&
                                'EQ', ibid, r8b, cbid, kinti,&
                                r8b, k8b, iret)
                    if (iret .eq. 10) then
                        valk(1) = 'INTITULE'
                        valk(2) = tabpr0
                        call u2mesk('F', 'UTILITAI7_1', 2, valk)
                    else if (iret .eq. 20) then
                        valk(1) = tabpr0
                        valk(2) = 'INTITULE'
                        call u2mesk('F', 'UTILITAI7_3', 2, valk)
                    endif
                endif
            endif
            call tbexip(table, valek(1), exist, k8b)
            if (.not. exist) then
                valk (1) = table
                valk (2) = 'INTITULE'
                valk (3) = kinti
                valk (4) = valek(1)
                call u2mesg('F', 'POSTRCCM_17', 4, valk, 0,&
                            0, 0, 0.d0)
            endif
            call tbexv1(table, valek(1), instan, 'V', nbins0,&
                        k8b)
            call jedetr(instan)
        endif
    endif
    nbinst = nbinst + nbins0
!
! ----- PRESENCE DES COMPOSANTES DANS LA TABLE
!
    if (iocc .ne. 1) goto 14
!
    if (.not. cfait) then
        if (nbinti .eq. 1) then
            table = tabl0
            tabfle = tabfl0
            tabpre = tabpr0
        else
            cfait = .true.
            table = '&&RCEVO2.RESU_ME'
            tabfle = '&&RCEVO2.SIGM_TH'
            tabpre = '&&RCEVO2.RESU_PR'
            call tbextb(tabl0, 'V', table, 1, 'INTITULE',&
                        'EQ', ibid, r8b, cbid, kinti,&
                        r8b, k8b, iret)
            if (iret .eq. 10) then
                valk(1) = 'INTITULE'
                valk(2) = tabl0
                call u2mesk('F', 'UTILITAI7_1', 2, valk)
            else if (iret .eq. 20) then
                valk(1) = tabl0
                valk(2) = 'INTITULE'
                call u2mesk('F', 'UTILITAI7_3', 2, valk)
            endif
            if (flexio) then
                call tbextb(tabfl0, 'V', tabfle, 1, 'INTITULE',&
                            'EQ', ibid, r8b, cbid, kinti,&
                            r8b, k8b, iret)
                if (iret .eq. 10) then
                    valk(1) = 'INTITULE'
                    valk(2) = tabfl0
                    call u2mesk('F', 'UTILITAI7_1', 2, valk)
                else if (iret .eq. 20) then
                    valk(1) = tabfl0
                    valk(2) = 'INTITULE'
                    call u2mesk('F', 'UTILITAI7_3', 2, valk)
                endif
            endif
            if (lrocht) then
                call tbextb(tabpr0, 'V', tabpre, 1, 'INTITULE',&
                            'EQ', ibid, r8b, cbid, kinti,&
                            r8b, k8b, iret)
                if (iret .eq. 10) then
                    valk(1) = 'INTITULE'
                    valk(2) = tabpr0
                    call u2mesk('F', 'UTILITAI7_1', 2, valk)
                else if (iret .eq. 20) then
                    valk(1) = tabpr0
                    valk(2) = 'INTITULE'
                    call u2mesk('F', 'UTILITAI7_3', 2, valk)
                endif
            endif
        endif
    endif
!
    ncmpr = 6
    do 12 i = 1, 4
        call tbexip(table, nocmp(i), exist, k8b)
        if (.not. exist) then
            valk (1) = table
            valk (2) = 'INTITULE'
            valk (3) = kinti
            valk (4) = nocmp(i)
            call u2mesg('F', 'POSTRCCM_17', 4, valk, 0,&
                        0, 0, 0.d0)
        endif
        if (flexio) then
            call tbexip(tabfle, nocmp(i), exist, k8b)
            if (.not. exist) then
                valk (1) = tabfle
                valk (2) = 'INTITULE'
                valk (3) = kinti
                valk (4) = nocmp(i)
                call u2mesg('F', 'POSTRCCM_17', 4, valk, 0,&
                            0, 0, 0.d0)
            endif
        endif
        if (lrocht) then
            call tbexip(tabpre, nocmp(i), exist, k8b)
            if (.not. exist) then
                valk (1) = tabpre
                valk (2) = 'INTITULE'
                valk (3) = kinti
                valk (4) = nocmp(i)
                call u2mesg('F', 'POSTRCCM_17', 4, valk, 0,&
                            0, 0, 0.d0)
            endif
        endif
12  continue
    call tbexip(table, nocmp(5), exist, k8b)
    if (.not. exist) ncmpr = 4
!
! ----- ON RECUPERE L'ABSC_CURV DANS LA TABLE
!
    call tbexip(table, valek(2), exist, k8b)
    if (.not. exist) then
        valk (1) = table
        valk (2) = 'INTITULE'
        valk (3) = kinti
        valk (4) = valek(2)
        call u2mesg('F', 'POSTRCCM_17', 4, valk, 0,&
                    0, 0, 0.d0)
    endif
    call tbexv1(table, valek(2), abscur, 'V', nbabsc,&
                k8b)
!
14  continue
    if (cfait) then
        call detrsd('TABLE', table)
        if (flexio) call detrsd('TABLE', tabfle)
        if (lrocht) call detrsd('TABLE', tabpre)
    endif
!
    10 end do
!
    call jeveuo(abscur, 'L', jabsc)
    call wkvect('&&RCEVO2.CONTRAINTES', 'V V R', nbabsc, jcont)
    call wkvect('&&RCEVO2.CONT_FLEXIO', 'V V R', nbabsc, jcofl)
    call wkvect('&&RCEVO2.CONT_PRESSI', 'V V R', nbabsc, jcopr)
!
! --- CREATION DES OBJETS DE TRAVAIL
!
    ndim = 6 * nbinst * ncmp
    call wkvect(csigm, 'V V R', ndim, jsigm)
    call wkvect(cinst, 'V V R', nbinst, jinst)
    call wkvect(cnoc, 'V V I', nbinst, jnocc)
    call wkvect(cresu, 'V V K8', nbinst, jresu)
    if (lfatig) then
        ndim = 2 * nbinst * ncmp
        call wkvect(csiex, 'V V R', ndim, jsioe)
        if (kemixt) then
            call wkvect(cstex, 'V V R', ndim, jstoe)
            call wkvect(csmex, 'V V R', ndim, jsmoe)
        endif
    endif
    if (lrocht) then
        call wkvect(cpres, 'V V K8', nbtran, jresp)
    endif
!
! --- RECUPERATION DES INFORMATIONS
!
    ii = 0
    do 100, iocc = 1, nbtran, 1
!
    call getvis(motclf, 'NB_OCCUR', iocc, iarg, 1,&
                nbcycl, n1)
!
    call getvid(motclf, 'TABL_RESU_MECA', iocc, iarg, 1,&
                tabl0, n1)
!
    flexii = .false.
    call getvid(motclf, 'TABL_SIGM_THER', iocc, iarg, 1,&
                tabfl0, n1)
    if (n1 .ne. 0) flexii = .true.
!
    call getvid(motclf, 'TABL_RESU_PRES', iocc, iarg, 1,&
                tabpr0, n1)
    if (n1 .ne. 0) then
        lrocht = .true.
        zk8(jresp-1+iocc) = tabpr0
    endif
!
    if (nbinti .eq. 1) then
        table = tabl0
        tabfle = tabfl0
        tabpre = tabpr0
    else
        table = '&&RCEVO2.RESU_ME'
        tabfle = '&&RCEVO2.SIGM_TH'
        tabpre = '&&RCEVO2.RESU_PR'
        call tbextb(tabl0, 'V', table, 1, 'INTITULE',&
                    'EQ', ibid, r8b, cbid, kinti,&
                    r8b, k8b, iret)
        if (iret .eq. 10) then
            valk(1) = 'INTITULE'
            valk(2) = tabl0
            call u2mesk('F', 'UTILITAI7_1', 2, valk)
        else if (iret .eq. 20) then
            valk(1) = tabl0
            valk(2) = 'INTITULE'
            call u2mesk('F', 'UTILITAI7_3', 2, valk)
        endif
        if (flexii) then
            call tbextb(tabfl0, 'V', tabfle, 1, 'INTITULE',&
                        'EQ', ibid, r8b, cbid, kinti,&
                        r8b, k8b, iret)
            if (iret .eq. 10) then
                valk(1) = 'INTITULE'
                valk(2) = tabfl0
                call u2mesk('F', 'UTILITAI7_1', 2, valk)
            else if (iret .eq. 20) then
                valk(1) = tabfl0
                valk(2) = 'INTITULE'
                call u2mesk('F', 'UTILITAI7_3', 2, valk)
            endif
        endif
        if (lrocht) then
            call tbextb(tabpr0, 'V', tabpre, 1, 'INTITULE',&
                        'EQ', ibid, r8b, cbid, kinti,&
                        r8b, k8b, iret)
            if (iret .eq. 10) then
                valk(1) = 'INTITULE'
                valk(2) = tabpr0
                call u2mesk('F', 'UTILITAI7_1', 2, valk)
            else if (iret .eq. 20) then
                valk(1) = tabpr0
                valk(2) = 'INTITULE'
                call u2mesk('F', 'UTILITAI7_3', 2, valk)
            endif
        endif
    endif
!
! ----- ON RECUPERE LES INSTANTS DANS LA TABLE
!
    call getvr8(motclf, 'INST', iocc, iarg, 0,&
                r8b, n1)
    if (n1 .ne. 0) then
        nbins0 = -n1
        call wkvect(instan, 'V V R', nbins0, kinst)
        call getvr8(motclf, 'INST', iocc, iarg, nbins0,&
                    zr(kinst), n1)
        call getvr8(motclf, 'PRECISION', iocc, iarg, 1,&
                    prec(1), n1)
        call getvtx(motclf, 'CRITERE', iocc, iarg, 1,&
                    crit(1), n1)
    else
        call getvid(motclf, 'LIST_INST', iocc, iarg, 1,&
                    nomf, n1)
        if (n1 .ne. 0) then
            call jelira(nomf//'.VALE', 'LONMAX', nbins0)
            call jeveuo(nomf//'.VALE', 'L', kinst)
            call getvr8(motclf, 'PRECISION', iocc, iarg, 1,&
                        prec(1), n1)
            call getvtx(motclf, 'CRITERE', iocc, iarg, 1,&
                        crit(1), n1)
        else
            prec(1) = 1.0d-06
            crit(1) = 'RELATIF'
            call tbexip(table, valek(1), exist, k8b)
            if (.not. exist) then
                valk (1) = table
                valk (2) = 'INTITULE'
                valk (3) = kinti
                valk (4) = valek(1)
                call u2mesg('F', 'POSTRCCM_17', 4, valk, 0,&
                            0, 0, 0.d0)
            endif
            call tbexv1(table, valek(1), instan, 'V', nbins0,&
                        k8b)
            call jeveuo(instan, 'L', kinst)
        endif
    endif
!
    do 102 i = 1, nbins0
!
        ii = ii + 1
        zr (jinst+ii-1) = zr(kinst+i-1)
        zi (jnocc-1+ii) = nbcycl
        zk8(jresu-1+ii) = tabl0
!
        vale(1) = zr(kinst+i-1)
!
        do 104 j = 1, ncmpr
!
            do 106 k = 1, nbabsc
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
!
                if (flexii) then
                    call tbliva(tabfle, 2, valek, ibid, vale,&
                                cbid, k8b, crit, prec, nocmp(j),&
                                k8b, ibid, zr(jcofl+k-1), cbid, k8b,&
                                iret)
                    if (iret .ne. 0) then
                        valk (1) = tabfle
                        valk (2) = nocmp(j)
                        valk (3) = valek(1)
                        valk (4) = valek(2)
                        call u2mesg('F', 'POSTRCCM_2', 4, valk, 0,&
                                    0, 2, vale)
                    endif
                endif
!
                if (lrocht) then
                    call tbliva(tabpre, 2, valek, ibid, vale,&
                                cbid, k8b, crit, prec, nocmp(j),&
                                k8b, ibid, zr(jcopr+k-1), cbid, k8b,&
                                iret)
                    if (iret .ne. 0) then
                        valk (1) = tabpre
                        valk (2) = nocmp(j)
                        valk (3) = valek(1)
                        valk (4) = valek(2)
                        call u2mesg('F', 'POSTRCCM_2', 4, valk, 0,&
                                    0, 2, vale)
                    endif
                endif
!
106          continue
!
            if (lfatig) then
                lo = ncmp*(ii-1) + j
                le = ncmp*nbinst + ncmp*(ii-1) + j
                zr(jsioe-1+lo) = zr(jcont)
                zr(jsioe-1+le) = zr(jcont+nbabsc-1)
                if (kemixt) then
                    if (flexii) then
                        zr(jstoe-1+lo) = zr(jcofl)
                        zr(jstoe-1+le) = zr(jcofl+nbabsc-1)
                    else
                        zr(jstoe-1+lo) = 0.d0
                        zr(jstoe-1+le) = 0.d0
                    endif
                    zr(jsmoe-1+lo) = zr(jcont) - zr(jstoe-1+lo)
                    zr(jsmoe-1+le) = zr(jcont+nbabsc-1) - zr( jstoe-1+le)
                endif
            endif
!
            call rc32my(nbabsc, zr(jabsc), zr(jcont), momen0, momen1)
            momen1 = 0.5d0*momen1
!
            l = ncmp*(ii-1) + j
            zr(jsigm-1+l) = momen0
            l = ncmp*nbinst + ncmp*(ii-1) + j
            zr(jsigm-1+l) = momen1
!
            if (flexii) then
                call rc32my(nbabsc, zr(jabsc), zr(jcofl), momen0, momen1)
                momen1 = 0.5d0*momen1
            else
                momen0 = 0.d0
                momen1 = 0.d0
            endif
            l = 2*ncmp*nbinst + ncmp*(ii-1) + j
            zr(jsigm-1+l) = momen0
            l = 3*ncmp*nbinst + ncmp*(ii-1) + j
            zr(jsigm-1+l) = momen1
!
            if (lrocht) then
                call rc32my(nbabsc, zr(jabsc), zr(jcopr), momen0, momen1)
                momen1 = 0.5d0*momen1
            else
                momen0 = r8vide()
                momen1 = r8vide()
            endif
            l = 4*ncmp*nbinst + ncmp*(ii-1) + j
            zr(jsigm-1+l) = momen0
            l = 5*ncmp*nbinst + ncmp*(ii-1) + j
            zr(jsigm-1+l) = momen1
!
104      continue
!
102  continue
    call jedetr(instan)
    if (nbinti .ne. 1) then
        call detrsd('TABLE', table)
        if (flexii) call detrsd('TABLE', tabfle)
        if (lrocht) call detrsd('TABLE', tabpre)
    endif
!
    100 end do
!
    call jedetr(abscur)
    call jedetr('&&RCEVO2.CONTRAINTES')
    call jedetr('&&RCEVO2.CONT_FLEXIO')
    call jedetr('&&RCEVO2.CONT_PRESSI')
!
9999  continue
    call jedema()
end subroutine
