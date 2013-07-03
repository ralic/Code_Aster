subroutine rcevom(csigm, cinst, cnoc, sm, lfatig,&
                  lpmpb, lsn, csno, csne, flexio,&
                  csneo, csnee, cfao, cfae, cspo,&
                  cspe, cresu, kinti, it, jt,&
                  lrocht, symax, cpres, kemixt, cspto,&
                  cspte, cspmo, cspme)
! aslint: disable=W1501,W1501,W1504
    implicit   none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcevfu.h"
#include "asterfort/rcmcrt.h"
#include "asterfort/rctres.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
    integer :: it, jt
    real(kind=8) :: sm, symax
    logical :: lfatig, lpmpb, lsn, flexio, lrocht, kemixt
    character(len=16) :: kinti
    character(len=24) :: csigm, cinst, cnoc, csno, csne, csneo, csnee, cfao
    character(len=24) :: cfae, cspo, cspe, cresu, cpres, cspto, cspte, cspmo
    character(len=24) :: cspme
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
!     TYPE_RESU = 'VALE_MAX'
!
!     ------------------------------------------------------------------
!
    integer :: ncmp, jsigm, jinst, nbinst, nbordr, jsno, jsne, ind, i1, i2, icmp
    integer :: l1, l2, l3, l4, npara, ik, ir, i, vaio(5), vaie(5), ioo1, ioo2
    integer :: ioe1, ioe2, npar1, jspo, jspe, jfao, jfae, jnoc, jresu, jresp
    integer :: jspto, jspte, jspmo, jspme
    parameter  ( ncmp = 6 )
    real(kind=8) :: tpm(ncmp), tpb(ncmp), tpmpbo(ncmp), tpmpbe(ncmp), pm, pb
    real(kind=8) :: pmpbo, pmpbe, ipm, ipb, ipmpbo, ipmpbe, sno, sne, i1sno
    real(kind=8) :: i2sno, i1sne, i2sne, spo, spe, keo, kee, sao, sae, nao, nae
    real(kind=8) :: doo, doe, dco, dce, stlin, stpar, ketho, kethe, tresca
    real(kind=8) :: valo(39), vale(39), spmo, spme, spto, spte
    complex(kind=8) :: c16b
    character(len=8) :: k8b, nomres, typara(39), rpm, rpb, rpmpbo, rpmpbe, r1sno
    character(len=8) :: r1sne, r2sno, r2sne
    character(len=16) :: nomcmd, concep, nopara(39), vako(5), vake(5)
!
    integer :: nparen, nparpm, nparsn, nparse, nparf1, nparrt, nparf2
    parameter  ( nparen=4, nparpm=7, nparsn=5, nparse=3, nparf1=10,&
     &             nparrt=5 , nparf2=13)
    character(len=8) :: typaen(nparen), typapm(nparpm), typasn(nparsn)
    character(len=8) :: typase(nparse), typaf1(nparf1), typart(nparrt)
    character(len=8) :: typaf2(nparf2)
    character(len=16) :: nopaen(nparen), nopapm(nparpm), nopasn(nparsn)
    character(len=16) :: nopase(nparse), nopaf1(nparf1), nopart(nparrt)
    character(len=16) :: nopaf2(nparf2)
!
    data nopaen / 'INTITULE', 'LIEU', 'SM', '3SM' /
    data typaen / 'K16',      'K8'  , 'R' , 'R'   /
    data nopart / 'TABL_PRES', 'SY', 'SIGM_M_PRES',&
     &              'VALE_MAXI_LINE', 'VALE_MAXI_PARAB' /
    data typart / 'K8', 'R' , 'R'   , 'R'   , 'R'   /
    data nopapm / 'TABL_RESU', 'INST_PM', 'PM', 'INST_PB', 'PB',&
     &              'INST_PMB' , 'PMB' /
    data typapm / 'K8', 'R', 'R', 'R', 'R', 'R', 'R' /
    data nopasn / 'TABL_RESU_1', 'INST_SN_1',&
     &              'TABL_RESU_2', 'INST_SN_2', 'SN' /
    data typasn / 'K8', 'R' ,'K8', 'R', 'R'  /
    data nopase / 'INST_SN*_1', 'INST_SN*_2', 'SN*' /
    data typase / 'R' , 'R', 'R'  /
    data nopaf1 / 'INST_SALT_1', 'NB_OCCUR_1',&
     &              'INST_SALT_2', 'NB_OCCUR_2',&
     &              'SP', 'KE', 'SALT', 'NADM', 'DOMMAGE',&
     &              'DOMMAGE_CUMU' /
    data typaf1 / 'R', 'I', 'R', 'I','R', 'R', 'R', 'R', 'R', 'R' /
    data nopaf2 / 'INST_SALT_1', 'NB_OCCUR_1',&
     &              'INST_SALT_2', 'NB_OCCUR_2',&
     &              'SP', 'SP_MECA','SP_THER','KE_MECA', 'KE_THER',&
     &              'SALT', 'NADM', 'DOMMAGE', 'DOMMAGE_CUMU' /
    data typaf2 / 'R', 'I', 'R', 'I','R', 'R', 'R', 'R', 'R', 'R',&
     &              'R', 'R', 'R' /
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call getres(nomres, concep, nomcmd)
!
    call jeveuo(cresu, 'L', jresu)
    call jeveuo(cinst, 'L', jinst)
    call jelira(cinst, 'LONMAX', nbinst, k8b)
!
! --- CREATION DE LA TABLE
!
    if (it .eq. 1 .and. jt .eq. 1) then
        npara = nparen
        do 10 i = 1, nparen
            nopara(i) = nopaen(i)
            typara(i) = typaen(i)
10      continue
        if (lrocht) then
            do 11 i = 1, nparrt
                nopara(npara+i) = nopart(i)
                typara(npara+i) = typart(i)
11          continue
            npara = npara + nparrt
        endif
        if (lpmpb) then
            do 12 i = 1, nparpm
                nopara(npara+i) = nopapm(i)
                typara(npara+i) = typapm(i)
12          continue
            npara = npara + nparpm
        endif
        if (lsn) then
            do 14 i = 1, nparsn
                nopara(npara+i) = nopasn(i)
                typara(npara+i) = typasn(i)
14          continue
            npara = npara + nparsn
        endif
        if (flexio .and. lsn) then
            do 16 i = 1, nparse
                nopara(npara+i) = nopase(i)
                typara(npara+i) = typase(i)
16          continue
            npara = npara + nparse
        endif
        if (lfatig) then
            if (.not. kemixt) then
                do 18 i = 1, nparf1
                    nopara(npara+i) = nopaf1(i)
                    typara(npara+i) = typaf1(i)
18              continue
                npara = npara + nparf1
            else
                do 19 i = 1, nparf2
                    nopara(npara+i) = nopaf2(i)
                    typara(npara+i) = typaf2(i)
19              continue
                npara = npara + nparf2
            endif
        endif
!
        call tbcrsd(nomres, 'G')
        call tbajpa(nomres, npara, nopara, typara)
    endif
!
! --- LES LIGNES LIEU ET SM
!
    ik = 0
    npara = nparen
    do 30 i = 1, nparen
        nopara(i) = nopaen(i)
30  end do
    ik = ik + 1
    vako(ik) = kinti
    vake(ik) = kinti
    ik = ik + 1
    vako(ik) = 'ORIG'
    vake(ik) = 'EXTR'
!
    valo(1) = sm
    vale(1) = sm
!
    valo(2) = 3*sm
    vale(2) = 3*sm
!
! --- POUR LE ROCHET THERMIQUE
!
    if (lrocht) then
        call jeveuo(csigm, 'L', jsigm)
        call jeveuo(cpres, 'L', jresp)
! RECHERCHE DU MAXIMUM DE LA CONTRAINTE DE MEMBRANE DUE A LA PRESSION
        pm = 0.d0
        do 400 i = 1, nbinst
            do 402 icmp = 1, ncmp
                l3 = 4*ncmp*nbinst + ncmp*(i-1) + icmp
                tpm(icmp) = zr(jsigm-1+l3)
402          continue
            call rctres(tpm, tresca)
            if (tresca .gt. pm) pm = tresca
400      continue
        call rcmcrt(symax, pm, stlin, stpar)
!
        npar1 = npara + 1
        nopara(npar1) = 'TABL_PRES'
        vako(ik+1) = zk8(jresp-1+jt)
        vake(ik+1) = zk8(jresp-1+jt)
        npar1 = npar1 + 1
        nopara(npar1) = 'SY'
        ir = 2 + 1
        valo(ir) = symax
        vale(ir) = symax
        npar1 = npar1 + 1
        nopara(npar1) = 'SIGM_M_PRES'
        ir = ir + 1
        valo(ir) = pm
        vale(ir) = pm
        npar1 = npar1 + 1
        nopara(npar1) = 'VALE_MAXI_LINE'
        ir = ir + 1
        valo(ir) = stlin
        vale(ir) = stlin
        npar1 = npar1 + 1
        nopara(npar1) = 'VALE_MAXI_PARAB'
        ir = ir + 1
        valo(ir) = stpar
        vale(ir) = stpar
        call tbajli(nomres, npar1, nopara, vaio, valo,&
                    c16b, vako, 0)
        call tbajli(nomres, npar1, nopara, vaie, vale,&
                    c16b, vake, 0)
!
    endif
!
! --- POUR L'OPTION "PMPB"
!
    if (lpmpb) then
!
! --- LES CRITERES DE NIVEAU 0 VISENT A PREMUNIR LE MATERIEL CONTRE LES
!     DOMMAGES DE DEFORMATION EXCESSIVE, D'INSTABILITE PLASTIQUE ET
!     D'INSTABILITE ELASTIQUE ET ELASTOPLASTIQUE.
!     ON NE PREND QUE LA PARTIE MECANIQUE
!
        call jeveuo(csigm, 'L', jsigm)
        pm = 0.d0
        pb = 0.d0
        pmpbo = 0.d0
        pmpbe = 0.d0
        rpm = zk8(jresu)
        rpb = zk8(jresu)
        rpmpbo = zk8(jresu)
        rpmpbe = zk8(jresu)
        ipm = zr(jinst)
        ipb = zr(jinst)
        ipmpbo = zr(jinst)
        ipmpbe = zr(jinst)
        do 100 i = 1, nbinst
            do 102 icmp = 1, ncmp
                l1 = ncmp*(i-1) + icmp
                l2 = ncmp*nbinst + ncmp*(i-1) + icmp
                l3 = 2*ncmp*nbinst + ncmp*(i-1) + icmp
                l4 = 3*ncmp*nbinst + ncmp*(i-1) + icmp
                tpm(icmp) = zr(jsigm-1+l1) - zr(jsigm-1+l3)
                tpb(icmp) = zr(jsigm-1+l2) - zr(jsigm-1+l4)
                tpmpbo(icmp) = zr(jsigm-1+l1) - zr(jsigm-1+l2) - ( zr(jsigm-1+l3) - zr(jsigm-1+l4&
                               &) )
                tpmpbe(icmp) = zr(jsigm-1+l1) + zr(jsigm-1+l2) - ( zr(jsigm-1+l3) + zr(jsigm-1+l4&
                               &) )
102          continue
            call rctres(tpm, tresca)
            if (tresca .gt. pm) then
                pm = tresca
                ipm = zr(jinst+i-1)
                rpm = zk8(jresu+i-1)
            endif
            call rctres(tpb, tresca)
            if (tresca .gt. pb) then
                pb = tresca
                ipb = zr(jinst+i-1)
                rpb = zk8(jresu+i-1)
            endif
            call rctres(tpmpbo, tresca)
            if (tresca .gt. pmpbo) then
                pmpbo = tresca
                ipmpbo = zr(jinst+i-1)
                rpmpbo = zk8(jresu+i-1)
            endif
            call rctres(tpmpbe, tresca)
            if (tresca .gt. pmpbe) then
                pmpbe = tresca
                ipmpbe = zr(jinst+i-1)
                rpmpbe = zk8(jresu+i-1)
            endif
100      continue
!
        npar1 = npara + 1
        nopara(npar1) = 'TABL_RESU'
        vako(ik+1) = rpm
        vake(ik+1) = rpm
        npar1 = npar1 + 1
        nopara(npar1) = 'INST_PM'
        ir = 2 + 1
        valo(ir) = ipm
        vale(ir) = ipm
        npar1 = npar1 + 1
        nopara(npar1) = 'PM'
        ir = ir + 1
        valo(ir) = pm
        vale(ir) = pm
        call tbajli(nomres, npar1, nopara, vaio, valo,&
                    c16b, vako, 0)
        call tbajli(nomres, npar1, nopara, vaie, vale,&
                    c16b, vake, 0)
!
        npar1 = npara + 1
        nopara(npar1) = 'TABL_RESU'
        vako(ik+1) = rpb
        vake(ik+1) = rpb
        npar1 = npar1 + 1
        nopara(npar1) = 'INST_PB'
        ir = 2 + 1
        valo(ir) = ipb
        vale(ir) = ipb
        npar1 = npar1 + 1
        nopara(npar1) = 'PB'
        ir = ir + 1
        valo(ir) = pb
        vale(ir) = pb
        call tbajli(nomres, npar1, nopara, vaio, valo,&
                    c16b, vako, 0)
        call tbajli(nomres, npar1, nopara, vaie, vale,&
                    c16b, vake, 0)
!
        npar1 = npara + 1
        nopara(npar1) = 'TABL_RESU'
        vako(ik+1) = rpmpbo
        vake(ik+1) = rpmpbe
        npar1 = npar1 + 1
        nopara(npar1) = 'INST_PMB'
        ir = 2 + 1
        valo(ir) = ipmpbo
        vale(ir) = ipmpbe
        npar1 = npar1 + 1
        nopara(npar1) = 'PMB'
        ir = ir + 1
        valo(ir) = pmpbo
        vale(ir) = pmpbe
        call tbajli(nomres, npar1, nopara, vaio, valo,&
                    c16b, vako, 0)
        call tbajli(nomres, npar1, nopara, vaie, vale,&
                    c16b, vake, 0)
    endif
!
! --- POUR L'OPTION "SN"
!
    if (lsn) then
        call jeveuo(csno, 'L', jsno)
        call jeveuo(csne, 'L', jsne)
        sno = 0.d0
        sne = 0.d0
        ind = 0
        r1sno = zk8(jresu)
        r2sno = zk8(jresu)
        r1sne = zk8(jresu)
        r2sne = zk8(jresu)
        i1sno = zr(jinst)
        i2sno = zr(jinst)
        i1sne = zr(jinst)
        i2sne = zr(jinst)
        do 200 i1 = 1, nbinst
            ind = ind + 1
            if (zr(jsno+ind-1) .gt. sno) then
                sno = zr(jsno+ind-1)
                i1sno = zr(jinst+i1-1)
                i2sno = zr(jinst+i1-1)
                r1sno = zk8(jresu+i1-1)
                r2sno = zk8(jresu+i1-1)
            endif
            if (zr(jsne+ind-1) .gt. sne) then
                sne = zr(jsne+ind-1)
                i1sne = zr(jinst+i1-1)
                i2sne = zr(jinst+i1-1)
                r1sne = zk8(jresu+i1-1)
                r2sne = zk8(jresu+i1-1)
            endif
            do 202 i2 = i1+1, nbinst
                ind = ind + 1
                if (zr(jsno+ind-1) .gt. sno) then
                    sno = zr(jsno+ind-1)
                    i1sno = zr(jinst+i1-1)
                    i2sno = zr(jinst+i2-1)
                    r1sno = zk8(jresu+i1-1)
                    r2sno = zk8(jresu+i2-1)
                endif
                if (zr(jsne+ind-1) .gt. sne) then
                    sne = zr(jsne+ind-1)
                    i1sne = zr(jinst+i1-1)
                    i2sne = zr(jinst+i2-1)
                    r1sne = zk8(jresu+i1-1)
                    r2sne = zk8(jresu+i2-1)
                endif
202          continue
200      continue
!
        npar1 = npara + 1
        nopara(npar1) = 'TABL_RESU_1'
        vako(ik+1) = r1sno
        vake(ik+1) = r1sne
        npar1 = npar1 + 1
        nopara(npar1) = 'INST_SN_1'
        ir = 2 + 1
        valo(ir) = i1sno
        vale(ir) = i1sne
!
        npar1 = npar1 + 1
        nopara(npar1) = 'TABL_RESU_2'
        vako(ik+2) = r2sno
        vake(ik+2) = r2sne
        npar1 = npar1 + 1
        nopara(npar1) = 'INST_SN_2'
        ir = ir + 1
        valo(ir) = i2sno
        vale(ir) = i2sne
!
        npar1 = npar1 + 1
        nopara(npar1) = 'SN'
        ir = ir + 1
        valo(ir) = sno
        vale(ir) = sne
        call tbajli(nomres, npar1, nopara, vaio, valo,&
                    c16b, vako, 0)
        call tbajli(nomres, npar1, nopara, vaie, vale,&
                    c16b, vake, 0)
    endif
!
    if (flexio .and. lsn) then
        call jeveuo(csneo, 'L', jsno)
        call jeveuo(csnee, 'L', jsne)
        sno = 0.d0
        sne = 0.d0
        sno = 0.d0
        sne = 0.d0
        ind = 0
        r1sno = zk8(jresu)
        r2sno = zk8(jresu)
        r1sne = zk8(jresu)
        r2sne = zk8(jresu)
        i1sno = zr(jinst)
        i2sno = zr(jinst)
        i1sne = zr(jinst)
        i2sne = zr(jinst)
        do 210 i1 = 1, nbinst
            ind = ind + 1
            if (zr(jsno+ind-1) .gt. sno) then
                sno = zr(jsno+ind-1)
                i1sno = zr(jinst+i1-1)
                i2sno = zr(jinst+i1-1)
                r1sno = zk8(jresu+i1-1)
                r2sno = zk8(jresu+i1-1)
            endif
            if (zr(jsne+ind-1) .gt. sne) then
                sne = zr(jsne+ind-1)
                i1sne = zr(jinst+i1-1)
                i2sne = zr(jinst+i1-1)
                r1sne = zk8(jresu+i1-1)
                r2sne = zk8(jresu+i1-1)
            endif
            do 212 i2 = i1+1, nbinst
                ind = ind + 1
                if (zr(jsno+ind-1) .gt. sno) then
                    sno = zr(jsno+ind-1)
                    i1sno = zr(jinst+i1-1)
                    i2sno = zr(jinst+i2-1)
                    r1sno = zk8(jresu+i1-1)
                    r2sno = zk8(jresu+i2-1)
                endif
                if (zr(jsne+ind-1) .gt. sne) then
                    sne = zr(jsne+ind-1)
                    i1sne = zr(jinst+i1-1)
                    i2sne = zr(jinst+i2-1)
                    r1sne = zk8(jresu+i1-1)
                    r2sne = zk8(jresu+i2-1)
                endif
212          continue
210      continue
!
        npar1 = npara + 1
        nopara(npar1) = 'TABL_RESU_1'
        vako(ik+1) = r1sno
        vake(ik+1) = r1sne
        npar1 = npar1 + 1
        nopara(npar1) = 'INST_SN*_1'
        ir = 2 + 1
        valo(ir) = i1sno
        vale(ir) = i1sne
!
        npar1 = npar1 + 1
        nopara(npar1) = 'TABL_RESU_2'
        vako(ik+2) = r2sno
        vake(ik+2) = r2sne
        npar1 = npar1 + 1
        nopara(npar1) = 'INST_SN*_2'
        ir = ir + 1
        valo(ir) = i2sno
        vale(ir) = i2sne
!
        npar1 = npar1 + 1
        nopara(npar1) = 'SN*'
        ir = ir + 1
        valo(ir) = sno
        vale(ir) = sne
        call tbajli(nomres, npar1, nopara, vaio, valo,&
                    c16b, vako, 0)
        call tbajli(nomres, npar1, nopara, vaie, vale,&
                    c16b, vake, 0)
    endif
!
! --- POUR L'OPTION "FATIGUE"
!
    if (lfatig) then
        call jelira(cspo, 'LONMAX', nbordr, k8b)
        call jeveuo(cspo, 'L', jspo)
        call jeveuo(cspe, 'L', jspe)
        call jeveuo(cfao, 'L', jfao)
        call jeveuo(cfae, 'L', jfae)
        call jeveuo(cnoc, 'L', jnoc)
        spo = 0.d0
        keo = 0.d0
        sao = 0.d0
        nao = 0.d0
        doo = 0.d0
        spe = 0.d0
        kee = 0.d0
        sae = 0.d0
        nae = 0.d0
        doe = 0.d0
        r1sno = zk8(jresu)
        r2sno = zk8(jresu)
        r1sne = zk8(jresu)
        r2sne = zk8(jresu)
        i1sno = zr(jinst)
        i2sno = zr(jinst)
        i1sne = zr(jinst)
        i2sne = zr(jinst)
        if (kemixt) then
            call jeveuo(cspmo, 'L', jspmo)
            call jeveuo(cspme, 'L', jspme)
            call jeveuo(cspto, 'L', jspto)
            call jeveuo(cspte, 'L', jspte)
            spmo = 0.d0
            spme = 0.d0
            spto = 0.d0
            spte = 0.d0
            ketho = 0.d0
            kethe = 0.d0
        endif
        do 300 i = 1, nbordr
            spo = max ( spo , zr(jspo-1+i) )
            keo = max ( keo , zr(jfao-1+5*(i-1)+1) )
            nao = max ( nao , zr(jfao-1+5*(i-1)+3) )
            doo = max ( doo , zr(jfao-1+5*(i-1)+4) )
            spe = max ( spe , zr(jspe-1+i) )
            kee = max ( kee , zr(jfae-1+5*(i-1)+1) )
            nae = max ( nae , zr(jfae-1+5*(i-1)+3) )
            doe = max ( doe , zr(jfae-1+5*(i-1)+4) )
            if (kemixt) then
                spmo = max ( spmo , zr(jspmo-1+i) )
                spto = max ( spto , zr(jspto-1+i) )
                spme = max ( spme , zr(jspme-1+i) )
                spte = max ( spte , zr(jspte-1+i) )
                ketho = max ( ketho , zr(jfao-1+5*(i-1)+5) )
                kethe = max ( kethe , zr(jfae-1+5*(i-1)+5) )
            endif
300      continue
        ind = 0
        do 310 i1 = 1, nbinst
            ind = ind + 1
            if (zr(jfao-1+4*(ind-1)+2) .gt. sao) then
                sao = zr(jfao-1+4*(ind-1)+2)
                ioo1 = zi(jnoc+i1-1)
                ioo2 = zi(jnoc+i1-1)
                r1sno = zk8(jresu+i1-1)
                r2sno = zk8(jresu+i1-1)
                i1sno = zr(jinst+i1-1)
                i2sno = zr(jinst+i1-1)
            endif
            if (zr(jfae-1+4*(ind-1)+2) .gt. sae) then
                sao = zr(jfae-1+4*(ind-1)+2)
                ioe1 = zi(jnoc+i1-1)
                ioe2 = zi(jnoc+i1-1)
                r1sne = zk8(jresu+i1-1)
                r2sne = zk8(jresu+i1-1)
                i1sne = zr(jinst+i1-1)
                i2sne = zr(jinst+i1-1)
            endif
            do 312 i2 = i1+1, nbinst
                ind = ind + 1
                if (zr(jfao-1+4*(ind-1)+2) .gt. sao) then
                    sao = zr(jfao-1+4*(ind-1)+2)
                    ioo1 = zi(jnoc+i1-1)
                    ioo2 = zi(jnoc+i2-1)
                    r1sno = zk8(jresu+i1-1)
                    r2sno = zk8(jresu+i2-1)
                    i1sno = zr(jinst+i1-1)
                    i2sno = zr(jinst+i2-1)
                endif
                if (zr(jfae-1+4*(ind-1)+2) .gt. sae) then
                    sae = zr(jfae-1+4*(ind-1)+2)
                    ioe1 = zi(jnoc+i1-1)
                    ioe2 = zi(jnoc+i2-1)
                    r1sne = zk8(jresu+i1-1)
                    r2sne = zk8(jresu+i2-1)
                    i1sne = zr(jinst+i1-1)
                    i2sne = zr(jinst+i2-1)
                endif
312          continue
310      continue
!
        npar1 = npara + 1
        nopara(npar1) = 'TABL_RESU_1'
        vako(ik+1) = r1sno
        vake(ik+1) = r1sne
        npar1 = npar1 + 1
        nopara(npar1) = 'INST_SALT_1'
        ir = 2 + 1
        valo(ir) = i1sno
        vale(ir) = i1sne
        npar1 = npar1 + 1
        nopara(npar1) = 'NB_OCCUR_1'
        vaio(1) = ioo1
        vaie(1) = ioe1
!
        npar1 = npar1 + 1
        nopara(npar1) = 'TABL_RESU_2'
        vako(ik+2) = r2sno
        vake(ik+2) = r2sne
        npar1 = npar1 + 1
        nopara(npar1) = 'INST_SALT_2'
        ir = ir + 1
        valo(ir) = i2sno
        vale(ir) = i2sne
        npar1 = npar1 + 1
        nopara(npar1) = 'NB_OCCUR_2'
        vaio(2) = ioo2
        vaie(2) = ioe2
!
        npar1 = npar1 + 1
        nopara(npar1) = 'SP'
        ir = ir + 1
        valo(ir) = spo
        vale(ir) = spe
        if (kemixt) then
            npar1 = npar1 + 1
            nopara(npar1) = 'SP_MECA'
            ir = ir + 1
            valo(ir) = spmo
            vale(ir) = spme
            npar1 = npar1 + 1
            nopara(npar1) = 'SP_THER'
            ir = ir + 1
            valo(ir) = spto
            vale(ir) = spte
            npar1 = npar1 + 1
            nopara(npar1) = 'KE_MECA'
            ir = ir + 1
            valo(ir) = keo
            vale(ir) = kee
            npar1 = npar1 + 1
            nopara(npar1) = 'KE_THER'
            ir = ir + 1
            valo(ir) = ketho
            vale(ir) = kethe
        else
            npar1 = npar1 + 1
            nopara(npar1) = 'KE'
            ir = ir + 1
            valo(ir) = keo
            vale(ir) = kee
        endif
        npar1 = npar1 + 1
        nopara(npar1) = 'SALT'
        ir = ir + 1
        valo(ir) = sao
        vale(ir) = sae
        npar1 = npar1 + 1
        nopara(npar1) = 'NADM'
        ir = ir + 1
        valo(ir) = nao
        vale(ir) = nae
        npar1 = npar1 + 1
        nopara(npar1) = 'DOMMAGE'
        ir = ir + 1
        valo(ir) = doo
        vale(ir) = doe
        call tbajli(nomres, npar1, nopara, vaio, valo,&
                    c16b, vako, 0)
        call tbajli(nomres, npar1, nopara, vaie, vale,&
                    c16b, vake, 0)
!
        call rcevfu(cnoc, cfao, dco)
        call rcevfu(cnoc, cfae, dce)
!
        npar1 = npara + 1
        nopara(npar1) = 'DOMMAGE_CUMU'
        call rcevfu(cnoc, cfao, dco)
        call rcevfu(cnoc, cfae, dce)
        valo(3) = dco
        vale(3) = dce
        call tbajli(nomres, npar1, nopara, vaio, valo,&
                    c16b, vako, 0)
        call tbajli(nomres, npar1, nopara, vaie, vale,&
                    c16b, vake, 0)
    endif
!
    call jedema()
end subroutine
