subroutine rcevod(csigm, cinst, cnoc, sm, lfatig,&
                  lpmpb, lsn, csno, csne, flexio,&
                  csneo, csnee, cfao, cfae, cspo,&
                  cspe, cresu, kinti, it, jt,&
                  lrocht, symax, cpres, kemixt, cspto,&
                  cspte, cspmo, cspme)
! aslint: disable=W1504
    implicit   none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/infniv.h"
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
!     TYPE_RESU = 'DETAILS'
!
!     ------------------------------------------------------------------
!
    integer :: ncmp, jsigm, jinst, nbinst, jsno, jsne, ind, i1, i2, icmp, l1, l2
    integer :: l3, l4, npara, ik, ir, i, vaio(5), vaie(5), npar1, jresp, jsneo
    integer :: jsnee, jspo, jspe, jfao, jfae, jnoc, jresu, jspto, jspte, jspmo
    integer :: jspme
    parameter  ( ncmp = 6 )
    real(kind=8) :: tpm(ncmp), tpb(ncmp), tpmpbo(ncmp), tpmpbe(ncmp), dco, dce
    real(kind=8) :: tresca, valo(39), vale(39), stlin, stpar
    complex(kind=8) :: c16b
    character(len=8) ::  nomres, typara(39)
    character(len=16) :: nomcmd, concep, nopara(39), vako(5), vake(5)
!
    integer :: nparen, nparpm, nparsn, nparse, nparf1, nparf2, nparf3, nparrt
    integer :: ifm, niv
    parameter  ( nparen=4, nparpm=5, nparsn=5, nparse=1,&
     &             nparf1=14, nparf2=13, nparrt=6 , nparf3=17)
    character(len=8) :: typaen(nparen), typapm(nparpm), typasn(nparsn)
    character(len=8) :: typase(nparse), typaf1(nparf1), typaf2(nparf2)
    character(len=8) :: typart(nparrt), typaf3(nparf3)
    character(len=16) :: nopaen(nparen), nopapm(nparpm), nopasn(nparsn)
    character(len=16) :: nopase(nparse), nopaf1(nparf1), nopaf2(nparf2)
    character(len=16) :: nopart(nparrt), nopaf3(nparf3)
!
    data nopaen / 'INTITULE', 'LIEU', 'SM', '3SM' /
    data typaen / 'K16',      'K8'  , 'R' , 'R'   /
    data nopart / 'TABL_PRES', 'SY', 'INST', 'SIGM_M_PRES',&
     &              'VALE_MAXI_LINE', 'VALE_MAXI_PARAB' /
    data typart / 'K8', 'R', 'R' , 'R'   , 'R'   , 'R'   /
    data nopapm / 'TABL_RESU', 'INST', 'PM', 'PB', 'PMB' /
    data typapm / 'K8'       , 'R'   , 'R' , 'R' , 'R'   /
    data nopasn / 'TABL_RESU_1', 'INST_1',&
     &              'TABL_RESU_2', 'INST_2', 'SN' /
    data typasn / 'K8', 'R' ,'K8', 'R', 'R'  /
    data nopase / 'SN*' /
    data typase / 'R'   /
    data nopaf1 / 'TABL_RESU_1', 'INST_1', 'NB_OCCUR_1',&
     &              'TABL_RESU_2', 'INST_2', 'NB_OCCUR_2',&
     &              'SN', 'SN*', 'SP', 'KE', 'SALT', 'NADM',&
     &              'DOMMAGE', 'DOMMAGE_CUMU' /
    data typaf1 / 'K8', 'R', 'I', 'K8', 'R', 'I',&
     &              'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R' /
    data nopaf2 / 'TABL_RESU_1', 'INST_1', 'NB_OCCUR_1',&
     &              'TABL_RESU_2', 'INST_2', 'NB_OCCUR_2',&
     &              'SN', 'SP', 'KE', 'SALT', 'NADM',&
     &              'DOMMAGE', 'DOMMAGE_CUMU' /
    data typaf2 / 'K8', 'R', 'I', 'K8', 'R', 'I',&
     &              'R', 'R', 'R', 'R', 'R', 'R', 'R' /
    data nopaf3 / 'TABL_RESU_1', 'INST_1', 'NB_OCCUR_1',&
     &            'TABL_RESU_2', 'INST_2', 'NB_OCCUR_2',&
     &            'SN', 'SN*', 'SP','SP_MECA','SP_THER','KE_MECA',&
     &            'KE_THER', 'SALT', 'NADM', 'DOMMAGE', 'DOMMAGE_CUMU' /
    data typaf3 / 'K8', 'R', 'I', 'K8', 'R', 'I','R', 'R',&
     &              'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R' /
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call getres(nomres, concep, nomcmd)
    call infniv(ifm, niv)
!
    call jeveuo(cresu, 'L', jresu)
    call jeveuo(cinst, 'L', jinst)
    call jelira(cinst, 'LONMAX', nbinst)
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
        if (lfatig) then
            if (kemixt) then
                do 14 i = 1, nparf3
                    nopara(npara+i) = nopaf3(i)
                    typara(npara+i) = typaf3(i)
14              continue
                npara = npara + nparf3
            else
                if (flexio) then
                    do 15 i = 1, nparf1
                        nopara(npara+i) = nopaf1(i)
                        typara(npara+i) = typaf1(i)
15                  continue
                    npara = npara + nparf1
                else
                    do 16 i = 1, nparf2
                        nopara(npara+i) = nopaf2(i)
                        typara(npara+i) = typaf2(i)
16                  continue
                    npara = npara + nparf2
                endif
            endif
        else
            if (lsn) then
                do 18 i = 1, nparsn
                    nopara(npara+i) = nopasn(i)
                    typara(npara+i) = typasn(i)
18              continue
                npara = npara + nparsn
                if (flexio) then
                    do 20 i = 1, nparse
                        nopara(npara+i) = nopase(i)
                        typara(npara+i) = typase(i)
20                  continue
                    npara = npara + nparse
                endif
            endif
        endif
!
        call tbcrsd(nomres, 'G')
        call tbajpa(nomres, npara, nopara, typara)
    endif
!
! --- LES LIGNES  LIEU ET SM
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
        do 404 i = 1, nparrt
            nopara(npara+i) = nopart(i)
404      continue
        npar1 = npara + nparrt
        vako(ik+1) = zk8(jresp-1+jt)
        vake(ik+1) = zk8(jresp-1+jt)
        ir = 2 + 1
        valo(ir) = symax
        vale(ir) = symax
        do 400 i = 1, nbinst
            ir = 3 + 1
            valo(ir) = zr(jinst+i-1)
            vale(ir) = zr(jinst+i-1)
            do 402 icmp = 1, ncmp
                l3 = 4*ncmp*nbinst + ncmp*(i-1) + icmp
                tpm(icmp) = zr(jsigm-1+l3)
402          continue
            call rctres(tpm, tresca)
            call rcmcrt(symax, tresca, stlin, stpar)
!
            ir = ir + 1
            valo(ir) = tresca
            vale(ir) = tresca
            ir = ir + 1
            valo(ir) = stlin
            vale(ir) = stlin
            ir = ir + 1
            valo(ir) = stpar
            vale(ir) = stpar
            call tbajli(nomres, npar1, nopara, vaio, valo,&
                        c16b, vako, 0)
            call tbajli(nomres, npar1, nopara, vaie, vale,&
                        c16b, vake, 0)
400      continue
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
        do 102 i = 1, nparpm
            nopara(npara+i) = nopapm(i)
102      continue
        npar1 = npara + nparpm
!
        call jeveuo(csigm, 'L', jsigm)
        do 110 i = 1, nbinst
            vako(ik+1) = zk8(jresu+i-1)
            ir = 2 + 1
            valo(ir) = zr(jinst+i-1)
            do 112 icmp = 1, ncmp
                l1 = ncmp*(i-1) + icmp
                l2 = ncmp*nbinst + ncmp*(i-1) + icmp
                l3 = 2*ncmp*nbinst + ncmp*(i-1) + icmp
                l4 = 3*ncmp*nbinst + ncmp*(i-1) + icmp
                tpm(icmp) = zr(jsigm-1+l1) - zr(jsigm-1+l3)
                tpb(icmp) = zr(jsigm-1+l2) - zr(jsigm-1+l4)
                tpmpbo(icmp) = zr(jsigm-1+l1) - zr(jsigm-1+l2) - (zr(jsigm-1+l3) - zr(jsigm-1+l4)&
                               &)
112          continue
            call rctres(tpm, tresca)
            ir = ir + 1
            valo(ir) = tresca
            call rctres(tpb, tresca)
            ir = ir + 1
            valo(ir) = tresca
            call rctres(tpmpbo, tresca)
            ir = ir + 1
            valo(ir) = tresca
            call tbajli(nomres, npar1, nopara, vaio, valo,&
                        c16b, vako, 0)
110      continue
        do 120 i = 1, nbinst
            vake(ik+1) = zk8(jresu+i-1)
            ir = 2 + 1
            vale(ir) = zr(jinst+i-1)
            do 122 icmp = 1, ncmp
                l1 = ncmp*(i-1) + icmp
                l2 = ncmp*nbinst + ncmp*(i-1) + icmp
                l3 = 2*ncmp*nbinst + ncmp*(i-1) + icmp
                l4 = 3*ncmp*nbinst + ncmp*(i-1) + icmp
                tpm(icmp) = zr(jsigm-1+l1) - zr(jsigm-1+l3)
                tpb(icmp) = zr(jsigm-1+l2) - zr(jsigm-1+l4)
                tpmpbe(icmp) = zr(jsigm-1+l1) + zr(jsigm-1+l2) - (zr(jsigm-1+l3) + zr(jsigm-1+l4)&
                               &)
122          continue
            call rctres(tpm, tresca)
            ir = ir + 1
            vale(ir) = tresca
            call rctres(tpb, tresca)
            ir = ir + 1
            vale(ir) = tresca
            call rctres(tpmpbe, tresca)
            ir = ir + 1
            vale(ir) = tresca
            call tbajli(nomres, npar1, nopara, vaie, vale,&
                        c16b, vake, 0)
120      continue
    endif
!
! --- POUR L'OPTION "SN"
!
    if (lsn .and. .not.lfatig) then
        do 202 i = 1, nparsn
            nopara(npara+i) = nopasn(i)
202      continue
        npar1 = npara + nparsn
        if (flexio) then
            npar1 = npar1 + 1
            nopara(npar1) = 'SN*'
        endif
!
        call jeveuo(csno, 'L', jsno)
        if (flexio) call jeveuo(csneo, 'L', jsneo)
!
        ind = 0
        do 210 i1 = 1, nbinst
            ind = ind + 1
            do 212 i2 = i1+1, nbinst
                ind = ind + 1
                vako(ik+1) = zk8(jresu+i1-1)
                vako(ik+2) = zk8(jresu+i2-1)
                ir = 2 + 1
                valo(ir) = zr(jinst+i1-1)
                ir = ir + 1
                valo(ir) = zr(jinst+i2-1)
                ir = ir + 1
                valo(ir) = zr(jsno+ind-1)
                if (flexio) then
                    ir = ir + 1
                    valo(ir) = zr(jsneo+ind-1)
                endif
                call tbajli(nomres, npar1, nopara, vaio, valo,&
                            c16b, vako, 0)
212          continue
210      continue
!
        call jeveuo(csne, 'L', jsne)
        if (flexio) call jeveuo(csnee, 'L', jsnee)
        ind = 0
        do 220 i1 = 1, nbinst
            ind = ind + 1
            do 222 i2 = i1+1, nbinst
                ind = ind + 1
                vake(ik+1) = zk8(jresu+i1-1)
                vake(ik+2) = zk8(jresu+i2-1)
                ir = 2 + 1
                vale(ir) = zr(jinst+i1-1)
                ir = ir + 1
                vale(ir) = zr(jinst+i2-1)
                ir = ir + 1
                vale(ir) = zr(jsne+ind-1)
                if (flexio) then
                    ir = ir + 1
                    vale(ir) = zr(jsnee+ind-1)
                endif
                call tbajli(nomres, npar1, nopara, vaie, vale,&
                            c16b, vake, 0)
222          continue
220      continue
    endif
!
! --- POUR L'OPTION "FATIGUE"
!
    if (lfatig) then
        if (kemixt) then
            do 301 i = 1, nparf3
                nopara(npara+i) = nopaf3(i)
301          continue
            npar1 = npara + nparf3 - 1
        else if (flexio) then
            do 302 i = 1, nparf1
                nopara(npara+i) = nopaf1(i)
302          continue
            npar1 = npara + nparf1 - 1
        else
            do 304 i = 1, nparf2
                nopara(npara+i) = nopaf2(i)
304          continue
            npar1 = npara + nparf2 - 1
        endif
!
        call jeveuo(cnoc, 'L', jnoc)
        call jeveuo(csno, 'L', jsno)
        if (flexio) call jeveuo(csneo, 'L', jsneo)
        call jeveuo(cspo, 'L', jspo)
        call jeveuo(cfao, 'L', jfao)
        if (kemixt) then
            call jeveuo(cspmo, 'L', jspmo)
            call jeveuo(cspto, 'L', jspto)
        endif
        ind = 0
        do 310 i1 = 1, nbinst
            ind = ind + 1
            do 312 i2 = i1+1, nbinst
                ind = ind + 1
                vako(ik+1) = zk8(jresu+i1-1)
                vako(ik+2) = zk8(jresu+i2-1)
                vaio(1) = zi(jnoc+i1-1)
                vaio(2) = zi(jnoc+i2-1)
                ir = 2 + 1
                valo(ir) = zr(jinst+i1-1)
                ir = ir + 1
                valo(ir) = zr(jinst+i2-1)
                ir = ir + 1
                valo(ir) = zr(jsno+ind-1)
                if (flexio) then
                    ir = ir + 1
                    valo(ir) = zr(jsneo+ind-1)
                endif
                ir = ir + 1
                valo(ir) = zr(jspo-1+ind)
                if (kemixt) then
                    ir = ir + 1
                    valo(ir) = zr(jspmo-1+ind)
                    ir = ir + 1
                    valo(ir) = zr(jspto-1+ind)
                endif
                ir = ir + 1
                valo(ir) = zr(jfao-1+5*(ind-1)+1)
                if (kemixt) then
                    ir = ir + 1
                    valo(ir) =zr(jfao-1+5*(ind-1)+5)
                endif
                ir = ir + 1
                valo(ir) = zr(jfao-1+5*(ind-1)+2)
                ir = ir + 1
                valo(ir) = zr(jfao-1+5*(ind-1)+3)
                ir = ir + 1
                valo(ir) = zr(jfao-1+5*(ind-1)+4)
                call tbajli(nomres, npar1, nopara, vaio, valo,&
                            c16b, vako, 0)
312          continue
310      continue
!
        call jeveuo(csne, 'L', jsne)
        if (flexio) call jeveuo(csnee, 'L', jsnee)
        call jeveuo(cspe, 'L', jspe)
        call jeveuo(cfae, 'L', jfae)
        if (kemixt) then
            call jeveuo(cspme, 'L', jspme)
            call jeveuo(cspte, 'L', jspte)
        endif
        ind = 0
        do 320 i1 = 1, nbinst
            ind = ind + 1
            do 322 i2 = i1+1, nbinst
                ind = ind + 1
                vake(ik+1) = zk8(jresu+i1-1)
                vake(ik+2) = zk8(jresu+i2-1)
                vaie(1) = zi(jnoc+i1-1)
                vaie(2) = zi(jnoc+i2-1)
                ir = 2 + 1
                vale(ir) = zr(jinst+i1-1)
                ir = ir + 1
                vale(ir) = zr(jinst+i2-1)
                ir = ir + 1
                vale(ir) = zr(jsne+ind-1)
                if (flexio) then
                    ir = ir + 1
                    vale(ir) = zr(jsnee+ind-1)
                endif
                ir = ir + 1
                vale(ir) = zr(jspe-1+ind)
                if (kemixt) then
                    ir = ir + 1
                    vale(ir) = zr(jspme-1+ind)
                    ir = ir + 1
                    vale(ir) = zr(jspte-1+ind)
                endif
                ir = ir + 1
                vale(ir) = zr(jfae-1+5*(ind-1)+1)
                if (kemixt) then
                    ir = ir + 1
                    vale(ir) =zr(jfae-1+5*(ind-1)+5)
                endif
                ir = ir + 1
                vale(ir) = zr(jfae-1+5*(ind-1)+2)
                ir = ir + 1
                vale(ir) = zr(jfae-1+5*(ind-1)+3)
                ir = ir + 1
                vale(ir) = zr(jfae-1+5*(ind-1)+4)
                call tbajli(nomres, npar1, nopara, vaie, vale,&
                            c16b, vake, 0)
322          continue
320      continue
!
        nopara(npara+1) = 'DOMMAGE_CUMU'
        npar1 = npara + 1
        if (niv .eq. 2) write(6,*) '******* ORIGINE DU SEGMENT *******'
        call rcevfu(cnoc, cfao, dco)
        if (niv .eq. 2) write(6,*) '******* EXTREMITE DU SEGMENT *******'
        call rcevfu(cnoc, cfae, dce)
        valo(3) = dco
        vale(3) = dce
        call tbajli(nomres, npar1, nopara, vaio, valo,&
                    c16b, vako, 0)
        call tbajli(nomres, npar1, nopara, vaie, vale,&
                    c16b, vake, 0)
!
    endif
!
    call jedema()
end subroutine
