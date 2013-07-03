subroutine rvcalq(iocc, sdeval, vec1, vec2, repere,&
                  nomcp, nbcpnc, nbcpcd, option, quant,&
                  sdlieu, codir, valdir, sdcalq, courbe)
! aslint: disable=W1501
    implicit   none
!
#include "jeveux.h"
!
#include "asterc/getvr8.h"
#include "asterc/r8vide.h"
#include "asterfort/infniv.h"
#include "asterfort/jacobi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/numek8.h"
#include "asterfort/rvinvt.h"
#include "asterfort/rvpstd.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=24) :: sdeval, quant, sdlieu
    character(len=19) :: sdcalq
    character(len=16) :: option
    character(len=8) :: nomcp(*), repere, courbe
    integer :: nbcpnc, nbcpcd, codir
    real(kind=8) :: valdir(*), vec1(*), vec2(*)
    real(kind=8) :: vtv, vax, vay, vaz, vbx, vby, vbz
    logical :: tridim
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     CALCUL DE LA QUANTITE POST-TRAITEE
!     ------------------------------------------------------------------
! IN  IOCC   : I : NUMERO DE L' OCCURENCE TRAITEE
! IN  SDEVAL : K : SS_CHP_GD DE L' EVALUATION DES CMP NECESSAIRE
! IN  VEC1   : R : VECTEUR 1 DU NOUVEAU REPERE (TANGEANTE)
! IN  VEC2   : R : VECTEUR 2 DU NOUVEAU REPERE (NORMALE)
! IN  SDLIEU : K : NOM DE LA SD LIEU DU LIEU TRAITE
! IN  REPERE : K : NOM DU REPERE DE POST_TRAITEMENT
! IN  NOMCP  : K : TABLEAU DES NOMS DE CMP CANDIDATES
! IN  OPTION : K : NOM DE L' OPTION DEMANDEE
! IN  NBCPCD : I : NOMBRE DE CMP CANDIDATE
! IN  NBCPNC : I : NOMBRE DE CMP NECESSAIRE
! IN  QUANT  : K : NOM DE LA QUANTITE A POST-TRAITER
! IN  CODIR  : I : CODE DES DIRECTIONS ACTIVES (TRACE DIRECTION)
! IN  VALDIR : R : VALEUR DES DIRECTIONS ACTIVES (TRACE DIRECTION)
! OUT SDCALQ : K : NOM DE LA SD DE CALCUL DE LA QUANTITE
!                    .VALE : S V R8 <-- VALEUR DES CMP DE LA QUANTITE
!                                       DOCU = 'CHNO' OU 'CHLM'
!                    .PADR : S V I  <-- TABLE D' INDIRECTION SUR .VALE
!
!                    .PNBN : S V I  <-- TABLE NB_NOEUD PAR MAILLES
!                                       N' EXISTE QUE POUR 'CHLM'
!                    .PNCO : S V I  <-- TABLE DES NB_COUCHE PAR MAILLES
!                                       N' EXISTE QUE POUR 'CHLM'
!                    .PNSP : S V I  <-- TABLE DES NB_SS-PT PAR MAILLES
!                                       N' EXISTE QUE POUR 'CHLM'
!     ------------------------------------------------------------------
!
!
!
    character(len=80) :: kbid
    character(len=24) :: nvalee, npadre, npnbne, nvaleq, npadrq, npnbnq, npcmpe
    character(len=24) :: nnocpq, npncoe, npnspe
    character(len=4) :: docu, docul, k4
    character(len=2) :: tq
    character(len=1) :: k1
!
    integer :: nbpt, lpadr, lpnbn, apnbnq, avaleq, apadrq, ibid, anocpq, apncoe
    integer :: apnbne, avalee, apadre, idecq, idece, nvp, n, nboc
    integer :: adre, adrq, ll, i, j, k, ioc, apcmpe, anumcp, iadr, acpgd, apnspe
    integer :: av1x, av2x, av1y, av2y, ipadr, apnspq
    integer :: kd1, kd2, kd3, kd4, kd5, kd6, idec, ny, il
    integer :: l, lnq, lne, m, mder, nbadrq, nbnd, nc, num, pt, apncoq
    integer :: nperm, itype, iordre, ifm, niv
    integer :: ico, icoef, ind, indice, iocc, isp, nbco, nbsp, nitjac
!
    real(kind=8) :: vp1, vp2, tr, det, a, b, c, txx, txy, tyy, txz, tyz, sxx
    real(kind=8) :: sxy, syy
    real(kind=8) :: tx, ty, tz, tzz, val, vm, aux, zero, vecty(3)
    real(kind=8) :: v1x, v2x, v3x, v1y, v2y, v3y, v1z, v2z, v3z, v1n, v2n, v2p
    real(kind=8) :: ar(6), br(6), vecpro(3, 3), valpro(3), v2(2)
    real(kind=8) :: jacaux(3), tol, toldyn
    integer :: tcoe, tcoq, tnde, tndq
!
    integer :: asgtu
    real(kind=8) :: sgtu
    integer :: iarg
    sgtu(i) = zr(asgtu+i-1)
!     FORME BILINEAIRE ASSOCIEE AU TENSEUR
    vtv(vax,vay,vaz,vbx,vby,vbz) = vax* (vbx*txx+vby*txy+vbz*txz) + vay* (vbx*txy+vby*tyy+vbz*tyz&
                                   &) + vaz* (vbx*txz+vby*tyz+vbz*tzz)
!
!======================================================================
!
    call jemarq()
    zero = 0.0d0
    nvp = 3
    if (courbe .eq. ' ') then
        tridim = .false.
    else
        call getvr8('ACTION', 'VECT_Y', iocc, iarg, 3,&
                    vecty, ny)
        tridim = ny .ne. 0
    endif
    call infniv(ifm, niv)
!
    nvalee = sdeval(1:19)//'.VALE'
    npadre = sdeval(1:19)//'.PADR'
    npnbne = sdeval(1:19)//'.PNBN'
    npcmpe = sdeval(1:19)//'.PCMP'
    npncoe = sdeval(1:19)//'.PNCO'
    npnspe = sdeval(1:19)//'.PNSP'
    nnocpq = sdcalq(1:19)//'.NOCP'
    nvaleq = sdcalq(1:19)//'.VALE'
    npadrq = sdcalq(1:19)//'.PADR'
    npnbnq = sdcalq(1:19)//'.PNBN'
!
    call jelira(sdlieu(1:19)//'.REFE', 'DOCU', ibid, docul)
    call jelira(nvalee, 'DOCU', ibid, docu)
    call jelira(npadre, 'LONMAX', lpadr, kbid)
    call jeveuo(nvalee, 'L', avalee)
    call jeveuo(npadre, 'L', apadre)
!
    k4 = option(1:4)
    k1 = quant(1:1)
    if ((k4.eq.'SIGM') .or. (k4.eq.'EPSI') .or. (k4.eq.'SIEF')) then
        tq = 'T3'
    else if (k4.eq.'EFGE') then
        tq = 'T2'
        else if ((k4.eq.'FLUX') .or. (k4.eq.'DEPL') .or. (k4.eq.'FORC'))&
    then
        tq = 'V3'
    else
        tq = 'AS'
    endif
!
    if (k1 .eq. 'I') then
        lne = 6
        if (tq .eq. 'T3') then
            lnq = 4
        else
            lnq = 8
        endif
        call wkvect(nnocpq, 'V V K8', 4, anocpq)
        zk8(anocpq+1-1) = 'VMIS'
        zk8(anocpq+2-1) = 'TRESCA'
        zk8(anocpq+3-1) = 'TRACE'
        zk8(anocpq+4-1) = 'DETER'
!
    else if (k1.eq.'E') then
        lne = 6
        if (tq .eq. 'T3') then
            lnq = 12
            call wkvect(nnocpq, 'V V K8', lnq, anocpq)
            zk8(anocpq+1-1) = 'PRIN_1'
            zk8(anocpq+2-1) = 'PRIN_2'
            zk8(anocpq+3-1) = 'PRIN_3'
            zk8(anocpq+4-1) = 'VECT_1_X'
            zk8(anocpq+5-1) = 'VECT_1_Y'
            zk8(anocpq+6-1) = 'VECT_1_Z'
            zk8(anocpq+7-1) = 'VECT_2_X'
            zk8(anocpq+8-1) = 'VECT_2_Y'
            zk8(anocpq+9-1) = 'VECT_2_Z'
            zk8(anocpq+10-1) = 'VECT_3_X'
            zk8(anocpq+11-1) = 'VECT_3_Y'
            zk8(anocpq+12-1) = 'VECT_3_Z'
        else
            lnq = 8
            call wkvect(nnocpq, 'V V K8', lnq, anocpq)
            zk8(anocpq+1-1) = 'PRIN_1'
            zk8(anocpq+2-1) = 'PRIN_2'
            zk8(anocpq+3-1) = 'VECT_1_X'
            zk8(anocpq+4-1) = 'VECT_1_Y'
            zk8(anocpq+5-1) = 'PRIN_1'
            zk8(anocpq+6-1) = 'PRIN_2'
            zk8(anocpq+7-1) = 'VECT_2_X'
            zk8(anocpq+8-1) = 'VECT_2_Y'
        endif
!
    else if (quant(1:7).eq.'TRACE_D') then
        if (tq .eq. 'T3') then
            lnq = 3
            call wkvect(nnocpq, 'V V K8', 3, anocpq)
            zk8(anocpq+1-1) = 'TR_DIR_1'
            zk8(anocpq+2-1) = 'TR_DIR_2'
            zk8(anocpq+3-1) = 'TR_DIR_3'
            if (codir .le. 3) then
                lne = 3
            else if (codir.le.6) then
                lne = 5
            else
                lne = 6
            endif
        else if (tq.eq.'T2') then
            lnq = 4
            call wkvect(nnocpq, 'V V K8', 4, anocpq)
            zk8(anocpq+1-1) = 'TR_DIR_1'
            zk8(anocpq+2-1) = 'TR_DIR_2'
            zk8(anocpq+3-1) = 'TR_DIR_1'
            zk8(anocpq+4-1) = 'TR_DIR_2'
            if (codir .le. 3) then
                lne = 4
            else
                lne = 6
            endif
        else
            lnq = 1
            call wkvect(nnocpq, 'V V K8', 1, anocpq)
            zk8(anocpq+1-1) = 'TRAC_DIR'
            if (codir .le. 3) then
                lne = 1
            else if (codir.le.6) then
                lne = 2
            else
                lne = 3
            endif
        endif
!
    else if (quant(1:7).eq.'TRACE_N') then
        if (tq .eq. 'T3') then
            lne = 5
            lnq = 3
            call wkvect(nnocpq, 'V V K8', 3, anocpq)
            zk8(anocpq+1-1) = 'TR_NOR_1'
            zk8(anocpq+2-1) = 'TR_NOR_2'
            zk8(anocpq+3-1) = 'TR_NOR_3'
        else if (tq.eq.'T2') then
            lne = 6
            lnq = 4
            call wkvect(nnocpq, 'V V K8', 4, anocpq)
            zk8(anocpq+1-1) = 'TR_NOR_1'
            zk8(anocpq+2-1) = 'TR_NOR_2'
            zk8(anocpq+3-1) = 'TR_NOR_1'
            zk8(anocpq+4-1) = 'TR_NOR_2'
        else
            lne = 3
            lnq = 1
            call wkvect(nnocpq, 'V V K8', 1, anocpq)
            zk8(anocpq+1-1) = 'TRAC_NOR'
        endif
    else
        lne = nbcpnc
        lnq = nbcpcd
    endif
!
    nbpt = 0
    icoef = 1
    if (docu .eq. 'CHNO') then
        nbpt = lpadr
        lpnbn = lpadr
    else
        call jelira(npnbne, 'LONMAX', lpnbn, kbid)
        call jeveuo(npnbne, 'L', apnbne)
        do 10,i = 1,lpnbn,1
        nbpt = nbpt + zi(apnbne+i-1)
10      continue
        call jeveuo(npncoe, 'L', apncoe)
        call jeveuo(npnspe, 'L', apnspe)
        icoef = zi(apncoe+1-1)*zi(apnspe+1-1)
    endif
    call wkvect(nvaleq, 'V V R', lnq*nbpt*icoef, avaleq)
    call wkvect(npadrq, 'V V I', lpadr, apadrq)
    call wkvect(npnbnq, 'V V I', lpnbn, apnbnq)
    call wkvect(sdcalq(1:19)//'.PNCO', 'V V I', lpnbn, apncoq)
    call wkvect(sdcalq(1:19)//'.PNSP', 'V V I', lpnbn, apnspq)
    if (docu .eq. 'CHNO') then
        call wkvect('&&RVCALQ.CHNO.PNSP', 'V V I', lpadr, apnspe)
        call wkvect('&&RVCALQ.CHNO.PNCO', 'V V I', lpadr, apncoe)
        call wkvect('&&RVCALQ.CHNO.PNBN', 'V V I', lpadr, apnbne)
        do 20,i = 1,lpadr,1
        zi(apadrq+i-1) = 1 + (i-1)*lnq
        zi(apnbnq+i-1) = 1
        zi(apncoq+i-1) = 1
        zi(apnspq+i-1) = 1
        zi(apnbne+i-1) = 1
        zi(apncoe+i-1) = 1
        zi(apnspe+i-1) = 1
20      continue
    else
        zi(apadrq+1-1) = 1
        do 30,i = 1,lpnbn - 1,1
        nbco = zi(apncoe+i-1)
        nbsp = zi(apnspe+i-1)
        nbnd = zi(apnbne+i-1)
        zi(apnbnq+i-1) = nbnd
        zi(apadrq+i) = zi(apadrq+i-1) + lnq*nbnd*nbsp*nbco
        zi(apncoq+i-1) = nbco
        zi(apnspq+i-1) = nbsp
30      continue
        zi(apnbnq+lpnbn-1) = zi(apnbne+lpnbn-1)
        zi(apncoq+lpnbn-1) = zi(apncoe+lpnbn-1)
        zi(apnspq+lpnbn-1) = zi(apnspe+lpnbn-1)
    endif
!
    k1 = quant(1:1)
    if (k1 .eq. 'I') then
        if (tq .eq. 'T3') then
            do 70,ipadr = 1,lpadr,1
            idece = zi(apadre+ipadr-1)
            idecq = zi(apadrq+ipadr-1)
            nbsp = zi(apnspe+ipadr-1)
            nbco = zi(apncoe+ipadr-1)
            nbnd = zi(apnbne+ipadr-1)
            tnde = 6*nbsp
            tndq = 4*nbsp
            tcoq = tndq*nbnd
            tcoe = tnde*nbnd
            do 60,ico = 1,nbco,1
            adre = (ico-1)*tcoe
            adrq = (ico-1)*tcoq
            do 50,ind = 1,nbnd,1
            do 40,isp = 1,nbsp,1
            call rvinvt(zr(avalee+idece-1+adre+ (isp- 1)*6), zr(avaleq+idecq-1+adrq+ (isp-1)*4),&
                        zr(avaleq+idecq-1+adrq+ (isp-1)*4+1),&
                        zr(avaleq+idecq-1+adrq+ (isp-1)*4+2),&
                        zr(avaleq+idecq-1+adrq+ (isp-1)*4+3))
40          continue
            adre = adre + tnde
            adrq = adrq + tndq
50          continue
60          continue
70          continue
        else
            do 110,ipadr = 1,lpadr,1
            idece = zi(apadre+ipadr-1)
            idecq = zi(apadrq+ipadr-1)
            nbsp = zi(apnspe+ipadr-1)
            nbco = zi(apncoe+ipadr-1)
            nbnd = zi(apnbne+ipadr-1)
            tnde = lne*nbsp
            tndq = lnq*nbsp
            tcoq = tndq*nbnd
            tcoe = tnde*nbnd
            do 100,ico = 1,nbco,1
            adre = (ico-1)*tcoe
            adrq = (ico-1)*tcoq
            do 90,ind = 1,zi(apnbnq+ipadr-1),1
            do 80,isp = 1,nbsp,1
            if (zr(avalee+idece-1+adre+(isp-1)*6) .eq. r8vide()) then
                a = 0.d0
            else
                a = zr(avalee+idece-1+adre+ (isp-1)*6)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+1) .eq. r8vide()) then
                b = 0.d0
            else
                b = zr(avalee+idece-1+adre+ (isp-1)*6+ 1)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+2) .eq. r8vide()) then
                c = 0.d0
            else
                c = zr(avalee+idece-1+adre+ (isp-1)*6+ 2)
            endif
            tr = a + b
            det = a*b - c*c
            tx = max(abs(a),abs(b),abs(c))
            if (tx .eq. zero) then
                vp1 = zero
                vp2 = zero
                vm = zero
            else
                tx = 0.5d0* (a-b)
                if (abs(tx) .gt. abs(c)) then
                    aux = c/tx
                    aux = tx*sqrt(1.0d0+aux*aux)
                else
                    aux = tx/c
                    aux = c*sqrt(1.0d0+aux*aux)
                endif
                vp1 = 0.5d0*tr + aux
                vp2 = 0.5d0*tr - aux
                a = a - tr/3.0d0
                b = b - tr/3.0d0
                c = sqrt(2.0d0)*c
                if (abs(a) .gt. max(abs(b),abs(c))) then
                    aux = abs(a)
                    tx = b/a
                    ty = c/a
                    else if (abs(b).gt.max(abs(a),abs(c)))&
                                then
                    aux = abs(b)
                    tx = a/b
                    ty = c/b
                else
                    aux = abs(c)
                    tx = a/c
                    ty = b/c
                endif
                vm = sqrt((1.0d0+tx*tx+ty*ty)*3.0d0/ 2.0d0)
                vm = aux*vm
            endif
            c = abs(vp1-vp2)
            zr(avaleq+idecq-1+adrq+ (isp-1)*8) = vm
            zr(avaleq+idecq-1+adrq+ (isp-1)*8+1) = c
            zr(avaleq+idecq-1+adrq+ (isp-1)*8+2) = tr
            zr(avaleq+idecq-1+adrq+ (isp-1)*8+3) =&
                            det
            if (zr(avalee+idece-1+adre+(isp-1)*6+3) .eq. r8vide()) then
                a = 0.d0
            else
                a = zr(avalee+idece-1+adre+ (isp-1)*6+ 3)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+4) .eq. r8vide()) then
                b = 0.d0
            else
                b = zr(avalee+idece-1+adre+ (isp-1)*6+ 4)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+5) .eq. r8vide()) then
                c = 0.d0
            else
                c = zr(avalee+idece-1+adre+ (isp-1)*6+ 5)
            endif
            tr = a + b
            det = a*b - c*c
            tx = max(abs(a),abs(b),abs(c))
            if (tx .eq. zero) then
                vp1 = zero
                vp2 = zero
                vm = zero
            else
                tx = 0.5d0* (a-b)
                if (abs(tx) .gt. abs(c)) then
                    aux = c/tx
                    aux = tx*sqrt(1.0d0+aux*aux)
                else
                    aux = tx/c
                    aux = c*sqrt(1.0d0+aux*aux)
                endif
                vp1 = 0.5d0*tr + aux
                vp2 = 0.5d0*tr - aux
                a = a - tr/3.0d0
                b = b - tr/3.0d0
                c = sqrt(2.0d0)*c
                if (abs(a) .gt. max(abs(b),abs(c))) then
                    aux = abs(a)
                    tx = b/a
                    ty = c/a
                    else if (abs(b).gt.max(abs(a),abs(c)))&
                                then
                    aux = abs(b)
                    tx = a/b
                    ty = c/b
                else
                    aux = abs(c)
                    tx = a/c
                    ty = b/c
                endif
                vm = sqrt((1.0d0+tx*tx+ty*ty)*3.0d0/ 2.0d0)
                vm = aux*vm
            endif
            c = abs(vp1-vp2)
            zr(avaleq+idecq-1+adrq+ (isp-1)*8+4) = vm
            zr(avaleq+idecq-1+adrq+ (isp-1)*8+5) = c
            zr(avaleq+idecq-1+adrq+ (isp-1)*8+6) = tr
            zr(avaleq+idecq-1+adrq+ (isp-1)*8+7) =&
                            det
80          continue
            adre = adre + tnde
            adrq = adrq + tndq
90          continue
100          continue
110          continue
        endif
    else if (k1.eq.'E') then
        if (tq .eq. 'T3') then
            nperm = 12
            tol = 1.d-10
            toldyn = 1.d-2
            itype = 0
            iordre = 0
            do 150,ipadr = 1,lpadr,1
            idece = zi(apadre+ipadr-1)
            idecq = zi(apadrq+ipadr-1)
            nbsp = zi(apnspe+ipadr-1)
            nbco = zi(apncoe+ipadr-1)
            nbnd = zi(apnbne+ipadr-1)
            tnde = lne*nbsp
            tndq = lnq*nbsp
            tcoq = tndq*nbnd
            tcoe = tnde*nbnd
            do 140,ico = 1,nbco,1
            adre = (ico-1)*tcoe
            adrq = (ico-1)*tcoq
            do 130,ind = 1,zi(apnbnq+ipadr-1),1
            do 120,isp = 1,nbsp,1
            if (zr(avalee+idece-1+adre+(isp-1)*6) .eq. r8vide()) then
                ar(1) = 0.d0
            else
                ar(1) = zr(avalee+idece-1+adre+(isp-1) *6)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+3) .eq. r8vide()) then
                ar(2) = 0.d0
            else
                ar(2) = zr(avalee+idece-1+adre+(isp-1) *6+3)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+4) .eq. r8vide()) then
                ar(3) = 0.d0
            else
                ar(3) = zr(avalee+idece-1+adre+(isp-1) *6+4)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+1) .eq. r8vide()) then
                ar(4) = 0.d0
            else
                ar(4) = zr(avalee+idece-1+adre+(isp-1) *6+1)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+5) .eq. r8vide()) then
                ar(5) = 0.d0
            else
                ar(5) = zr(avalee+idece-1+adre+(isp-1) *6+5)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+2) .eq. r8vide()) then
                ar(6) = 0.d0
            else
                ar(6) = zr(avalee+idece-1+adre+(isp-1) *6+2)
            endif
            br(1) = 1.d0
            br(2) = 0.d0
            br(3) = 0.d0
            br(4) = 1.d0
            br(5) = 0.d0
            br(6) = 1.d0
            call jacobi(nvp, nperm, tol, toldyn, ar,&
                        br, vecpro, valpro, jacaux, nitjac,&
                        itype, iordre)
            indice = avaleq + idecq - 1 + adrq + (isp- 1)*lnq
            zr(indice) = valpro(1)
            zr(indice+1) = valpro(2)
            zr(indice+2) = valpro(3)
            zr(indice+3) = vecpro(1,1)
            zr(indice+4) = vecpro(2,1)
            zr(indice+5) = vecpro(3,1)
            zr(indice+6) = vecpro(1,2)
            zr(indice+7) = vecpro(2,2)
            zr(indice+8) = vecpro(3,2)
            zr(indice+9) = vecpro(1,3)
            zr(indice+10) = vecpro(2,3)
            zr(indice+11) = vecpro(3,3)
120          continue
            adre = adre + tnde
            adrq = adrq + tndq
130          continue
140          continue
150          continue
        else
            do 190,ipadr = 1,lpadr,1
            idece = zi(apadre+ipadr-1)
            idecq = zi(apadrq+ipadr-1)
            nbsp = zi(apnspe+ipadr-1)
            nbco = zi(apncoe+ipadr-1)
            nbnd = zi(apnbne+ipadr-1)
            tnde = lne*nbsp
            tndq = lnq*nbsp
            tcoq = tndq*nbnd
            tcoe = tnde*nbnd
            do 180,ico = 1,nbco,1
            adre = (ico-1)*tcoe
            adrq = (ico-1)*tcoq
            do 170,ind = 1,zi(apnbnq+ipadr-1),1
            do 160,isp = 1,nbsp
            if (zr(avalee+idece-1+adre+(isp-1)*6) .eq. r8vide()) then
                a = 0.d0
            else
                a = zr(avalee+idece-1+adre+(isp-1)*6)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+1) .eq. r8vide()) then
                b = 0.d0
            else
                b = zr(avalee+idece-1+adre+(isp-1)*6+ 1)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+2) .eq. r8vide()) then
                c = 0.d0
            else
                c = zr(avalee+idece-1+adre+(isp-1)*6+ 2)
            endif
            tr = max(abs(a),abs(b),abs(c))
            if (tr .eq. zero) then
                vp1 = zero
                vp2 = zero
            else
                tr = 0.5d0* (a-b)
                if (abs(tr) .gt. abs(c)) then
                    aux = c/tr
                    aux = tr*sqrt(1.0d0+aux*aux)
                else
                    aux = tr/c
                    aux = c*sqrt(1.0d0+aux*aux)
                endif
                vp1 = 0.5d0* (a+b) + aux
                vp2 = 0.5d0* (a+b) - aux
            endif
            indice = avaleq + idecq - 1 + adrq + (isp- 1)*lnq
            zr(indice) = vp1
            zr(indice+1) = vp2
            zr(indice+2) = 1.d0
            zr(indice+3) = 0.d0
            if (zr(avalee+idece-1+adre+(isp-1)*6+3) .eq. r8vide()) then
                a = 0.d0
            else
                a = zr(avalee+idece-1+adre+(isp-1)*6+ 3)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+4) .eq. r8vide()) then
                b = 0.d0
            else
                b = zr(avalee+idece-1+adre+(isp-1)*6+ 4)
            endif
            if (zr(avalee+idece-1+adre+(isp-1)*6+5) .eq. r8vide()) then
                c = 0.d0
            else
                c = zr(avalee+idece-1+adre+(isp-1)*6+ 5)
            endif
            tr = max(abs(a),abs(b),abs(c))
            if (tr .eq. zero) then
                vp1 = zero
                vp2 = zero
            else
                tr = 0.5d0* (a-b)
                if (abs(tr) .gt. abs(c)) then
                    aux = c/tr
                    aux = tr*sqrt(1.0d0+aux*aux)
                else
                    aux = tr/c
                    aux = c*sqrt(1.0d0+aux*aux)
                endif
                vp1 = 0.5d0* (a+b) + aux
                vp2 = 0.5d0* (a+b) - aux
            endif
            zr(indice+4) = vp1
            zr(indice+5) = vp2
            zr(indice+6) = 0.d0
            zr(indice+7) = 1.d0
160          continue
            adre = adre + tnde
            adrq = adrq + tndq
170          continue
180          continue
190          continue
        endif
    else if (quant(1:7).eq.'TRACE_D') then
        if (docu .eq. 'CHLM') then
            do 230,ipadr = 1,lpadr,1
            idece = zi(apadre+ipadr-1)
            idecq = zi(apadrq+ipadr-1)
            nbsp = zi(apnspe+ipadr-1)
            nbco = zi(apncoe+ipadr-1)
            nbnd = zi(apnbne+ipadr-1)
            tnde = lne*nbsp
            tndq = lnq*nbsp
            tcoq = tndq*nbnd
            tcoe = tnde*nbnd
            do 220,ico = 1,nbco,1
            adre = (ico-1)*tcoe
            adrq = (ico-1)*tcoq
            do 210,ind = 1,zi(apnbnq+ipadr-1),1
            do 200,isp = 1,nbsp,1
            call rvpstd(zr(avalee+idece-1+adre+ (isp- 1)*lne), tq, codir, valdir,&
                        zr(avaleq+idecq- 1+adrq+ (isp- 1)*lnq))
200          continue
            adre = adre + tnde
            adrq = adrq + tndq
210          continue
220          continue
230          continue
        else
            do 240,ipadr = 1,lpadr,1
            idece = zi(apadre+ipadr-1)
            idecq = zi(apadrq+ipadr-1)
            nbnd = zi(apnbne+ipadr-1)
            call rvpstd(zr(avalee+idece-1), tq, codir, valdir, zr(avaleq+idecq-1))
240          continue
        endif
!
    else if (quant(1:7).eq.'TRACE_N') then
        call jelira(sdlieu(1:19)//'.ABSC', 'NMAXOC', nboc, kbid)
        if (docu .eq. 'CHNO') then
            call jelira(npadrq, 'LONMAX', l, kbid)
            do 250,n = 1,l,1
            idece = zi(apadre+n-1)
            idecq = zi(apadrq+n-1)
            v2(1) = vec2(2*(n-1)+1)
            v2(2) = vec2(2*(n-1)+2)
            call rvpstd(zr(avalee+idece-1), tq, 4, v2, zr(avaleq+ idecq-1))
250          continue
        else
            if (docul .eq. 'LSTN') then
                call jelira(jexnum(sdlieu(1:19)//'.ABSC', 1), 'LONMAX', l, kbid)
                do 290,m = 1,l,1
                idece = zi(apadre+m-1)
                idecq = zi(apadrq+m-1)
                nbsp = zi(apnspe+m-1)
                nbco = zi(apncoe+m-1)
                nbnd = zi(apnbne+m-1)
                tnde = lne*nbsp
                tndq = lnq*nbsp
                tcoq = tndq*nbnd
                tcoe = tnde*nbnd
                v2(1) = vec2(2*(m-1)+1)
                v2(2) = vec2(2*(m-1)+2)
                do 280,ico = 1,nbco,1
                adre = (ico-1)*tcoe
                adrq = (ico-1)*tcoq
                do 270,ind = 1,zi(apnbnq+m-1),1
                do 260,isp = 1,nbsp,1
                call rvpstd(zr(avalee+idece-1+adre+( isp-1)*lne), tq, 4, v2,&
                            zr(avaleq+idecq- 1+adrq+(isp-1)*lnq))
260              continue
                adre = adre + tnde
                adrq = adrq + tndq
270              continue
280              continue
290              continue
            else
                mder = 0
                do 330,ioc = 1,nboc,1
                call jelira(jexnum(sdlieu(1:19)//'.ABSC', ioc), 'LONMAX', l, kbid)
                do 320,m = mder + 1,mder + l - 1,1
                n = m + ioc - 1
                idece = zi(apadre+m-1)
                idecq = zi(apadrq+m-1)
                nbsp = zi(apnspe+m-1)
                nbco = zi(apncoe+m-1)
                nbnd = zi(apnbne+m-1)
                tnde = lne*nbsp
                tndq = lnq*nbsp
                tcoq = tndq*nbnd
                tcoe = tnde*nbnd
                v2(1) = vec2(2*(m-1)+1)
                v2(2) = vec2(2*(m-1)+2)
                do 310,ico = 1,nbco,1
                adre = (ico-1)*tcoe
                adrq = (ico-1)*tcoq
                do 300,isp = 1,nbsp,1
                call rvpstd(zr(avalee+idece-1+adre+( isp-1)*lne), tq, 4, v2,&
                            zr(avaleq+idecq- 1+adrq+(isp-1)*lnq))
                call rvpstd(zr(avalee+idece-1+adre+( isp-1+nbsp)*lne), tq, 4, v2,&
                            zr(avaleq+ idecq-1+adrq+(isp-1+nbsp)*lnq))
300              continue
310              continue
320              continue
                mder = mder + l - 1
330              continue
            endif
        endif
    else if (repere(1:1).eq.'L' .and. tridim) then
!               -------------------------------
        call wkvect(nnocpq, 'V V K8', nbcpcd, anocpq)
        call jeveuo(courbe//'S1   '//'.DESC', 'L', asgtu)
        do 340,i = 1,nbcpcd,1
        zk8(anocpq+i-1) = nomcp(i)
340      continue
        call jelira(jexnum(sdlieu(1:19)//'.ABSC', 1), 'LONMAX', l, kbid)
        call jelira(npadrq, 'LONMAX', nbadrq, kbid)
        call jelira(sdlieu(1:19)//'.ABSC', 'NMAXOC', nboc, kbid)
        call jeveuo(npcmpe, 'L', apcmpe)
        call jeveuo(sdeval(1:19)//'.NUGD', 'L', iadr)
        call jeveuo(jexnum('&CATA.GD.NOMCMP', zi(iadr)), 'L', acpgd)
        call jelira(jexnum('&CATA.GD.NOMCMP', zi(iadr)), 'LONMAX', nc, kbid)
        call wkvect('&&RVCALQ.NUM.CP.CD', 'V V I', nbcpcd, anumcp)
        call numek8(zk8(acpgd), nomcp, nc, nbcpcd, zi(anumcp))
        ioc = 1
        do 400,i = 1,nbadrq,1
        adre = zi(apadre+i-1)
        adrq = zi(apadrq+i-1)
        nbsp = zi(apnspe+i-1)
        nbco = zi(apncoe+i-1)
        nbnd = zi(apnbne+i-1)
        tnde = lne*nbsp
        tndq = lnq*nbsp
        tcoq = tndq*nbnd
        tcoe = tnde*nbnd
!         -- VECTEUR COLINEAIRE AU CHEMIN
        v1x = sgtu(4) - sgtu(1)
        v1y = sgtu(5) - sgtu(2)
        v1z = sgtu(6) - sgtu(3)
        v1n = sqrt(v1x**2+v1y**2+v1z**2)
        v1x = v1x/v1n
        v1y = v1y/v1n
        v1z = v1z/v1n
!         -- VECTEUR VECT_Y FOURNI
        v2x = vecty(1)
        v2y = vecty(2)
        v2z = vecty(3)
!         -- PROJECTION / NORMALISATION
        v2p = v1x*v2x + v1y*v2y + v1z*v2z
        v2x = v2x - v2p*v1x
        v2y = v2y - v2p*v1y
        v2z = v2z - v2p*v1z
        v2n = sqrt(v2x**2+v2y**2+v2z**2)
        v2x = v2x/v2n
        v2y = v2y/v2n
        v2z = v2z/v2n
!         -- VECTEUR TANGENT
        v3x = v1y*v2z - v2y*v1z
        v3y = v1z*v2x - v2z*v1x
        v3z = v1x*v2y - v2x*v1y
        if (niv .ge. 2) then
            write(ifm,1000) iocc
            write(ifm,1002) 'V1 : ', v1x, v1y, v1z
            write(ifm,1002) 'V2 : ', v2x, v2y, v2z
            write(ifm,1002) 'V3 : ', v3x, v3y, v3z
            1000       format( 'OCCURRENCE ', i4 )
            1002       format( 1p, a5, e12.5, 2x, e12.5, 2x, e12.5 )
        endif
        do 390,ico = 1,nbco,1
        do 380,j = 1,nbnd,1
        do 370,isp = 1,nbsp,1
        idece = (ico-1)*tcoe + (j-1)*tnde + (isp-1)* lne
        idecq = (ico-1)*tcoq + (j-1)*tndq + (isp-1)* lnq
        pt = 1
        if (tq .eq. 'V3') then
            do 350,k = 1,nbcpcd,1
            num = zi(anumcp+k-1)
            if (num .ge. 1 .and. num .le. 3) then
                idec = 0
            else
                idec = 3
            endif
            kd1 = zi(apcmpe+idec+1-1)
            kd2 = zi(apcmpe+idec+2-1)
            kd3 = zi(apcmpe+idec+3-1)
            if (zr(avalee+adre+idece+kd1-2) .eq. r8vide()) then
                tx = 0.d0
            else
                tx = zr(avalee+adre+idece+kd1-2)
            endif
            if (zr(avalee+adre+idece+kd2-2) .eq. r8vide()) then
                ty = 0.d0
            else
                ty = zr(avalee+adre+idece+kd2-2)
            endif
            if (zr(avalee+adre+idece+kd3-2) .eq. r8vide()) then
                tz = 0.d0
            else
                tz = zr(avalee+adre+idece+kd3-2)
            endif
            if (num .eq. 1 .or. num .eq. 4) then
                val = v1x*tx + v1y*ty + v1z*tz
            else if (num.eq.2 .or. num.eq.5) then
                val = v2x*tx + v2y*ty + v2z*tz
            else if (num.eq.3 .or. num.eq.6) then
                val = v3x*tx + v3y*ty + v3z*tz
            endif
            zr(avaleq+adrq+idecq+pt-2) = val
            pt = pt + 1
350          continue
        else if (tq.eq.'T3') then
            do 360,k = 1,nbcpcd,1
            num = zi(anumcp+k-1)
            kd1 = zi(apcmpe+1-1)
            kd2 = zi(apcmpe+2-1)
            kd3 = zi(apcmpe+3-1)
            kd4 = zi(apcmpe+4-1)
            kd5 = zi(apcmpe+5-1)
            kd6 = zi(apcmpe+6-1)
            if (zr(avalee+adre+idece+kd1-2) .eq. r8vide()) then
                txx = 0.d0
            else
                txx = zr(avalee+adre+idece+kd1-2)
            endif
            if (zr(avalee+adre+idece+kd2-2) .eq. r8vide()) then
                tyy = 0.d0
            else
                tyy = zr(avalee+adre+idece+kd2-2)
            endif
            if (zr(avalee+adre+idece+kd3-2) .eq. r8vide()) then
                tzz = 0.d0
            else
                tzz = zr(avalee+adre+idece+kd3-2)
            endif
            if (zr(avalee+adre+idece+kd4-2) .eq. r8vide()) then
                txy = 0.d0
            else
                txy = zr(avalee+adre+idece+kd4-2)
            endif
            if (zr(avalee+adre+idece+kd5-2) .eq. r8vide()) then
                txz = 0.d0
            else
                txz = zr(avalee+adre+idece+kd5-2)
            endif
            if (zr(avalee+adre+idece+kd6-2) .eq. r8vide()) then
                tyz = 0.d0
            else
                tyz = zr(avalee+adre+idece+kd6-2)
            endif
            if (num .eq. 1) then
                val = vtv(v1x,v1y,v1z,v1x,v1y,v1z)
            else if (num.eq.2) then
                val = vtv(v2x,v2y,v2z,v2x,v2y,v2z)
            else if (num.eq.3) then
                val = vtv(v3x,v3y,v3z,v3x,v3y,v3z)
            else if (num.eq.4) then
                val = vtv(v1x,v1y,v1z,v2x,v2y,v2z)
            else if (num.eq.5) then
                val = vtv(v1x,v1y,v1z,v3x,v3y,v3z)
            else
                val = vtv(v2x,v2y,v2z,v3x,v3y,v3z)
            endif
            zr(avaleq+adrq+idecq+pt-2) = val
            pt = pt + 1
360          continue
        else if (tq.eq.'AS') then
            call jelira(nvalee, 'LONMAX', l, kbid)
            do 362,il = 1,l,1
            zr(avaleq+il-1) = zr(avalee+il-1)
362          continue
        else
            call u2mesk('F', 'POSTRELE_14', 1, k4)
        endif
370      continue
380      continue
390      continue
400      continue
        call jedetr('&&RVCALQ.NUM.CP.CD')
!
    else
        call wkvect(nnocpq, 'V V K8', nbcpcd, anocpq)
        do 410,i = 1,nbcpcd,1
        zk8(anocpq+i-1) = nomcp(i)
410      continue
        if (repere(1:1) .eq. 'G') then
            call jelira(nvalee, 'LONMAX', l, kbid)
            do 420,i = 1,l,1
            zr(avaleq+i-1) = zr(avalee+i-1)
420          continue
        else
            call jelira(jexnum(sdlieu(1:19)//'.ABSC', 1), 'LONMAX', l, kbid)
!
            call jelira(npadrq, 'LONMAX', nbadrq, kbid)
            call jelira(sdlieu(1:19)//'.ABSC', 'NMAXOC', nboc, kbid)
            call jeveuo(npcmpe, 'L', apcmpe)
            call jeveuo(sdeval(1:19)//'.NUGD', 'L', iadr)
            call jeveuo(jexnum('&CATA.GD.NOMCMP', zi(iadr)), 'L', acpgd)
            call jelira(jexnum('&CATA.GD.NOMCMP', zi(iadr)), 'LONMAX', nc, kbid)
            call wkvect('&&RVCALQ.NUM.CP.CD', 'V V I', nbcpcd, anumcp)
            call numek8(zk8(acpgd), nomcp, nc, nbcpcd, zi(anumcp))
            ioc = 1
            do 500,i = 1,nbadrq,1
            adre = zi(apadre+i-1)
            adrq = zi(apadrq+i-1)
            nbsp = zi(apnspe+i-1)
            nbco = zi(apncoe+i-1)
            nbnd = zi(apnbne+i-1)
            tnde = lne*nbsp
            tndq = lnq*nbsp
            tcoq = tndq*nbnd
            tcoe = tnde*nbnd
            call wkvect('&&RVCALQ.V1X', 'V V R', nbnd, av1x)
            call wkvect('&&RVCALQ.V1Y', 'V V R', nbnd, av1y)
            call wkvect('&&RVCALQ.V2X', 'V V R', nbnd, av2x)
            call wkvect('&&RVCALQ.V2Y', 'V V R', nbnd, av2y)
            if ((docu.eq.'CHNO') .or. (docul.eq.'LSTN')) then
                if (repere .eq. 'LOCAL') then
                    v1x = vec1(2* (i-1)+1)
                    v1y = vec1(2* (i-1)+2)
                    v2x = vec2(2* (i-1)+1)
                    v2y = vec2(2* (i-1)+2)
                else if (repere.eq.'POLAIRE') then
                    v1x = vec1(2* (i-1)+1)
                    v1y = vec1(2* (i-1)+2)
                    v2x = -vec2(2* (i-1)+1)
                    v2y = -vec2(2* (i-1)+2)
                else
                    v1x = vec2(2* (i-1)+1)
                    v1y = vec2(2* (i-1)+2)
                    v2x = vec1(2* (i-1)+1)
                    v2y = vec1(2* (i-1)+2)
                endif
                do 430,j = 1,nbnd,1
                zr(av1x+j-1) = v1x
                zr(av1y+j-1) = v1y
                zr(av2x+j-1) = v2x
                zr(av2y+j-1) = v2y
430              continue
            else
                if (i .lt. l) then
                    n = i + ioc - 1
                else
                    n = n + 2
                    ioc = ioc + 1
                    if (ioc .le. nboc) then
                        call jelira(jexnum(sdlieu(1:19)//'.ABSC', ioc), 'LONMAX', ll, kbid)
                        l = l + ll - 1
                    endif
                endif
                if (repere .eq. 'LOCAL') then
                    zr(av1x+1-1) = vec1(2* (n-1)+1)
                    zr(av1y+1-1) = vec1(2* (n-1)+2)
                    zr(av2x+1-1) = vec2(2* (n-1)+1)
                    zr(av2y+1-1) = vec2(2* (n-1)+2)
                    zr(av1x+2-1) = vec1(2*n+1)
                    zr(av1y+2-1) = vec1(2*n+2)
                    zr(av2x+2-1) = vec2(2*n+1)
                    zr(av2y+2-1) = vec2(2*n+2)
                else
                    zr(av1x+1-1) = vec1(2* (n-1)+1)
                    zr(av1y+1-1) = vec1(2* (n-1)+2)
                    zr(av2x+1-1) = -vec2(2* (n-1)+1)
                    zr(av2y+1-1) = -vec2(2* (n-1)+2)
                    zr(av1x+2-1) = vec1(2*n+1)
                    zr(av1y+2-1) = vec1(2*n+2)
                    zr(av2x+2-1) = -vec2(2*n+1)
                    zr(av2y+2-1) = -vec2(2*n+2)
                endif
            endif
            do 490,ico = 1,nbco,1
            do 480,j = 1,nbnd,1
            v1x = zr(av1x+j-1)
            v1y = zr(av1y+j-1)
            v2x = zr(av2x+j-1)
            v2y = zr(av2y+j-1)
            do 470,isp = 1,nbsp,1
            idece = (ico-1)*tcoe + (j-1)*tnde + (isp- 1)*lne
            idecq = (ico-1)*tcoq + (j-1)*tndq + (isp- 1)*lnq
            pt = 1
            if (tq .eq. 'V3') then
                do 440,k = 1,nbcpcd,1
                num = zi(anumcp+k-1)
                if (num .eq. 1) then
                    kd1 = zi(apcmpe+1-1)
                    kd2 = zi(apcmpe+2-1)
                    if (zr(avalee+adre+idece+kd1- 2) .eq. r8vide()) then
                        tx = 0.d0
                    else
                        tx = zr(avalee+adre+idece+kd1- 2)
                    endif
                    if (zr(avalee+adre+idece+kd2- 2) .eq. r8vide()) then
                        ty = 0.d0
                    else
                        ty = zr(avalee+adre+idece+kd2- 2)
                    endif
                    val = v1x*tx + v1y*ty
                else if (num.eq.2) then
                    kd1 = zi(apcmpe+1-1)
                    kd2 = zi(apcmpe+2-1)
                    if (zr(avalee+adre+idece+kd1- 2) .eq. r8vide()) then
                        tx = 0.d0
                    else
                        tx = zr(avalee+adre+idece+kd1- 2)
                    endif
                    if (zr(avalee+adre+idece+kd2- 2) .eq. r8vide()) then
                        ty = 0.d0
                    else
                        ty = zr(avalee+adre+idece+kd2- 2)
                    endif
                    val = v2x*tx + v2y*ty
                else if (num.eq.3) then
                    kd1 = zi(apcmpe+3-1)
                    val = zr(avalee+adre+idece+ kd1-2)
                else if (num.eq.4) then
                    kd1 = zi(apcmpe+4-1)
                    kd2 = zi(apcmpe+5-1)
                    if (zr(avalee+adre+idece+kd1- 2) .eq. r8vide()) then
                        tx = 0.d0
                    else
                        tx = zr(avalee+adre+idece+kd1- 2)
                    endif
                    if (zr(avalee+adre+idece+kd2- 2) .eq. r8vide()) then
                        ty = 0.d0
                    else
                        ty = zr(avalee+adre+idece+kd2- 2)
                    endif
                    val = v1x*tx + v1y*ty
                else if (num.eq.5) then
                    kd1 = zi(apcmpe+4-1)
                    kd2 = zi(apcmpe+5-1)
                    if (zr(avalee+adre+idece+kd1- 2) .eq. r8vide()) then
                        tx = 0.d0
                    else
                        tx = zr(avalee+adre+idece+kd1- 2)
                    endif
                    if (zr(avalee+adre+idece+kd2- 2) .eq. r8vide()) then
                        ty = 0.d0
                    else
                        ty = zr(avalee+adre+idece+kd2- 2)
                    endif
                    val = v2x*tx + v2y*ty
                else
                    kd1 = zi(apcmpe+6-1)
                    val = zr(avalee+adre+idece+ kd1-2)
                endif
                zr(avaleq+adrq+idecq+pt-2) = val
                pt = pt + 1
440              continue
            else if (tq.eq.'T3') then
                do 450,k = 1,nbcpcd,1
                num = zi(anumcp+k-1)
                if (num .eq. 1) then
                    kd1 = zi(apcmpe+1-1)
                    kd2 = zi(apcmpe+2-1)
                    kd3 = zi(apcmpe+4-1)
                    if (zr(avalee+adre+idece+kd1- 2) .eq. r8vide()) then
                        txx = 0.d0
                    else
                        txx = zr(avalee+adre+idece+ kd1-2)
                    endif
                    if (zr(avalee+adre+idece+kd2- 2) .eq. r8vide()) then
                        tyy = 0.d0
                    else
                        tyy = zr(avalee+adre+idece+ kd2-2)
                    endif
                    if (zr(avalee+adre+idece+kd3- 2) .eq. r8vide()) then
                        txy = 0.d0
                    else
                        txy = zr(avalee+adre+idece+ kd3-2)
                    endif
                    val = v1x* (v1x*txx+v1y*txy) + v1y* (v1x*txy+v1y*tyy)
                else if (num.eq.2) then
                    kd1 = zi(apcmpe+1-1)
                    kd2 = zi(apcmpe+2-1)
                    kd3 = zi(apcmpe+4-1)
                    if (zr(avalee+adre+idece+kd1- 2) .eq. r8vide()) then
                        txx = 0.d0
                    else
                        txx = zr(avalee+adre+idece+ kd1-2)
                    endif
                    if (zr(avalee+adre+idece+kd2- 2) .eq. r8vide()) then
                        tyy = 0.d0
                    else
                        tyy = zr(avalee+adre+idece+ kd2-2)
                    endif
                    if (zr(avalee+adre+idece+kd3- 2) .eq. r8vide()) then
                        txy = 0.d0
                    else
                        txy = zr(avalee+adre+idece+ kd3-2)
                    endif
                    val = v2x* (v2x*txx+v2y*txy) + v2y* (v2x*txy+v2y*tyy)
                else if (num.eq.3) then
                    kd1 = zi(apcmpe+3-1)
                    val = zr(avalee+adre+idece+ kd1-2)
                else if (num.eq.4) then
                    kd1 = zi(apcmpe+1-1)
                    kd2 = zi(apcmpe+2-1)
                    kd3 = zi(apcmpe+4-1)
                    if (zr(avalee+adre+idece+kd1- 2) .eq. r8vide()) then
                        txx = 0.d0
                    else
                        txx = zr(avalee+adre+idece+ kd1-2)
                    endif
                    if (zr(avalee+adre+idece+kd2- 2) .eq. r8vide()) then
                        tyy = 0.d0
                    else
                        tyy = zr(avalee+adre+idece+ kd2-2)
                    endif
                    if (zr(avalee+adre+idece+kd3- 2) .eq. r8vide()) then
                        txy = 0.d0
                    else
                        txy = zr(avalee+adre+idece+ kd3-2)
                    endif
                    val = v1x* (v2x*txx+v2y*txy) + v1y* (v2x*txy+v2y*tyy)
                else if (num.eq.5) then
                    kd1 = zi(apcmpe+5-1)
                    kd2 = zi(apcmpe+6-1)
                    if (zr(avalee+adre+idece+kd1- 2) .eq. r8vide()) then
                        txz = 0.d0
                    else
                        txz = zr(avalee+adre+idece+ kd1-2)
                    endif
                    if (zr(avalee+adre+idece+kd2- 2) .eq. r8vide()) then
                        tyz = 0.d0
                    else
                        tyz = zr(avalee+adre+idece+ kd2-2)
                    endif
                    val = v1x*txz + v1y*tyz
                else
                    kd1 = zi(apcmpe+5-1)
                    kd2 = zi(apcmpe+6-1)
                    if (zr(avalee+adre+idece+kd1- 2) .eq. r8vide()) then
                        txz = 0.d0
                    else
                        txz = zr(avalee+adre+idece+ kd1-2)
                    endif
                    if (zr(avalee+adre+idece+kd2- 2) .eq. r8vide()) then
                        tyz = 0.d0
                    else
                        tyz = zr(avalee+adre+idece+ kd2-2)
                    endif
                    val = v2x*txz + v2y*tyz
                endif
                zr(avaleq+adrq+idecq+pt-2) = val
                pt = pt + 1
450              continue
            else
                kd1 = zi(apcmpe+7-1)
                kd2 = zi(apcmpe+8-1)
                kd3 = zi(apcmpe+9-1)
                if (zr(avalee+adre+idece+(j-1)*lne+ kd1-2) .eq. r8vide()) then
                    txx = 0.d0
                else
                    txx = zr(avalee+adre+idece+(j-1)* lne+kd1-2)
                endif
                if (zr(avalee+adre+idece+(j-1)*lne+ kd2-2) .eq. r8vide()) then
                    tyy = 0.d0
                else
                    tyy = zr(avalee+adre+idece+(j-1)* lne+kd2-2)
                endif
                if (zr(avalee+adre+idece+(j-1)*lne+ kd3-2) .eq. r8vide()) then
                    txy = 0.d0
                else
                    txy = zr(avalee+adre+idece+(j-1)* lne+kd3-2)
                endif
                kd1 = zi(apcmpe+10-1)
                kd2 = zi(apcmpe+11-1)
                kd3 = zi(apcmpe+12-1)
                if (zr(avalee+adre+idece+(j-1)*lne+ kd1-2) .eq. r8vide()) then
                    sxx = 0.d0
                else
                    sxx = zr(avalee+adre+idece+(j-1)* lne+kd1-2)
                endif
                if (zr(avalee+adre+idece+(j-1)*lne+ kd2-2) .eq. r8vide()) then
                    syy = 0.d0
                else
                    syy = zr(avalee+adre+idece+(j-1)* lne+kd2-2)
                endif
                if (zr(avalee+adre+idece+(j-1)*lne+ kd3-2) .eq. r8vide()) then
                    sxy = 0.d0
                else
                    sxy = zr(avalee+adre+idece+(j-1)* lne+kd3-2)
                endif
                do 460,k = 1,nbcpcd,1
                num = zi(anumcp+k-1)
                if (num .eq. 7) then
                    val = v1x* (v1x*txx+v1y*txy) + v1x* (v1x*txy+v1y*tyy)
                else if (num.eq.8) then
                    val = v2x* (v2x*txx+v2y*txy) + v2x* (v2x*txy+v2y*tyy)
                else if (num.eq.9) then
                    val = v1x* (v2x*txx+v2y*txy) + v1x* (v2x*txy+v2y*tyy)
                else if (num.eq.10) then
                    val = v1x* (v1x*sxx+v1y*sxy) + v1x* (v1x*sxy+v1y*syy)
                else if (num.eq.11) then
                    val = v2x* (v2x*sxx+v2y*sxy) + v2x* (v2x*sxy+v2y*syy)
                else
                    val = v1x* (v2x*sxx+v2y*sxy) + v1x* (v2x*sxy+v2y*syy)
                endif
                zr(avaleq+adrq+idecq+pt-2) = val
                pt = pt + 1
460              continue
            endif
470          continue
480          continue
490          continue
            call jedetr('&&RVCALQ.V1X')
            call jedetr('&&RVCALQ.V1Y')
            call jedetr('&&RVCALQ.V2X')
            call jedetr('&&RVCALQ.V2Y')
500          continue
            call jedetr('&&RVCALQ.NUM.CP.CD')
        endif
    endif
    call jeecra(nvaleq, 'DOCU', i, docu)
    if (docu .eq. 'CHNO') then
        call jedetr('&&RVCALQ.CHNO.PNSP')
        call jedetr('&&RVCALQ.CHNO.PNCO')
        call jedetr('&&RVCALQ.CHNO.PNBN')
    endif
    call jedema()
end subroutine
