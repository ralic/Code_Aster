subroutine op0168()
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     OPERATEUR  EXTR_MODE
!     ------------------------------------------------------------------
!
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/infmaj.h"
#include "asterfort/iunifi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerecu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsvpar.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/vpcrea.h"
#include "asterfort/vprecu.h"
#include "asterfort/vpstor.h"
#include "asterfort/wkvect.h"
!-----------------------------------------------------------------------
    integer :: i, ibid, ifr, impr, iord, iprec, iret
    integer :: j, jadr, jme, jnom, jor, jordr
    integer :: k, lmod, lmode, lvali, lvalk, lvalr, n1
    integer :: n10, n2, n3, n4, n5, n6, n7
    integer :: n8, n9, nbfilt, nbme, nbmode, nbmodt, nbmodu
    integer :: nbmr, nbpara, nbpari, nbpark, nbparr, ndimt, neq
    integer :: npari, npark, nparr, nume, nume1, nume2
    real(kind=8) :: cumulx, cumuly, cumulz, dx, dy, dz, fremax
    real(kind=8) :: fremin, freq, seuil, undf
!-----------------------------------------------------------------------
    parameter   ( nbpari=1 , nbparr=15 , nbpark=3, nbpara=19 )
    integer :: lpar(3)
    integer :: vali(2)
    real(kind=8) :: r8b, prec, zero, mastot, valr(7)
    character(len=1) ::  typmod
    character(len=3) :: ouinon
    character(len=8) :: k8b, modeou, modein
    character(len=16) :: typcon, nomcmd, critfi, nompar(3), nomsy, nompav
    character(len=19) :: numedd
    character(len=24) :: masse, amor, raide, refd, massi, amori, raidi, kmode
    character(len=24) :: valk(3)
    character(len=24) :: kvec, kvali, kvalr, kvalk, nopara(nbpara)
    complex(kind=8) :: c16b
    integer :: iarg
!     ------------------------------------------------------------------
    data  refd  / '                   .REFD' /
    data  kvec  / '&&OP0168.VAL_PROPRE' /
    data  kvali / '&&OP0168.GRAN_MODAL_I' /
    data  kvalr / '&&OP0168.GRAN_MODAL_R' /
    data  kvalk / '&&OP0168.GRAN_MODAL_K_' /
    data nompar / 'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' ,&
     &              'MASS_EFFE_UN_DZ' /
    data  nopara /        'NUME_MODE'       ,&
     &  'NORME'           , 'TYPE_MODE'       , 'NOEUD_CMP'       ,&
     &  'FREQ'            , 'OMEGA2'          , 'AMOR_REDUIT'     ,&
     &  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,&
     &  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,&
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,&
     &  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    zero = 0.d0
    undf = r8vide( )
    call getres(modeou, typcon, nomcmd)
    ifr = iunifi('RESULTAT')
!
!     --- RECUPERATION DU NOMBRE DE MODES A EXTRAIRE ---
    kmode = '&&OP0168.MODE.RETENU'
    nbmr = 0
    ndimt = 0
    call getfac('FILTRE_MODE', nbfilt)
    if (nbfilt .ne. 0) then
        call wkvect('&&OP0168.NOM_MODE', 'V V K8', nbfilt, jnom)
        call jecrec(kmode, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbfilt)
        do 10 i = 1, nbfilt
            call getvid('FILTRE_MODE', 'MODE', i, iarg, 1,&
                        modein, n1)
            call jelira(modein//'           .ORDR', 'LONUTI', iret)
            if (iret .eq. 0) goto 10
            nbmr = nbmr + 1
!
            if (nbmr .eq. 1) then
!      --- MATRICES DE REFERENCE DES MODES ---
                refd(1:8) = modein
                call jeveuo(refd, 'L', lmode)
                raide = zk24(lmode)
                masse = zk24(lmode+1)
                amor = zk24(lmode+2)
                numedd = zk24(lmode+3)(1:19)
                call vpcrea(0, modeou, masse, amor, raide,&
                            numedd, ibid)
            endif
!
            zk8(jnom+nbmr-1) = modein
            refd(1:8) = modein
            call jeveuo(refd, 'L', lmode)
            raidi = zk24(lmode)
            massi = zk24(lmode+1)
            amori = zk24(lmode+2)
            if (massi .ne. masse .or. amori .ne. amor .or. raidi .ne. raide) call u2mess(&
                                                                             'F', 'ALGELINE3_9')
!
            call rsorac(modein, 'LONUTI', ibid, r8b, k8b,&
                        c16b, r8b, k8b, nbmodt, 1,&
                        ibid)
            call wkvect('&&OP0168.NUME_ORDRE', 'V V I', nbmodt, jor)
            call rsorac(modein, 'TOUT_ORDRE', ibid, r8b, k8b,&
                        c16b, r8b, k8b, zi(jor), nbmodt,&
                        ibid)
!
            call jecroc(jexnum(kmode, nbmr))
            call jeecra(jexnum(kmode, nbmr), 'LONMAX', nbmodt)
            call jeveuo(jexnum(kmode, nbmr), 'E', jordr)
!
            call getvtx('FILTRE_MODE', 'TOUT_ORDRE', i, iarg, 1,&
                        ouinon, n1)
            if (n1 .ne. 0 .and. ouinon .eq. 'OUI') then
                nbmode = nbmodt
                do 12 j = 1, nbmode
                    zi(jordr+j-1) = zi(jor+j-1)
12              continue
                call jeecra(jexnum(kmode, nbmr), 'LONUTI', nbmode)
                ndimt = ndimt + nbmode
                call jedetr('&&OP0168.NUME_ORDRE')
                goto 10
            endif
!
            call getvis('FILTRE_MODE', 'NUME_ORDRE', i, iarg, 0,&
                        ibid, n2)
            if (n2 .ne. 0) then
                nbmodu = -n2
                call wkvect('&&OP0168.NUME_MODE', 'V V I', nbmodu, jme)
                call getvis('FILTRE_MODE', 'NUME_ORDRE', i, iarg, nbmodu,&
                            zi(jme), n2)
                nbmode = 0
                do 20 j = 1, nbmodu
                    do 22 k = 1, nbmodt
                        if (zi(jme+j-1) .eq. zi(jor+k-1)) then
                            nbmode = nbmode + 1
                            zi(jordr+nbmode-1) = zi(jme+j-1)
                            goto 20
                        endif
22                  continue
                    valk(1) = modein
                    vali(1) = zi(jme+j-1)
                    call u2mesg('A', 'ALGELINE4_55', 1, valk, 1,&
                                vali, 0, 0.d0)
20              continue
                call jeecra(jexnum(kmode, nbmr), 'LONUTI', nbmode)
                ndimt = ndimt + nbmode
                call jedetr('&&OP0168.NUME_MODE')
                call jedetr('&&OP0168.NUME_ORDRE')
                goto 10
            endif
!
            call getvis('FILTRE_MODE', 'NUME_MODE', i, iarg, 0,&
                        ibid, n3)
            if (n3 .ne. 0) then
                nbmodu = -n3
                call wkvect('&&OP0168.NUME_MODE', 'V V I', nbmodu, jme)
                call getvis('FILTRE_MODE', 'NUME_MODE', i, iarg, nbmodu,&
                            zi(jme), n3)
                nbmode = 0
                do 30 j = 1, nbmodt
                    iord = zi(jor+j-1)
                    call rsadpa(modein, 'L', 1, 'NUME_MODE', iord,&
                                0, jadr, k8b)
                    nume = zi(jadr)
                    do 32 k = 1, nbmodu
                        if (nume .eq. zi(jme+k-1)) then
                            nbmode = nbmode + 1
                            zi(jordr+nbmode-1) = iord
                        endif
32                  continue
30              continue
                call jeecra(jexnum(kmode, nbmr), 'LONUTI', nbmode)
                ndimt = ndimt + nbmode
                call jedetr('&&OP0168.NUME_MODE')
                call jedetr('&&OP0168.NUME_ORDRE')
                goto 10
            endif
!
            call getvis('FILTRE_MODE', 'NUME_MODE_EXCLU', i, iarg, 0,&
                        ibid, n4)
            if (n4 .ne. 0) then
                nbme = -n4
                call wkvect('&&OP0168.NUME_MODE', 'V V I', nbme, jme)
                call getvis('FILTRE_MODE', 'NUME_MODE_EXCLU', i, iarg, nbme,&
                            zi(jme), n4)
                nbmode = 0
                do 40 j = 1, nbmodt
                    iord = zi(jor+j-1)
                    call rsadpa(modein, 'L', 1, 'NUME_MODE', iord,&
                                0, jadr, k8b)
                    nume = zi(jadr)
                    do 42 k = 1, nbme
                        if (nume .eq. zi(jme+k-1)) goto 40
42                  continue
                    nbmode = nbmode + 1
                    zi(jordr+nbmode-1) = iord
40              continue
                call jeecra(jexnum(kmode, nbmr), 'LONUTI', nbmode)
                ndimt = ndimt + nbmode
                call jedetr('&&OP0168.NUME_ORDRE')
                call jedetr('&&OP0168.NUME_MODE')
                goto 10
            endif
!
            call getvr8('FILTRE_MODE', 'FREQ_MIN', i, iarg, 0,&
                        r8b, n5)
            if (n5 .ne. 0) then
                call getvr8('FILTRE_MODE', 'FREQ_MIN', i, iarg, 1,&
                            fremin, n5)
                call getvr8('FILTRE_MODE', 'FREQ_MAX', i, iarg, 1,&
                            fremax, n5)
                call getvr8('FILTRE_MODE', 'PRECISION', i, iarg, 1,&
                            prec, n5)
                fremin = fremin - prec
                fremax = fremax + prec
                nbmode = 0
                do 50 j = 1, nbmodt
                    iord = zi(jor+j-1)
                    call rsadpa(modein, 'L', 1, 'FREQ', iord,&
                                0, jadr, k8b)
                    freq = zr(jadr)
                    if (freq .ge. fremin .and. freq .le. fremax) then
                        nbmode = nbmode + 1
                        zi(jordr+nbmode-1) = iord
                    endif
50              continue
                call jeecra(jexnum(kmode, nbmr), 'LONUTI', nbmode)
                ndimt = ndimt + nbmode
                call jedetr('&&OP0168.NUME_ORDRE')
                goto 10
            endif
!
            call getvtx('FILTRE_MODE', 'CRIT_EXTR', i, iarg, 0,&
                        k8b, n6)
            if (n6 .ne. 0) then
                call getvtx('FILTRE_MODE', 'CRIT_EXTR', i, iarg, 1,&
                            critfi, n6)
                call getvr8('FILTRE_MODE', 'SEUIL', i, iarg, 1,&
                            seuil, n7)
                call getvr8('FILTRE_MODE', 'SEUIL_X', i, iarg, 1,&
                            seuil, n8)
                call getvr8('FILTRE_MODE', 'SEUIL_Y', i, iarg, 1,&
                            seuil, n9)
                call getvr8('FILTRE_MODE', 'SEUIL_Z', i, iarg, 1,&
                            seuil, n10)
                nbmode = 0
                if (critfi .eq. 'MASS_EFFE_UN' .and. typcon(1:9) .eq. 'MODE_MECA') then
                    do 60 j = 1, nbmodt
                        iord = zi(jor+j-1)
                        call rsadpa(modein, 'L', 3, nompar, iord,&
                                    0, lpar, k8b)
                        dx = zr(lpar(1))
                        dy = zr(lpar(2))
                        dz = zr(lpar(3))
                        if (dx .eq. undf .or. dy .eq. undf .or. dz .eq. undf) then
                            call u2mess('F', 'ALGELINE3_10')
                        endif
                        if (n7 .ne. 0) then
                            if (dx .ge. seuil .or. dy .ge. seuil .or. dz .ge. seuil) then
                                nbmode = nbmode + 1
                                zi(jordr+nbmode-1) = iord
                            endif
                        else if (n8.ne.0) then
                            if (dx .ge. seuil) then
                                nbmode = nbmode + 1
                                zi(jordr+nbmode-1) = iord
                            endif
                        else if (n9.ne.0) then
                            if (dy .ge. seuil) then
                                nbmode = nbmode + 1
                                zi(jordr+nbmode-1) = iord
                            endif
                        else if (n10.ne.0) then
                            if (dz .ge. seuil) then
                                nbmode = nbmode + 1
                                zi(jordr+nbmode-1) = iord
                            endif
                        endif
!
60                  continue
                endif
                if (critfi .eq. 'MASS_GENE') then
                    mastot = zero
                    do 61 j = 1, nbmodt
                        iord = zi(jor+j-1)
                        nompav = 'MASS_GENE'
                        call rsadpa(modein, 'L', 1, nompav, iord,&
                                    0, lpar, k8b)
                        mastot = mastot + zr(lpar(1))
61                  continue
                    do 62 j = 1, nbmodt
                        iord = zi(jor+j-1)
                        nompav = 'MASS_GENE'
                        call rsadpa(modein, 'L', 1, nompav, iord,&
                                    0, lpar, k8b)
                        dx = zr(lpar(1))/mastot
                        if (dx .ge. seuil) then
                            nbmode = nbmode + 1
                            zi(jordr+nbmode-1) = iord
                        endif
62                  continue
                endif
                call jeecra(jexnum(kmode, nbmr), 'LONUTI', nbmode)
                ndimt = ndimt + nbmode
                call jedetr('&&OP0168.NUME_ORDRE')
                goto 10
            endif
10      continue
    endif
!
!     --- STOCKAGE ---
!
    if (ndimt .eq. 0) call u2mess('F', 'ALGELINE3_11')
    call rscrsd('G', modeou, typcon, ndimt)
    iprec = 0
    nomsy = 'DEPL'
    do 100 i = 1, nbmr
        call jemarq()
        call jerecu('V')
        modein = zk8(jnom+i-1)
        call jelira(jexnum(kmode, i), 'LONUTI', nbmode)
        if (nbmode .eq. 0) then
            valk(1) = modein
            call u2mesg('A', 'ALGELINE4_56', 1, valk, 0,&
                        0, 0, 0.d0)
            goto 102
        endif
        call jeveuo(jexnum(kmode, i), 'L', jordr)
        call vprecu(modein, nomsy, nbmode, zi(jordr), kvec,&
                    nbpara, nopara, kvali, kvalr, kvalk,&
                    neq, nbmode, typmod, npari, nparr,&
                    npark)
        ASSERT(npari.eq.nbpari)
        ASSERT(nparr.eq.nbparr)
        ASSERT(npark.eq.nbpark)
        call jeveuo(kvec, 'L', lmod)
        call jeveuo(kvali, 'L', lvali)
        call jeveuo(kvalr, 'L', lvalr)
        call jeveuo(kvalk, 'L', lvalk)
        if (typmod .eq. 'R') then
            call vpstor(-1, typmod, modeou, nbmode, neq,&
                        zr(lmod), c16b, nbmode, nbpari, nbparr,&
                        nbpark, nopara, '    ', zi(lvali), zr(lvalr),&
                        zk24(lvalk), iprec)
        else if (typmod .eq. 'C') then
            call vpstor(-1, typmod, modeou, nbmode, neq,&
                        r8b, zc(lmod), nbmode, nbpari, nbparr,&
                        nbpark, nopara, '    ', zi(lvali), zr(lvalr),&
                        zk24(lvalk), iprec)
        else
            call u2mesk('F', 'ALGELINE2_44', 1, typmod)
        endif
        iprec = iprec + nbmode
        call jedetr(kvec)
        call jedetr(kvali)
        call jedetr(kvalr)
        call jedetr(kvalk)
102      continue
        call jedema()
100  end do
!
!     --- ON ALARME SI NUME_MODE IDENTIQUE ---
!
    call rsorac(modeou, 'LONUTI', ibid, r8b, k8b,&
                c16b, r8b, k8b, nbmode, 1,&
                ibid)
    call wkvect('&&OP0168.NUME_ORDRE', 'V V I', nbmode, jordr)
    call rsorac(modeou, 'TOUT_ORDRE', ibid, r8b, k8b,&
                c16b, r8b, k8b, zi(jordr), nbmode,&
                ibid)
    do 200 j = 1, nbmode
        iord = zi(jordr+j-1)
        call rsadpa(modeou, 'L', 1, 'NUME_MODE', iord,&
                    0, jadr, k8b)
        nume1 = zi(jadr)
        do 210 k = j+1, nbmode
            iord = zi(jordr+k-1)
            call rsadpa(modeou, 'L', 1, 'NUME_MODE', iord,&
                        0, jadr, k8b)
            nume2 = zi(jadr)
            if (nume1 .eq. nume2) then
                vali(1) = ibid
                vali(2) = iord
                call u2mesg('A', 'ALGELINE4_57', 0, ' ', 2,&
                            vali, 0, 0.d0)
            endif
210      continue
200  end do
!
!     --- LES IMPRESSIONS ---
!
    call getfac('IMPRESSION', impr)
    if (impr .ne. 0) then
        valk(1)=modeou
        valk(2)=typcon
        valk(3)=nomcmd
        call u2mesk('I', 'ALGELINE6_8', 3, valk)
        call getvtx('IMPRESSION', 'CUMUL', 1, iarg, 1,&
                    ouinon, n1)
        call getvtx('IMPRESSION', 'CRIT_EXTR', 1, iarg, 1,&
                    critfi, n2)
!
        call rsvpar(modeou, 1, 'MASS_EFFE_UN_DX', ibid, undf,&
                    k8b, iret)
        call rsadpa(modeou, 'L', 1, 'MASS_EFFE_UN_DX', 1,&
                    0, jadr, k8b)
!
        if (iret .ne. 100 .and. critfi .eq. 'MASS_EFFE_UN' .and. typcon(1:9) .eq.&
            'MODE_MECA') then
            if (ouinon .eq. 'OUI') then
                call u2mess('I', 'ALGELINE6_50')
            else
                call u2mess('I', 'ALGELINE6_52')
            endif
            cumulx = 0.d0
            cumuly = 0.d0
            cumulz = 0.d0
            do 300 j = 1, nbmode
                iord = zi(jordr+j-1)
                call rsadpa(modeou, 'L', 1, 'NUME_MODE', iord,&
                            0, jadr, k8b)
                nume = zi(jadr)
                call rsadpa(modeou, 'L', 1, 'FREQ', iord,&
                            0, jadr, k8b)
                freq = zr(jadr)
                call rsadpa(modeou, 'L', 3, nompar, iord,&
                            0, lpar, k8b)
                dx = zr(lpar(1))
                dy = zr(lpar(2))
                dz = zr(lpar(3))
                if (dx .eq. undf .or. dy .eq. undf .or. dz .eq. undf) then
                    call u2mess('F', 'ALGELINE3_10')
                endif
                cumulx = cumulx + dx
                cumuly = cumuly + dy
                cumulz = cumulz + dz
                vali(1) = iord
                vali(2) = nume
                valr(1) = freq
                valr(2) = dx
                valr(3) = dy
                valr(4) = dz
                valr(5) = cumulx
                valr(6) = cumuly
                valr(7) = cumulz
                if (ouinon .eq. 'OUI') then
                    call u2mesg('I', 'ALGELINE6_51', 0, '', 2,&
                                vali, 7, valr)
                else
                    call u2mesg('I', 'ALGELINE6_53', 0, '', 2,&
                                vali, 4, valr)
                endif
300          continue
        endif
        if (critfi .eq. 'MASS_GENE') then
            if (ouinon .eq. 'OUI') then
                call u2mess('I', 'ALGELINE6_54')
            else
                call u2mess('I', 'ALGELINE6_56')
            endif
            cumulx = 0.d0
            do 301 j = 1, nbmode
                iord = zi(jordr+j-1)
                call rsadpa(modeou, 'L', 1, 'NUME_MODE', iord,&
                            0, jadr, k8b)
                nume = zi(jadr)
                call rsadpa(modeou, 'L', 1, 'FREQ', iord,&
                            0, jadr, k8b)
                freq = zr(jadr)
                nompav = 'MASS_GENE'
                call rsadpa(modeou, 'L', 1, nompav, iord,&
                            0, lpar, k8b)
                dx = zr(lpar(1))
                cumulx = cumulx + dx
                vali(1) = iord
                vali(2) = nume
                valr(1) = freq
                valr(2) = dx
                valr(3) = cumulx
                if (ouinon .eq. 'OUI') then
                    call u2mesg('I', 'ALGELINE6_55', 0, '', 2,&
                                vali, 3, valr)
                else
                    call u2mesg('I', 'ALGELINE6_57', 0, '', 2,&
                                vali, 2, valr)
                endif
301          continue
        endif
    endif
!
    call titre()
!
    call jedema()
end subroutine
