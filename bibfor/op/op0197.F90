subroutine op0197()
    implicit none
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
!     RECA_WEIBULL        ---------
!                         COMMANDE OPTIMISATION WEIBULL : RECALAGE DES
!                         PARAMETRES DE LA METHODE.
!     RECA_WEIBULL        ---------
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/chmrck.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/gnomsd.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/interp.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/optimw.h"
#include "asterfort/peweib.h"
#include "asterfort/rccome.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsorac.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbexve.h"
#include "asterfort/tbfutb.h"
#include "asterfort/tbimpr.h"
#include "asterfort/tbtrtb.h"
#include "asterfort/utmess.h"
#include "asterfort/uttrir.h"
#include "asterfort/wkvect.h"
#include "asterfort/rs_getlast.h"
#include "asterfort/rs_getfirst.h"
!
    integer :: nbparr, nbpark, nbpars, nbpart, info, kk
    integer :: valii
    parameter    ( nbparr = 4, nbpark = 3, nbpars=3, nbpart=3 )
    character(len=6) :: chtemp
    character(len=8) :: tapait, k8bid, cara, typarr(nbparr), typars(nbpars)
    character(len=8) :: typart(nbpart), tabtri, typark(nbpark), tabout, resu
    character(len=8) :: chcop1, chcop2
    character(len=11) :: k11
    character(len=16) :: optcal(2), method, nomcmd, nomrc, concep, parcal(2)
    character(len=16) :: noparr(nbparr), nopark(nbpark), nopars(nbpars)
    character(len=16) :: nopart(nbpart)
    character(len=19) :: nomres
    character(len=24) :: collec, mate, noobj
    integer :: nbresu, ifm, n1, niv, itemp, ichmat, iresu, imod, nbins, iinst
    integer :: itabw, nbite, nitmax, isig, i, ibid, nbmtcm, nbmtrc, nbcal, nbval
    integer :: it, iseg, nchar, jcha, itabr, vali(nbparr), ix, iy
    integer :: nrupt, iweik, iweir, ipro, irent, isigk, isigkp, isigi, ntemp
    integer :: itpsi, itpre, ntpsi, ipth, inopa, itypa, ivapa, ikval, ikvak, imc
    integer :: nume_first, nume_last, nbold, anomm1, anomm2, iret
    real(kind=8) :: mini, minip, vini, epsi, mk, mkp, sigint, r8bid
    real(kind=8) :: valr(nbparr), test, proint, maxcs, tpsmin, tpsmax
    real(kind=8) :: valrr(3), inst_last, inst_first
    complex(kind=8) :: c16b
    aster_logical :: calm, cals, impr, dept, recm, recs
    integer, pointer :: nom_nures(:) => null()
    real(kind=8), pointer :: nom_inssig(:) => null()
    character(len=8), pointer :: vale(:) => null()
!
    data noparr / 'NURES', 'INST','SIGMA_WEIBULL','PROBA_WEIBULL'/
    data typarr / 'I','R','R','R' /
    data nopark / 'ITER_K', 'M(K)','SIGU(K)' /
    data typark / 'I','R','R' /
    data nopars / 'SIGMA_WEIBULL', 'PROBA_THE', 'PROBA_EXP'/
    data typars / 'R','R','R'/
    data nopart / 'TEMP','M', 'SIGMA_U'/
    data typart / 'R','R','R' /
    data chcop1 / '&&OPTIW1' /
    data chcop2/ '&&OPTIW2' /
!     ------------------------------------------------------------------
    call jemarq()
    call infmaj()
!     ------------------------------------------------------------------
    call getres(nomres, concep, nomcmd)
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, niv)
!
!     LECTURE DES MOTS-CLES DE RECA_WEIBULL
!
    r8bid = 0.d0
    impr = .false.
    call getvis(' ', 'INFO', scal=info, nbret=n1)
    if (info .eq. 2) impr = .true.
    call getvtx(' ', 'OPTION', scal=optcal(1), nbret=n1)
    call getvtx(' ', 'CORR_PLAST', scal=optcal(2), nbret=n1)
    call getvtx(' ', 'METHODE', scal=method, nbret=n1)
    call getvis(' ', 'ITER_GLOB_MAXI', scal=nitmax, nbret=n1)
    call getvr8(' ', 'INCO_GLOB_RELA', scal=epsi, nbret=n1)
    call getvtx(' ', 'LIST_PARA', nbval=0, nbret=n1)
    nbcal = -n1
    call getvtx(' ', 'LIST_PARA', nbval=nbcal, vect=parcal, nbret=n1)
!
!     CALM,CALS : SIGNIFIE QUE M OU/ET SIGMA ONT CONVERGES
!     RECM,RECS : SIGNIFIE QUE M OU/ET SIGMA SONT A RECALER
!
    calm = .true.
    cals = .true.
    recm = .false.
    recs = .false.
    do i = 1, nbcal
        if (parcal(i)(1:1) .eq. 'M') then
            calm = .false.
            recm = .true.
        endif
        if (parcal(i)(1:9) .eq. 'SIGM_REFE') then
            cals = .false.
            recs = .true.
        endif
    end do
!
!     --- LECTURE DES BASES DE RESULTATS (MOT-CLE RESU) ---
!
    call getfac('RESU', nbresu)
!
    call wkvect('&&OP0197.KVALRC', 'V V K24', nbresu, ikval)
    call wkvect('&&OP0197.KVALRCK', 'V V K24', nbresu, ikvak)
    call wkvect('&&OP0197.MODELE', 'V V K8', nbresu, imod)
    call wkvect('&&OP0197.TEMPE_RESU', 'V V R', nbresu, itemp)
    call wkvect('&&OP0197.TEMPE_SIGU', 'V V R', nbresu, itpsi)
    call wkvect('&&OP0197.INDTP_NURE', 'V V I', nbresu, itpre)
    call wkvect('&&OP0197.NBINS_RESU', 'V V I', nbresu, irent)
    call wkvect('&&OP0197.NOM_CHMAT', 'V V K8', nbresu, ichmat)
    call wkvect('&&OP0197.SIGMA_I', 'V V R', nbresu, isigi)
    call wkvect('&&OP0197.SIGMA_K', 'V V R', nbresu, isigk)
    call wkvect('&&OP0197.SIGMA_KP', 'V V R', nbresu, isigkp)
    collec = '&&OP0197.INST_RUPT'
    call jecrec(collec, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbresu)
    tabout = 'TABLE1'
    tabtri = 'TABLE2'
!
    nrupt = 0
    ntemp = 0
    ntpsi = 0
    do iresu = 1, nbresu
!
        call getvid('RESU', 'MODELE', iocc=iresu, scal=zk8(imod-1+iresu), nbret=n1)
        call getvid('RESU', 'CHAM_MATER', iocc=iresu, scal=zk8(ichmat-1+ iresu), nbret=n1)
        call getvr8('RESU', 'TEMPE', iocc=iresu, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            ntemp = ntemp+1
            call getvr8('RESU', 'TEMPE', iocc=iresu, scal=zr(itemp-1+iresu), nbret=n1)
            ntpsi = ntpsi+1
            zr(itpsi-1+ntpsi) = zr(itemp-1+iresu)
            zi(itpre-1+iresu) = ntpsi
            do i = 1, ntpsi-1
                if (zr(itemp-1+iresu) .eq. zr(itpsi-1+i)) then
                    ntpsi = ntpsi-1
                    zi(itpre-1+iresu) = i
                endif
            end do
        else
            zi(itpre-1+iresu) = 1
        endif
!
!      --- LECTURE DE LA LISTE D'INSTANTS DE RUPTURE (TRI CROISSANT)
!
        call getvr8('RESU', 'LIST_INST_RUPT', iocc=iresu, nbval=0, nbret=n1)
        nbins = -n1
        call jecroc(jexnum(collec, iresu))
        call jeecra(jexnum(collec, iresu), 'LONMAX', nbins)
        call jeveuo(jexnum(collec, iresu), 'E', iinst)
        call getvr8('RESU', 'LIST_INST_RUPT', iocc=iresu, nbval=nbins, vect=zr( iinst),&
                    nbret=n1)
        nbold = nbins
        call uttrir(nbins, zr(iinst), 0.d0)
        if (nbins .ne. nbold) then
            call utmess('F', 'UTILITAI3_28')
        endif
        zi(irent+iresu-1) = nbins
        nrupt = nrupt + nbins
        call jeecra(jexnum(collec, iresu), 'LONUTI', nbins)
        if (nbins .le. 1) then
            call utmess('F', 'UTILITAI3_29')
        endif
!
!       ON TESTE SI LES INSTANTS DE RUPTURE MIN ET MAX SONT
!       DANS LES INSTANTS DE CALCUL
!
        call getvid('RESU', 'EVOL_NOLI', iocc=iresu, scal=resu, nbret=n1)
        call rs_getfirst(resu, nume_first, inst_first)              
        call rs_getlast(resu, nume_last , inst_last)   
        tpsmin = inst_first
        tpsmax = inst_last
        if (zr(iinst) .lt. tpsmin) then
            valrr (1) = zr(iinst)
            valrr (2) = tpsmin
            call utmess('F', 'UTILITAI6_53', nr=2, valr=valrr)
        endif
        if (zr(iinst+nbins-1) .gt. tpsmax) then
            valrr (1) = zr(iinst+nbins-1)
            valrr (2) = tpsmax
            call utmess('F', 'UTILITAI6_54', nr=2, valr=valrr)
        endif
!
    end do
!
!     --- ON REGARDE SI LE RECALAGE DOIT S'EFFECTUER EN FONCTION
!     --- DE LA TEMPERATURE : SIGU(T)
!
    if (ntemp .gt. 0) then
!
        if (ntemp .ne. nbresu) then
            call utmess('F', 'UTILITAI3_30')
        else
            if (nbresu .gt. 1) then
                dept = .false.
                if (ntpsi .gt. 1) dept = .true.
            else
                dept = .false.
            endif
        endif
!
    else
        dept = .false.
        ntpsi = 1
    endif
!
!     --- CREATION DES TABLES DE RESULTATS
!
    call tbcrsd(nomres, 'G')
!
    if (method(1:9) .eq. 'REGR_LINE') then
        call tbajpa(nomres, nbpars, nopars, typars)
    else
        call tbajpa(nomres, 2, nopars, typars)
    endif
    if (dept) then
        call tbajpa(nomres, nbpart, nopart, typart)
    else
        call tbajpa(nomres, 2, nopart(2), typart(2))
    endif
!
    call wkvect('&&OP0197.NOPARK', 'V V K16', ntpsi+2, inopa)
    call wkvect('&&OP0197.VALPAR', 'V V R', ntpsi+1, ivapa)
    call wkvect('&&OP0197.TYPARK', 'V V K8', ntpsi+2, itypa)
    zk16(inopa) = nopark(1)
    zk16(inopa+1) = nopark(2)
    zk8(itypa) = typark(1)
    zk8(itypa+1) = typark(2)
    do i = 1, ntpsi
        if (ntemp .eq. 0) then
            zk16(inopa+1+i) = nopark(3)
        else
            ibid = int(zr(itpsi-1+i))
            call codent(ibid, 'G', chtemp)
            zk16(inopa+1+i) = nopark(3)(1:7)//'_T:'//chtemp
        endif
        zk8(itypa+1+i) = typark(3)
    end do
    tapait = '&&PAR_IT'
    call tbcrsd(tapait, 'V')
    call tbajpa(tapait, ntpsi+2, zk16(inopa), zk8(itypa))
!
!     --- RECHERCHE DE LA RC WEIBULL POUR CHAQUE BASE RESULTAT
!
    nomrc = 'WEIBULL         '
    do iresu = 1, nbresu
!
        call jelira(zk8(ichmat-1+iresu)//'.CHAMP_MAT .VALE', 'LONMAX', nbmtcm)
        call wkvect('&&OP0197.L_NOM_MAT', 'V V K8', nbmtcm, anomm1)
        call chmrck(zk8(ichmat-1+iresu), nomrc, zk8(anomm1), nbmtrc)
        call rccome(zk8(anomm1), 'WEIBULL', iret, k11_ind_nomrc=k11)
        zk24(ikval-1+iresu) = zk8(anomm1)//k11//'.VALR'
        zk24(ikvak-1+iresu) = zk8(anomm1)//k11//'.VALK'
        call jedetr('&&OP0197.L_NOM_MAT')
        call jeveuo(zk24(ikvak-1+iresu), 'L', iweik)
        call jeveuo(zk24(ikval-1+iresu), 'L', iweir)
        call jelira(zk24(ikvak-1+iresu), 'LONMAX', imc)
!
        do i = 1, imc
            if (zk16(iweik + i-1) .eq. 'M       ') then
                mini=zr(iweir + i-1)
            endif
            if (zk16(iweik + i-1) .eq. 'VOLU_REFE') then
                vini=zr(iweir + i-1)
            endif
            if (zk16(iweik + i-1) .eq. 'SIGM_REFE') then
                zr(isigi-1+iresu)=zr(iweir + i-1)
            endif
        end do
!
        if (iresu .gt. 1) then
            if (mini .ne. minip) then
                call utmess('F', 'UTILITAI3_31')
            endif
            if (zr(isigi-1+iresu) .ne. zr(isigi-2+iresu)) then
                call utmess('F', 'UTILITAI3_32')
            endif
        endif
        minip = mini
!
    end do
    valrr (1) = mini
    valrr (2) = vini
    valrr (3) = zr(isigi)
    call utmess('I', 'UTILITAI6_55', nr=3, valr=valrr)
!
    call wkvect('&&OP0197.NOM_TABLPE', 'V V K16', nbresu, itabw)
    call wkvect('&&OP0197.NOM_TABLIN', 'V V K16', nbresu, itabr)
    call wkvect('&&OP0197.PROBW', 'V V R', nrupt, ix)
    call wkvect('&&OP0197.SIGMW', 'V V R', nrupt, iy)
    call wkvect('&&OP0197.PROTH', 'V V R', nrupt, ipth)
!
!     --- INITIALISATION DES PARAMETRES AUX VALEURS DE DEPART
!
    nbite = 0
    mk = mini
    mkp = mini
    do iresu = 1, ntpsi
        zr(isigk+iresu-1) = zr(isigi+iresu-1)
        zr(isigkp+iresu-1) = zr(isigi+iresu-1)
    end do
!
200 continue
!
!     --- NOUVELLE ITERATION DE RECALAGE
!
    nbite = nbite + 1
!
    if (impr) then
        write(ifm,*) '***************************'
        write(ifm,*) 'ITERATION DE RECALAGE NO ',nbite
        write(ifm,*) '***************************'
    endif
!
    mk = mkp
    do iresu = 1, ntpsi
        zr(isigk+iresu-1) = zr(isigkp+iresu-1)
    end do
!
    do iresu = 1, nbresu
!
!        CALCUL POUR CHAQUE RESU DES CONTRAINTES DE WEIBULL ---
!
!       --- SURCHARGE DES PARAMETRES DE LA RC WEIBULL
!        --- PAR SIGU(K) ET M(K)
!
        call jelira(zk8(ichmat-1+iresu)//'.CHAMP_MAT .VALE', 'LONMAX', nbmtcm)
        call wkvect('&&OP0197.L_NOM_MAT', 'V V K8', nbmtcm, anomm2)
        call chmrck(zk8(ichmat-1+iresu), nomrc, zk8(anomm2), nbmtrc)
        call jelira(zk24(ikvak-1+iresu), 'LONMAX', imc)
!
!       DUPLICATION DE LA SD CHAM_MATER POUR LA SURCHARGE
!
        call copisd(' ', 'V', zk8(ichmat-1+iresu), chcop1)
        call jeveuo(chcop1//'.CHAMP_MAT .VALE', 'E', vk8=vale)
        do i = 1, nbmtcm
            if (vale(i) .eq. zk8(anomm2)) then
                call copisd(' ', 'V', zk8(anomm2), chcop2)
                vale(i) = chcop2
            endif
        end do
!
        call jedetr('&&OP0197.L_NOM_MAT')
        call rccome(chcop2, 'WEIBULL', iret, k11_ind_nomrc=k11) 
        call jeveuo(chcop2//k11//'.VALR', 'E', iweir)
        call jeveuo(chcop2//k11//'.VALK', 'L', iweik)
!
        do i = 1, imc
            if (zk16(iweik + i-1) .eq. 'M       ') then
                zr(iweir + i-1) = mk
            endif
            if (zk16(iweik + i-1) .eq. 'VOLU_REFE') then
                zr(iweir + i-1) = vini
            endif
            if (zk16(iweik + i-1) .eq. 'SIGM_REFE') then
                zr(iweir + i-1) = zr(isigk+zi(itpre-1+iresu)-1)
            endif
        end do
!
        call jedetc('V', '.MATE_CODE', 9)
        call jedetc('V', '.CODI', 20)
        mate = ' '
        call rcmfmc(chcop1, mate)
!
!
!       DETERMINATION DU NOM DES 2 TABLES A CREER:
        noobj ='12345678.TB00000   .TBNP'
        call gnomsd(' ', noobj, 12, 16)
        zk16(itabw-1+iresu)=noobj(1:16)
        read(noobj(12:16),'(I5)') kk
        kk=kk+1
        call codent(kk, 'D0', noobj(12:16))
        zk16(itabr-1+iresu)=noobj(1:16)
!
        nchar = 0
        call wkvect('&&OP0197.CHARGES', 'V V K8', 1, jcha)
!
        if (impr) then
            write(ifm,*) '*******************'
            write(ifm,*) '**** RESULTAT NO ',iresu
            write(ifm,*) '*******************'
            write(ifm,*)&
     &    'ETAPE 1 > CALCUL DES SIGMA WEIBULL : APPEL PEWEIB'
        endif
!
!        --- CALCUL DES SIGMA_WEIBULL
!
        cara = '        '
        call peweib(zk16(itabw-1+iresu), zk8(imod-1+iresu), mate, cara, chcop1,&
                    0, 1, iresu, nomcmd)
        call jedetr('&&TE0331')
        call jedetr('&&OP0197.CHARGES')
!
!       --- INTERPOLATION SUR LA BASE DE CALCUL DES VALEURS
!       --- DES CONTRAINTES DE WEIBULL AUX INSTANTS DE RUPTURE
!
        call tbexve(zk16(itabw-1+iresu), 'SIGMA_WEIBULL', '&&OP0197.NOM_VECSIG', 'V', nbval,&
                    k8bid)
        call tbexve(zk16(itabw-1+iresu), 'PROBA_WEIBULL', '&&OP0197.NOM_VECPRO', 'V', nbval,&
                    k8bid)
        call tbexve(zk16(itabw-1+iresu), 'INST', '&&OP0197.NOM_INSSIG', 'V', nbval,&
                    k8bid)
        call jelira(jexnum(collec, iresu), 'LONUTI', nbins)
        call jeveuo(jexnum(collec, iresu), 'L', iinst)
        call jeveuo('&&OP0197.NOM_VECSIG', 'L', isig)
        call jeveuo('&&OP0197.NOM_VECPRO', 'L', ipro)
!
        if (impr) then
            write(ifm,*) 'TABLEAU DES SIGMA WEIBULL : '
            do it = 1, nbval
                write(ifm,*) 'SIGW(',it,') = ',zr(isig+it-1)
            end do
            write(ifm,*) 'TABLEAU DES PROBA WEIBULL : '
            do it = 1, nbval
                write(ifm,*) 'PRW(',it,') = ',zr(ipro+it-1)
            end do
        endif
!
        call jeveuo('&&OP0197.NOM_INSSIG', 'L', vr=nom_inssig)
!
        call tbcrsd(zk16(itabr-1+iresu), 'V')
        call tbajpa(zk16(itabr-1+iresu), nbparr, noparr, typarr)
!
        if (impr) write(ifm,*) 'ETAPE 2 > INTERPOLATION SIGMA WEIBULL'
        do it = 1, nbins
            if (impr) write(ifm, *) 'INTERPOLATION NO ', it, ' / TEMPS = ', zr(iinst+it-1)
            call interp(nom_inssig, zr(isig), nbval, zr(iinst+it-1), sigint,&
                        iseg)
            call interp(nom_inssig, zr(ipro), nbval, zr(iinst+it-1), proint,&
                        iseg)
            vali(1) = iresu
            valr(1) = zr(iinst+it-1)
            valr(2) = sigint
            valr(3) = proint
            call tbajli(zk16(itabr-1+iresu), nbparr, noparr, vali, valr,&
                        [c16b], k8bid, 0)
            if (impr) write(ifm,*) 'SIGMA WEIBULL :',sigint
        end do
!
        call jedetr('&&OP0197.NOM_VECPRO')
        call jedetr('&&OP0197.NOM_VECSIG')
        call jedetr('&&OP0197.NOM_INSSIG')
!
        call jedetc('V', chcop1, 1)
        call jedetc('V', chcop2, 1)
!
    end do
!
!  ---   FUSION DES TABLES DE CONTRAINTES DE WEIBULL POUR
!  ---   TOUTES LES BASES DE RESULATS
!
    call tbfutb(tabout, 'G', nbresu, zk16(itabr), ' ',&
                ' ', [ibid], [r8bid], [c16b], k8bid)
!
!  ---   TRI DE LA TABLE DES CONTRAINTES DE WEIBULL
!
    call tbtrtb(tabout, 'G', tabtri, 1, noparr(3),&
                'CR', 0.d0, 'ABSOLU  ')
    call tbexve(tabtri, 'NURES', '&&OP0197.NOM_NURES', 'V', nrupt,&
                k8bid)
    call jeveuo('&&OP0197.NOM_NURES', 'L', vi=nom_nures)
    call tbexve(tabtri, 'SIGMA_WEIBULL', '&&OP0197.NOM_VECSIG', 'V', nrupt,&
                k8bid)
    call jeveuo('&&OP0197.NOM_VECSIG', 'L', isig)
    call tbexve(tabtri, 'PROBA_WEIBULL', '&&OP0197.NOM_VECPRO', 'V', nrupt,&
                k8bid)
    call jeveuo('&&OP0197.NOM_VECPRO', 'L', ipro)
    call detrsd('TABLE', tabtri)
    call detrsd('TABLE', tabout)
!
    if (impr) then
        write(ifm,*) 'ETAPE 3 > FUSION ET TRI DES SIGMA WEIBULL'
        do it = 1, nrupt
            write(ifm,*) 'SIGW(',it,') = ',zr(isig+it-1)
        end do
        write(ifm,*) 'ETAPE 4 > OPTIMISATION DES PARAMETRES'
    endif
!
!  ---   CALCUL DE M ET SIGMA-U AU RANG K+1 PAR UNE
!        DES DEUX METHODES DE RECALAGE
!
    call optimw(method, nrupt, zr(ix), zr(iy), zr(ipth),&
                zr(isig), zi(irent), nom_nures, nbresu, calm,&
                cals, mk, zr(isigk), mkp, zr(isigkp),&
                impr, ifm, dept, zi(itpre), ntpsi)
!
!  ---   STOCKAGE DANS LA TABLE TABL_PARA_ITER
!        TABLE PARAMETRES MODELE A L'ITERATION K : K,M(K),SIGU(K)
!
    vali(1) = nbite
    zr(ivapa) = mkp
    do iresu = 1, ntpsi
        zr(ivapa+iresu) = zr(isigkp+iresu-1)
    end do
    call tbajli(tapait, ntpsi+2, zk16(inopa), vali, zr(ivapa),&
                [c16b], k8bid, 0)
!
!  ---   CALCUL CRITERE DE CONVERGENCE (MK,MK+1,SUK,SUK+1)
!
    maxcs = 0.d0
    do iresu = 1, ntpsi
        if (( abs( (zr(isigkp+iresu-1) - zr(isigk+iresu-1) )/ zr(isigk+ iresu-1) ) ) .gt.&
            maxcs) then
            maxcs = abs( (zr(isigkp+iresu-1) - zr(isigk+iresu-1)) / zr(isigk+iresu-1 ) )
        endif
    end do
!
    if ((abs((mkp-mk)/mk)) .le. epsi) calm = .true.
    if (maxcs .le. epsi) cals = .true.
!
!     SI SIGMA A CONVERGE ALORS QUE M RESTE A CALER
!     ALORS ON CONTINUE A CALER M ET SIGMA
!
    if ((recm.and.recs) .and. (cals.and.(.not.calm))) cals = .false.
!
    test = max((abs((mkp-mk)/mk)),maxcs)
    if (impr) then
        write(ifm,*) 'CONVERGENCE POUR M-K : ',abs((mkp-mk)/mk)
        write(ifm,*) 'CONVERGENCE POUR SIGMA-K : ',maxcs
        if (calm) write(ifm,*) ' --> LE PARAMETRE M EST CALE'
        if (cals) write(ifm,*) ' --> LE PARAMETRE SIGMA EST CALE'
    endif
!
!        STOCKAGE DANS LA TABLE TABL_PROBA_SIGW
!        TABLE PROBA-SIGMA EXPERIENCE/THEORIE : SIGW(I),PF(SIGW),PF(I)
!
    if (calm .and. cals) then
        do it = 1, nrupt
            valr(1) = zr(isig+it-1)
            valr(2) = zr(ipro+it-1)
            if (method(1:9) .eq. 'REGR_LINE') then
                valr(3) = zr(ipth+it-1)
                call tbajli(nomres, nbpars, nopars, [ibid], valr,&
                            [c16b], k8bid, 0)
            else
                call tbajli(nomres, 2, nopars, [ibid], valr,&
                            [c16b], k8bid, 0)
            endif
        end do
    endif
!
    call jedetr('&&OP0197.NOM_VECSIG')
    call jedetr('&&OP0197.NOM_VECPRO')
    call jedetr('&&OP0197.NOM_NURES')
!
!
!
!  ---   STOCKAGE DANS LA TABLE TABL_PARA_TEMP
!        TABLE PARAMETRES FONCTION TEMPERATURE : T,M,SIGU(T)
!
    if (calm .and. cals) then
        do iresu = 1, ntpsi
            valr(1) = zr(itpsi+iresu-1)
            valr(2) = mkp
            valr(3) = zr(isigkp+iresu-1)
            if (dept) then
                call tbajli(nomres, nbpart, nopart, [ibid], valr,&
                            [c16b], k8bid, 0)
            else
                call tbajli(nomres, 2, nopart(2), [ibid], valr(2),&
                            [c16b], k8bid, 0)
            endif
        end do
    endif
!
!     ---  BOUCLAGE SI NON CONVERGENCE
!          ET NOMBRE D'ITERATIONS MAX NON ATTEINT
!
    if (((.not.calm).or.(.not.cals)) .and. nbite .lt. nitmax) goto 200
    if (nbite .eq. nitmax) then
        call utmess('F', 'UTILITAI2_53')
    endif
!
    valii = nbite
    valrr (1) = test
    call utmess('I', 'UTILITAI6_56', si=valii, sr=valrr(1))
!
    call tbimpr(tapait, 'EXCEL', ifm, ntpsi+2, zk16(inopa),&
                0, '1PE12.5')
!
    call jedema()
end subroutine
