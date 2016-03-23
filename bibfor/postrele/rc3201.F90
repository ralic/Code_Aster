subroutine rc3201(lpmpb, lsn, lsnet, lfatig, lrocht,&
                  lieu, ig, iocs, seisme, yapass,&
                  mater, utot, utotenv,&
                  resuas, resuss, resuca, resucs,&
                  factus, fatiguenv, resufin)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/codent.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/limend.h"
#include "asterfort/rc32fp.h"
#include "asterfort/rc32fs.h"
#include "asterfort/rc32fu.h"
#include "asterfort/rc32ms.h"
#include "asterfort/rc32pm.h"
#include "asterfort/rc32rt.h"
#include "asterfort/rc32sa.h"
#include "asterfort/rc32sn.h"
#include "asterfort/rc32sp.h"
#include "asterfort/rcma02.h"
#include "asterfort/rcmo02.h"
#include "asterfort/rcvale.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/getvtx.h"
!
    integer :: ig, iocs
    real(kind=8) :: snmax, snemax, spmax, kemax, samax, utot, sm, sigpm, utotenv
    real(kind=8) :: resuas(*), resuss(*), resuca(*), resucs(*), factus(*)
    real(kind=8) :: pmmax, pbmax, pmbmax, snpq, snpqs, resufin(*)
    aster_logical :: lpmpb, lsn, lsnet, lfatig, lrocht, seisme, lbid
    aster_logical :: fatiguenv, yapass
    character(len=4) :: lieu
    character(len=8) :: mater
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DES AMPLITUDES DE CONTRAINTES
!     CALCUL DU FACTEUR D'USAGE
!     ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE
!
!     Soit 2 états stabilisés I et J appartenant aux situations P et Q
!
!     on calcule le SALT(I,J) = 0,5*(EC/E)*Ke(Sn(P,Q))*Sp(I,J)
!
!     avec Sn(P,Q) = Max( Sn(I,J) )
!          Sn(I,J) = Max( Max(Sn(I,J,ThP)), Max(Sn(I,J,ThQ)) )
!
!     avec Sp(I,J) = Max( Max(Sp(I,J,ThP)), Max(Sp(I,J,ThQ)) )
!
!     ------------------------------------------------------------------
!
    integer :: nbsigr, nbsig2, jnsg, is1, ioc1, is2, ioc2, inds, jcombi
    integer :: ifm, niv, i1, i2, ndim, nscy, ns
    integer :: nsitup, nsituq, indi, i, icas, icss, nbsitu, i4
    integer :: jmfu, nbthep, nbtheq, jmke, nb
    real(kind=8) :: ppi, ppj, pqi, pqj, saltij(2), salijs(2), ug, sn, sp(2), smm
    real(kind=8) :: sns, sps(2), spp, sqq(2), sqqs(2), mpi(12), mpj(12), mqi(12)
    real(kind=8) :: mqj(12), mse(12), sij0(12), matpi(7), matpj(7), matqi(7)
    real(kind=8) :: mat1(7), mat2(7), matqj(7), saltse(2), snet, snets, vale(2)
    real(kind=8) :: sp12ma(2), sp2(2), fuij(2), fuse(2), spmeps, sp2s(2), spps
    real(kind=8) :: spmes2(2), spmeqs(2), spmeca(2), spther(2), ugenv
    real(kind=8) :: spmecs(2), spthes(2), spthem, spmecm, simpij, kemeca, kether
    real(kind=8) :: kemecs, kethes, pm, pb, pmpb, pms, pbs, pmpbs, spmec2(2)
    real(kind=8) :: spmecp, spmecq(2), spthep(2)
    character(len=8) :: knumes, kbid, typeke
!CC
    integer :: icodre(1)
    aster_logical :: endur, cmax, meca
    integer :: nocc
    real(kind=8) :: nadm(1)
    integer, pointer :: situ_numero(:) => null()
    integer, pointer :: impr_situ(:) => null()
    real(kind=8), pointer :: matrice_fu_b(:) => null()
    real(kind=8), pointer :: matrice_fu_s(:) => null()
    integer, pointer :: nb_occurr(:) => null()
    real(kind=8), pointer :: situ_pres_a(:) => null()
    integer, pointer :: situ_nb_occur(:) => null()
    real(kind=8), pointer :: situ_pres_b(:) => null()
! DEB ------------------------------------------------------------------
    call jemarq()
!
    pmmax = 0.d0
    pbmax = 0.d0
    pmbmax = 0.d0
    sm = 0.d0
    snmax = 0.d0
    snemax = 0.d0
    spmax = 0.d0
    kemax = 0.d0
    spmecm = 0.d0
    spthem = 0.d0
    samax = 0.d0
    sigpm = 0.d0
!
    call getvtx(' ', 'TYPE_KE', scal=typeke, nbret=nb)
!
    call infniv(ifm, niv)
    call jeveuo('&&RC3200.SITU_NUMERO', 'L', vi=situ_numero)
    call jeveuo('&&RC3200.SITU_COMBINABLE', 'L', jcombi)
    call jeveuo('&&RC3200.SITU_PRES_A', 'L', vr=situ_pres_a)
    call jeveuo('&&RC3200.SITU_PRES_B', 'L', vr=situ_pres_b)
    call jeveuo('&&RC3200.SITU_NB_OCCUR', 'L', vi=situ_nb_occur)
!
    call jelira('&&RC3200.SITU_PRES_A', 'LONUTI', nbsitu)
    call jelira(jexnum('&&RC3200.LES_GROUPES', ig), 'LONUTI', nbsigr)
    call jeveuo(jexnum('&&RC3200.LES_GROUPES', ig), 'L', jnsg)
!
    do 11 is1 = 1, nbsigr
        do 12 is2 = 1, 10
            resuas(10*(is1-1)+is2) = r8vide()
            resuss(10*(is1-1)+is2) = r8vide()
 12     continue
 11 continue
!
    do 13 is1 = 1, 12
        sij0(is1) = 0.d0
 13 continue
!
    if (iocs .eq. 0) then
        nbsig2 = nbsigr
    else
        nbsig2 = nbsigr - 1
    endif
    ndim = nbsig2*nbsig2
    AS_ALLOCATE(vi=nb_occurr, size=nbsig2)
    AS_ALLOCATE(vi=impr_situ, size=nbsig2)
    call wkvect('&&RC3201.MATRICE_FU', 'V V R', ndim, jmfu)
    call wkvect('&&RC3201.MATRICE_KE', 'V V R', ndim, jmke)
    if (seisme) then
        AS_ALLOCATE(vr=matrice_fu_b, size=ndim)
        AS_ALLOCATE(vr=matrice_fu_s, size=ndim)
    endif
!
    sp12ma(1)=0.d0
    sp12ma(2)=0.d0
    ns = 0
    nscy = 0
    if (seisme) then
        do 16 is1 = 1, nbsigr
            ioc1 = zi(jnsg+is1-1)-nbsitu
            if (ioc1 .eq. iocs) goto 18
 16     continue
        call utmess('F', 'POSTRCCM_30')
 18     continue
        ns = situ_nb_occur(1+2*(nbsitu+iocs)-2)
        nscy = situ_nb_occur(1+2*(nbsitu+iocs)-1)
        ppi = 0.d0
        nsitup = situ_numero(1+zi(jnsg+is1-1)-1)
        call rcmo02('S', nsitup, mse)
        call rcma02('A', iocs, matpi)
        nsituq = 0
        pm = 0.d0
        pb = 0.d0
        pmpb = 0.d0
        sn = 0.d0
        snet = 0.d0
        sp(1) = 0.d0
        sp(2) = 0.d0
        if (lpmpb) then
            call rc32pm(lieu, seisme, ppi, sij0, mse,&
                        pm, pb, pmpb)
            resuas(10*(is1-1)+1) = pm
            resuas(10*(is1-1)+2) = pb
            resuas(10*(is1-1)+3) = pmpb
            pmmax = max ( pmmax , pm )
            pbmax = max ( pbmax , pb )
            pmbmax = max ( pmbmax , pmpb )
        endif
        if (lsn) then
            call rc32sn('SN_SITU', lieu, nsitup, ppi, sij0,&
                        nsituq, ppi, sij0, seisme, mse,&
                        sn)
            resuas(10*(is1-1)+4) = sn
            snmax = max( snmax , sn )
        endif
        if (lsn .and. lsnet) then
            call rc32sn('SN*_SITU', lieu, nsitup, ppi, sij0,&
                        nsituq, ppi, sij0, seisme, mse,&
                        snet)
            resuas(10*(is1-1)+5) = snet
            snemax = max( snemax , snet )
        endif
        if (lfatig) then
            call rc32sp('SP_SITU', lieu, nsitup, ppi, sij0,&
                        nsituq, ppi, sij0, seisme, mse,&
                        sp, spmeca)
            call rc32sa('SITU', mater, matpi, matpi, sn,&
                        sp, spmeca, kemeca,&
                        kether, saltse, sm, fuse)
            resuas(10*(is1-1)+6) = sp(1)
            resuas(10*(is1-1)+7) = kemeca
            resuas(10*(is1-1)+8) = kether
            resuas(10*(is1-1)+9) = saltse(1)
        endif
        if (niv .ge. 2) then
            if (lpmpb) then
                write (ifm,*) '  SEISME,   PM = ',pm
                write (ifm,*) '            PB = ',pb
                write (ifm,*) '          PMPB = ',pmpb
            endif
            if (lsn) write (ifm,*) '  SEISME,   SN = ',sn
            if (lsn .and. lsnet) write (ifm,*) '  SEISME,  SN* = ',snet
            if (lfatig) write (ifm,*) '  SEISME,   SP = ',sp(1)
            if (typeke .eq. 'KE_MIXTE' .and. lfatig) then
                write (ifm,*) '            SPMECA = ',spmeca(1)
                spthep(1)=max(0.0,sp(1)-spmeca(1))
                write (ifm,*) '            SPTHER = ',spthep(1)
                write (ifm,*) '            KEMECA = ',kemeca
                write (ifm,*) '            KETHER = ',kether
            endif
            if (lfatig) write (ifm,*) '          SALT = ',saltse(1)
            if (lfatig) write (ifm,*) '          FU = ',fuse
        endif
    else
        do 30 i = 1, 12
            mse(i) = 0.d0
 30     continue
    endif
!
! --- SITUATION P :
!
    icas = 0
    icss = 0
    i1 = 0
    do 20 is1 = 1, nbsigr
        ioc1 = zi(jnsg+is1-1)
        if (ioc1 .gt. nbsitu) goto 20
        if (.not.zl(jcombi+ioc1-1)) goto 20
!
        i1 = i1 + 1
        nb_occurr(i1) = situ_nb_occur(1+2*ioc1-2)
        impr_situ(i1) = situ_numero(ioc1)
!
        nsitup = situ_numero(ioc1)
        nsituq = 0
        ppi = situ_pres_a(ioc1)
        call rcmo02('A', nsitup, mpi)
        call rcma02('A', ioc1, matpi)
        ppj = situ_pres_b(ioc1)
        call rcmo02('B', nsitup, mpj)
        call rcma02('B', ioc1, matpj)
        pms = 0.d0
        pm = 0.d0
        pbs = 0.d0
        pb = 0.d0
        pmpbs = 0.d0
        pmpb = 0.d0
        sns = 0.d0
        snets = 0.d0
        sn = 0.d0
        snet = 0.d0
        spmeca(1) = 0.d0
        spmecs(1) = 0.d0
        spthes(1) = 0.d0
        spther(1) = 0.d0
        sps(1) = 0.d0
        sps(2) = 0.d0
        indi = nbsig2*(i1-1) + (i1-1)
!
        if (lpmpb) then
            call rc32pm(lieu, .false._1, ppi, mpi, mse,&
                        pm, pb, pmpb)
            call rc32pm(lieu, .false._1, ppj, mpj, mse,&
                        pm, pb, pmpb)
            resuss(10*(is1-1)+1) = pm
            resuss(10*(is1-1)+2) = pb
            resuss(10*(is1-1)+3) = pmpb
            pmmax = max ( pmmax , pm )
            pbmax = max ( pbmax , pb )
            pmbmax = max ( pmbmax , pmpb )
        endif
        if (lsn) then
            call rc32sn('SN_SITU', lieu, nsitup, ppi, mpi,&
                        nsituq, ppj, mpj, .false._1, mse,&
                        sn)
            resuss(10*(is1-1)+4) = sn
            snmax = max(snmax,sn)
        endif
        if (lsn .and. lsnet) then
            call rc32sn('SN*_SITU', lieu, nsitup, ppi, mpi,&
                        nsituq, ppj, mpj, .false._1, mse,&
                        snet)
            resuss(10*(is1-1)+5) = snet
            snemax = max(snemax,snet)
        endif
        if (lrocht) then
            call rc32rt(lieu, ppi, ppj, simpij)
            sigpm = max ( sigpm, simpij )
        endif
        if (seisme) then
            if (lpmpb) then
                call rc32pm(lieu, seisme, ppi, mpi, mse,&
                            pms, pbs, pmpbs)
                call rc32pm(lieu, seisme, ppj, mpj, mse,&
                            pms, pbs, pmpbs)
                resuas(10*(is1-1)+1) = pms
                resuas(10*(is1-1)+2) = pbs
                resuas(10*(is1-1)+3) = pmpbs
                pmmax = max ( pmmax , pms )
                pbmax = max ( pbmax , pbs )
                pmbmax = max ( pmbmax , pmpbs )
            endif
            if (lsn) then
                call rc32sn('SN_SITU', lieu, nsitup, ppi, mpi,&
                            nsituq, ppj, mpj, seisme, mse,&
                            sns)
                resuas(10*(is1-1)+4) = sns
                snmax = max(snmax,sns)
            endif
            if (lsn .and. lsnet) then
                call rc32sn('SN*_SITU', lieu, nsitup, ppi, mpi,&
                            nsituq, ppj, mpj, seisme, mse,&
                            snets)
                resuas(10*(is1-1)+5) = snets
                snemax = max(snemax,snets)
            endif
        endif
        if (niv .ge. 2) then
            if (lpmpb) write (ifm,112) nsitup, pm, pb, pmpb
            if (lsn) then
                if (lsnet) then
                    if (seisme) then
                        write (ifm,117) nsitup, snet, snets
                    else
                        write (ifm,116) nsitup, snet
                    endif
                endif
                if (seisme) then
                    write (ifm,115) nsitup, sn, sns
                else
                    write (ifm,114) nsitup, sn
                endif
            endif
        endif
!
        if ((lpmpb .or. lsn .or. lsnet) .and. .not.lfatig) goto 20
!
        nocc = situ_nb_occur(1+2*ioc1-2)
        call rc32sp('SP_SITU', lieu, nsitup, ppi, mpi,&
                    nsituq, ppj, mpj, .false._1, mse,&
                    sp, spmeca)
        spmecp = spmeca(1)
        call rc32sa('SITU', mater, matpi, matpj, sn,&
                    sp, spmeca, kemeca,&
                    kether, saltij, smm, fuij)
        resuss(10*(is1-1)+6) = sp(1)
        resuss(10*(is1-1)+7) = kemeca
        resuss(10*(is1-1)+8) = kether
        resuss(10*(is1-1)+9) = saltij(1)
        kemax = max( kemax , kemeca )
!
        zr(jmfu-1+indi+1) = fuij(1)
        if (typeke .eq. 'KE_MECA') then
            zr(jmke-1+indi+1) = kemeca
        else
            zr(jmke-1+indi+1) = (kemeca*spmeca(1)+kether*(sp(1)-spmeca(1)))/(sp(1))
        endif
        if (saltij(1) .gt. samax) then
            samax = saltij(1)
            sm = smm
        endif
        if (seisme) then
            matrice_fu_b(indi+1) = fuij(1)
            call rc32sp('SP_SITU', lieu, nsitup, ppi, mpi,&
                        nsituq, ppj, mpj, seisme, mse,&
                        sps, spmecs)
            call rc32sa('SITU', mater, matpi, matpj, sns,&
                        sps, spmecs, kemecs,&
                        kethes, salijs, smm, fuij)
            resuas(10*(is1-1)+6) = sps(1)
            resuas(10*(is1-1)+7) = kemecs
            resuas(10*(is1-1)+8) = kethes
            resuas(10*(is1-1)+9) = salijs(1)
            kemax = max( kemax , kemeca )
            matrice_fu_s(indi+1) = fuij(1)
            spmeps = spmecs(1)
        endif
!
        spmax = max(spmax,sps(1),sp(1))
        spmecm = max(spmecm,spmecs(1),spmeca(1))
        spthem = max(0.0,spmax-spmecm)
        if (niv .ge. 2) then
            write (ifm,118) nsitup, sp(1)
            if (seisme) write (ifm,119) nsitup, sps(1)
            if (typeke .eq. 'KE_MIXTE') then
                spthep(1)=max(0.0,sp(1)-spmeca(1))
                write (ifm,150) nsitup,spmeca(1),spthep(1),kemeca,&
                kether
                if (seisme) write (ifm,151) nsitup,spmecs(1),kemecs
            endif
            write (ifm,160) nsitup, saltij(1),zr(jmfu-1+indi+1)
            if (seisme) write (ifm,161) nsitup, salijs(1), matrice_fu_s(indi+1)
        endif
!
        call limend(mater, saltij(1), 'WOHLER', kbid, endur)
        if (endur) then
            ug=0.d0
        else
            call rcvale(mater, 'FATIGUE', 1, 'SIGM    ', saltij(1),&
                        1, 'WOHLER  ', nadm(1), icodre(1), 2)
            if (nadm(1) .lt. 0) then
                vale(1) = saltij(1)
                vale(2) = nadm(1)
                call utmess('A', 'POSTRCCM_32', nr=2, valr=vale)
            endif
            ug = dble( nocc ) / nadm(1)
        endif
        resuas(10*(is1-1)+10) = ug
        resuss(10*(is1-1)+10) = ug
!        IF (NIV.GE.2)  WRITE (IFM,1061) NSITUP, UG
!
! ----- SITUATION Q :
!       -------------
        i2 = i1
        do 10 is2 = is1 + 1, nbsigr
            ioc2 = zi(jnsg+is2-1)
            if (.not.zl(jcombi+ioc2-1)) goto 10
            if (ioc2 .gt. nbsitu) goto 10
            i2 = i2 + 1
!
            nsituq = situ_numero(ioc2)
!
            pqi = situ_pres_a(ioc2)
            call rcmo02('A', nsituq, mqi)
            call rcma02('A', ioc2, matqi)
!
            pqj = situ_pres_b(ioc2)
            call rcmo02('B', nsituq, mqj)
            call rcma02('B', ioc2, matqj)
!
            if (lrocht) then
                call rc32rt(lieu, pqi, pqj, simpij)
                sigpm = max ( sigpm, simpij )
                call rc32rt(lieu, ppi, pqi, simpij)
                sigpm = max ( sigpm, simpij )
                call rc32rt(lieu, ppi, pqj, simpij)
                sigpm = max ( sigpm, simpij )
                call rc32rt(lieu, ppj, pqi, simpij)
                sigpm = max ( sigpm, simpij )
                call rc32rt(lieu, ppj, pqj, simpij)
                sigpm = max ( sigpm, simpij )
            endif
!
! ------- CALCUL DU SN(P,Q), ON A 4 COMBINAISONS + SN(P,P) et SN(Q,Q)
            snpq = 0.d0
            call rc32sn('SN_SITU', lieu, nsitup, ppi, mpi,&
                        nsitup, ppj, mpj, .false._1, mse,&
                        snpq)
            call rc32sn('SN_COMB', lieu, nsitup, ppi, mpi,&
                        nsituq, pqi, mqi, .false._1, mse,&
                        snpq)
            call rc32sn('SN_COMB', lieu, nsitup, ppi, mpi,&
                        nsituq, pqj, mqj, .false._1, mse,&
                        snpq)
            call rc32sn('SN_COMB', lieu, nsitup, ppj, mpj,&
                        nsituq, pqj, mqj, .false._1, mse,&
                        snpq)
            call rc32sn('SN_COMB', lieu, nsitup, ppj, mpj,&
                        nsituq, pqi, mqi, .false._1, mse,&
                        snpq)
            call rc32sn('SN_SITU', lieu, nsituq, pqi, mqi,&
                        nsituq, pqj, mqj, .false._1, mse,&
                        snpq)
            icss = icss + 1
            resucs(icss) = snpq
            snmax = max(snmax,snpq)
            if (seisme) then
                snpqs = 0.d0
                call rc32sn('SN_SITU', lieu, nsitup, ppi, mpi,&
                            nsitup, ppj, mpj, seisme, mse,&
                            snpqs)
                call rc32sn('SN_COMB', lieu, nsitup, ppi, mpi,&
                            nsituq, pqi, mqi, seisme, mse,&
                            snpqs)
                call rc32sn('SN_COMB', lieu, nsitup, ppi, mpi,&
                            nsituq, pqj, mqj, seisme, mse,&
                            snpqs)
                call rc32sn('SN_COMB', lieu, nsitup, ppj, mpj,&
                            nsituq, pqj, mqj, seisme, mse,&
                            snpqs)
                call rc32sn('SN_COMB', lieu, nsitup, ppj, mpj,&
                            nsituq, pqi, mqi, seisme, mse,&
                            snpqs)
                call rc32sn('SN_SITU', lieu, nsituq, pqi, mqi,&
                            nsituq, pqj, mqj, seisme, mse,&
                            snpqs)
                icas = icas + 1
                resuca(icas) = snpqs
                snmax = max(snmax,snpqs)
            endif
            if (niv .ge. 2) write (ifm,110) nsitup,nsituq,snpq
            if ((niv.ge.2) .and. seisme) write (ifm,111) snpqs
            inds = nbsig2*(i1-1) + (i2-1)
            indi = nbsig2*(i2-1) + (i1-1)
!
! ------- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(P,Q)
!
! NOMBRE DE PAS DE TEMPS POUR DISTINGUER LE CAS MECANIQUE PUR
            knumes = 'S       '
            call codent(nsitup, 'D0', knumes(2:8))
            call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbthep)
            knumes = 'S       '
            call codent(nsituq, 'D0', knumes(2:8))
            call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbtheq)
!
            meca = .false.
            if ((nbthep+nbtheq) .eq. 0) meca = .true.
!
!
! - PREMIERE COMBINAISON : PI - QI
            call rc32sp('SP_COMB', lieu, nsitup, ppi, mpi,&
                        nsituq, pqi, mqi, .false._1, mse,&
                        sp12ma, spmeca)
!
            do 14 i4 = 1, 7
                mat1(i4) = matpi(i4)
                mat2(i4) = matqi(i4)
14         continue
!
            if (seisme) then
                call rc32sp('SP_COMB', lieu, nsitup, ppi, mpi,&
                            nsituq, pqi, mqi, seisme, mse,&
                            sps, spmecs)
            endif
!
! - DEUXIEME COMBINAISON : PI - QJ
            call rc32sp('SP_COMB', lieu, nsitup, ppi, mpi,&
                        nsituq, pqj, mqj, .false._1, mse,&
                        sp2, spmec2)
!
            if (typeke .eq. 'KE_MIXTE') then
                call rc32ms(.true._1, spmeca, spmec2, lbid)
            endif
!
            if (seisme) then
                call rc32sp('SP_COMB', lieu, nsitup, ppi, mpi,&
                            nsituq, pqj, mqj, seisme, mse,&
                            sp2s, spmes2)
                call rc32ms(meca, sps, sp2s, cmax)
                if (typeke .eq. 'KE_MIXTE') then
                    call rc32ms(.true._1, spmecs, spmes2, lbid)
                endif
            endif
!
            call rc32ms(meca, sp12ma, sp2, cmax)
!
            if (cmax) then
                do 120 i4 = 1, 7
                    mat1(i4) = matpi(i4)
                    mat2(i4) = matqj(i4)
120             continue
            endif
!
! - TROISIEME COMBINAISON : PJ - QI
            call rc32sp('SP_COMB', lieu, nsitup, ppj, mpj,&
                        nsituq, pqi, mqi, .false._1, mse,&
                        sp2, spmec2)
!
            if (typeke .eq. 'KE_MIXTE') then
                call rc32ms(.true._1, spmeca, spmec2, lbid)
            endif
!
            if (seisme) then
                call rc32sp('SP_COMB', lieu, nsitup, ppj, mpj,&
                            nsituq, pqi, mqi, seisme, mse,&
                            sp2s, spmes2)
                call rc32ms(meca, sps, sp2s, cmax)
                if (typeke .eq. 'KE_MIXTE') then
                    call rc32ms(.true._1, spmecs, spmes2, lbid)
                endif
            endif
!
            call rc32ms(meca, sp12ma, sp2, cmax)
!
            if (cmax) then
                do 15 i4 = 1, 7
                    mat1(i4) = matpj(i4)
                    mat2(i4) = matqi(i4)
15             continue
            endif
!
! - QUATRIEME COMBINAISON : PJ - QJ
            call rc32sp('SP_COMB', lieu, nsitup, ppj, mpj,&
                        nsituq, pqj, mqj, .false._1, mse,&
                        sp2, spmec2)
!
            if (typeke .eq. 'KE_MIXTE') then
                call rc32ms(.true._1, spmeca, spmec2, lbid)
            endif
!
            if (seisme) then
                call rc32sp('SP_COMB', lieu, nsitup, ppj, mpj,&
                            nsituq, pqj, mqj, seisme, mse,&
                            sp2s, spmes2)
                call rc32ms(meca, sps, sp2s, cmax)
                if (typeke .eq. 'KE_MIXTE') then
                    call rc32ms(.true._1, spmecs, spmes2, lbid)
                endif
            endif
!
            call rc32ms(meca, sp12ma, sp2, cmax)
!
            if (cmax) then
                do 17 i4 = 1, 7
                    mat1(i4) = matpj(i4)
                    mat2(i4) = matqi(i4)
17             continue
            endif
!
! -  CINQUIEME COMBINAISON : QI - QJ
            call rc32sp('SP_SITU', lieu, nsituq, pqi, mqi,&
                        0, pqj, mqj, .false._1, mse,&
                        sqq, spmecq)
            spp = resuss(10*(is1-1)+6)
            if (sqq(1) .ge. sp12ma(1)) then
                sp12ma(1) = sqq(1)
                sp12ma(2) = spp
                do 124 i4 = 1, 7
                    mat1(1) = matqi(1)
                    mat2(1) = matqj(1)
124             continue
            endif
!
            if (typeke .eq. 'KE_MIXTE') then
                if (spmecq(1) .ge. spmeca(1)) then
                    spmeca(1) = spmecq(1)
                    spmeca(2) = spmecp
                endif
            endif
!
            if (seisme) then
                call rc32sp('SP_SITU', lieu, nsituq, pqi, mqi,&
                            nsituq, pqj, mqj, seisme, mse,&
                            sqqs, spmeqs)
                if (sqqs(1) .ge. sps(1)) then
                    sps(1) = sqqs(1)
                    spps = resuas(10*(is1-1)+6)
                    sps(2) = spps
                endif
                if (typeke .eq. 'KE_MIXTE') then
                    if (spmeqs(1) .ge. spmecs(1)) then
                        spmecs(1) = spmeqs(1)
                        spmecs(2) = spmeps
                    endif
                endif
            endif
!
! - SIXIEME COMBINAISON : PI - PJ
            spp = resuss(10*(is1-1)+6)
            if (spp .ge. sp12ma(1)) then
                sp12ma(1) = spp
                sp12ma(2) = sqq(1)
                do 123 i4 = 1, 7
                    mat1(1) = matpi(1)
                    mat2(1) = matpj(1)
123             continue
            endif
!
            if (typeke .eq. 'KE_MIXTE') then
                if (spmecp .ge. spmeca(1)) then
                    spmeca(1) = spmecp
                    spmeca(2) = spmecq(1)
                endif
            endif
!
            if (seisme) then
                spps = resuas(10*(is1-1)+6)
                if (spps .ge. sps(1)) then
                    sps(1) = spps
                    sps(2) = sqqs(1)
                endif
                if (typeke .eq. 'KE_MIXTE') then
                    if (spmeps .ge. spmecs(1)) then
                        spmecs(1) = spmeps
                        spmecs(2) = spmeqs(1)
                    endif
                endif
            endif
!
!
! - CALCUL DE SALT ASSOCIE A SP1 ET SP2
            call rc32sa('COMB', mater, mat1, mat2, snpq,&
                        sp12ma, spmeca, kemeca,&
                        kether, saltij, smm, fuij)
            icss = icss + 1
            resucs(icss) = sp12ma(1)
            icss = icss + 1
            resucs(icss) = sp12ma(2)
            icss = icss + 1
            resucs(icss) = saltij(1)
            icss = icss + 1
            resucs(icss) = saltij(2)
            icss = icss + 1
            resucs(icss) = 0.d0
            icss = icss + 1
            resucs(icss) = 0.d0
            icss = icss + 1
            resucs(icss) = 0.d0
            icss = icss + 1
            resucs(icss) = 0.d0
            icss = icss + 1
            resucs(icss) = 0.d0
            icss = icss + 1
            resucs(icss) = 0.d0
            icss = icss + 1
            resucs(icss) = 0.d0
            kemax = max( kemax , kemeca )
            zr(jmfu-1+indi+1) = fuij(1)+fuij(2)
            zr(jmfu-1+inds+1) = fuij(1)+fuij(2)
            if (typeke .eq. 'KE_MECA') then
                zr(jmke-1+indi+1) = kemax
                zr(jmke-1+inds+1) = kemax
            else
                zr(jmke-1+indi+1) = (kemeca*spmeca(1)+kether*(sp12ma(1)-spmeca(1)))/(sp12ma(1))
                zr(jmke-1+inds+1) = (kemeca*spmeca(1)+kether*(sp12ma(1)-spmeca(1)))/(sp12ma(1))
            endif
            if (saltij(1) .gt. samax) then
                samax = saltij(1)
                sm = smm
            else if (saltij(2).gt.samax) then
                samax = saltij(2)
                sm = smm
            endif
            if (seisme) then
                matrice_fu_b(inds+1) = fuij(1)+fuij(2)
                matrice_fu_b(indi+1) = fuij(1)+fuij(2)
! ON PREND SPTHES = SPTHER
                call rc32sa('COMB', mater, mat1, mat2, snpqs,&
                            sps, spmecs, kemecs,&
                            kethes, salijs, smm, fuij)
                icas = icas + 1
                resuca(icas) = sps(1)
                icas = icas + 1
                resuca(icas) = sps(2)
                icas = icas + 1
                resuca(icas) = salijs(1)
                icas = icas + 1
                resuca(icas) = salijs(2)
                icas = icas + 1
                resuca(icas) = 0.d0
                icas = icas + 1
                resuca(icas) = 0.d0
                icas = icas + 1
                resuca(icas) = 0.d0
                icas = icas + 1
                resuca(icas) = 0.d0
                icas = icas + 1
                resuca(icas) = 0.d0
                icas = icas + 1
                resuca(icas) = 0.d0
                icas = icas + 1
                resuca(icas) = 0.d0
                kemax = max( kemax , kemeca )
                matrice_fu_s(inds+1) = fuij(1)+fuij(2)
                matrice_fu_s(indi+1) = fuij(1)+fuij(2)
            endif
            spmax = max(spmax,sps(1),sp12ma(1),sps(2),sp12ma(2))
            spmecm = max(spmecm,spmecs(1),spmeca(1))
            spthem = max(0.0,spmax-spmecm)
            if (niv .ge. 2) then
                write (ifm,121) sp12ma(1), sp12ma(2)
                if (seisme) write (ifm,122) sps(1), sps(2)
                if (typeke .eq. 'KE_MIXTE') then
                    write (ifm,131) spmeca(1),spmeca(2),kemeca
                    spther(1)=max(0.0,sp12ma(1)-spmeca(1))
                    spther(2)=max(0.0,sp12ma(2)-spmeca(2))
                    write (ifm,132) spther(1),spther(2),kether
                    if (seisme) write (ifm,133) spmecs(1),spmecs(2), kemecs
                endif
                write (ifm,231) saltij(1), saltij(2)
                if (seisme) write (ifm,232) salijs(1), salijs(2)
                write (ifm,331) fuij(1), fuij(2)
            endif
 10     continue
 20 continue
!
! --- CALCUL DU FACTEUR D'USAGE
!
    if (lfatig) then
        if (seisme) then
            call rc32fs(nbsig2, nb_occurr, impr_situ, matrice_fu_s, matrice_fu_b,&
                        fuse(1), ns, nscy, ug)
            utot = utot + ug
        endif
        if (.not. yapass) then
            call rc32fu(nbsig2, nb_occurr, impr_situ, zr(jmfu), zr(jmke),&
                        lieu, ug, factus, ugenv, fatiguenv)
        else
            call rc32fp(nbsig2, nb_occurr, impr_situ, zi(jnsg), zr(jmfu),zr(jmke),&
                        lieu, ug, factus, ugenv, fatiguenv)
        endif
        utot = utot + ug
        utotenv = utotenv + ugenv
    endif
!
    resufin(1) = pmmax
    resufin(2) = pbmax
    resufin(3) = pmbmax
    resufin(4) = sm
    resufin(5) = snmax
    resufin(6) = snemax
    resufin(7) = spmax
    resufin(8) = kemax
    resufin(9) = samax
    resufin(10) = sigpm
    resufin(11) = spthem
!
    if (seisme) then
        AS_DEALLOCATE(vr=matrice_fu_b)
        AS_DEALLOCATE(vr=matrice_fu_s)
    endif
    call jedetr('&&RC3201.MATRICE_FU')
    call jedetr('&&RC3201.MATRICE_KE')
    AS_DEALLOCATE(vi=nb_occurr)
    AS_DEALLOCATE(vi=impr_situ)
!
!
    112 format (1p,' SITUATION ',i4,' PM =',e12.5,&
     &                            ' PB =',e12.5,' PMPB =',e12.5)
    114 format (1p,' SITUATION ',i4,' SN =',e12.5 )
    115 format (1p,' SITUATION ',i4,' SN =',e12.5 ,&
     &    ' SN AVEC SEISME =',e12.5 )
    116 format (1p,' SITUATION ',i4,' SN* =',e12.5 )
    117 format (1p,' SITUATION ',i4,' SN* =',e12.5,&
     &    ' SN* AVEC SEISME =',e12.5 )
    118 format (1p,' SITUATION ',i4,' SP =',e12.5)
    119 format (1p,' SITUATION ',i4,' AVEC SEISME : SP =',e12.5)
    150 format (1p,' SITUATION ',i4,' SPMECA=',e12.5,' SPTHER=',e12.5,&
     &                                ' KEMECA=',e12.5,' KETHER=',e12.5)
    151 format (1p,' SITUATION ',i4,' AVEC SEISME : SPMECA =',e12.5,&
     &                                ' KEMECA=',e12.5)
    160 format (1p,' SITUATION ',i4,' SALT =',e12.5,' FACT_USAGE =',e12.5)
    161 format (1p,' SITUATION ',i4,' AVEC SEISME : SALT =',e12.5,&
     &                                   ' FACT_USAGE =',e12.5)
!
    110 format (1p,' COMBINAISON DES SITUATIONS ',i4,3x,i4,'  SN =',e12.5)
    111 format (1p,41x,'AVEC SEISME : SN =',e12.5)
    121 format (1p,41x,'SP1 =',e12.5,2x,'SP2 =',e12.5)
    122 format (1p,41x,'AVEC SEISME : SP1 =',e12.5,2x,'SP2 =',e12.5)
    131 format (1p,41x,'SPMECA1=',e12.5,' SPMECA2=',e12.5,&
     &                                ' KEMECA=',e12.5)
    132 format (1p,41x,'SPTHER1=',e12.5,' SPTHER2=',e12.5,&
     &                                ' KETHER=',e12.5)
    133 format (1p,41x,'AVEC SEISME : SPMECA1=',e12.5,' SPMECA2=',e12.5,&
     &                                ' KEMECA=',e12.5)
    231 format (1p,41x,'SALT1 =',e12.5,2x,'SALT2 =',e12.5)
    232 format (1p,41x,'AVEC SEISME : SALT1 =',e12.5,2x,'SALT2 =',e12.5)
    331 format (1p,41x,'FU1 =',e12.5,2x,'FU2 =',e12.5)
    call jedema()
end subroutine
