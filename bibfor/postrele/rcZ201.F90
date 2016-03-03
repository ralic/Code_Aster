subroutine rcZ201(ig, lsn, lther, lfat, lefat,&
                  yapass, seisme, iocs, mater, lieu,&
                  utot, utotenv, resuas, resuss,&
                  resuca, resucs, factus, resumax)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/infniv.h"
#include "asterfort/getvtx.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnum.h"
#include "asterc/r8vide.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/rcmo02.h"
#include "asterfort/rcZ2sn.h"
#include "asterfort/rcZ2sp.h"
#include "asterfort/rcma02.h"
#include "asterfort/rc32sa.h"
#include "asterfort/rcZ2rt.h"

#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jexnom.h"
#include "asterfort/limend.h"
#include "asterfort/rc32fp.h"
#include "asterfort/rc32fs.h"
#include "asterfort/rc32fu.h"
#include "asterfort/rc32ms.h"
#include "asterfort/rcvale.h"
#include "asterfort/utmess.h"
#include "asterfort/as_deallocate.h"
!
    integer :: ig, iocs
    aster_logical :: lsn, lther, lfat, lefat, yapass, seisme
    character(len=8) :: mater
    character(len=4) :: lieu
    real(kind=8) :: utot, utotenv, resuas(*), resuss(*), resuca(*)
    real(kind=8) :: resucs(*), factus(*), resumax(*)
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200
!     CALCUL DES AMPLITUDES DE CONTRAINTES ET DU FACTEUR D'USAGE
!     ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE
!                IN  : IG,     NUMERO DU GROUPE
!                IN  : LSN,    OPTION SN
!                IN  : LTHER,  OPTION SN* ET ROCHET THERMIQUE
!                IN  : LFAT,   OPTION FATIGUE
!                IN  : LEFAT,  OPTION FATIGUE ENVIRONNEMENTALE
!                IN  : YAPASS, SI SITUATION DE PASSAGE
!                IN  : SEISME, PRISE EN COMPTE DU SEISME
!                IN  : IOCS,   NUMERO DE LA SITUATION DE SEISME
!                IN  : MATER,  CHAMP MATERIAU
!                IN  : LIEU,   ORIG ou EXTR
!                OUT : UTOT,   FACTEUR d'USAGE CUMULE
!                OUT : UTOTENV,FACTEUR d'USAGE avec ENVIRONNEMENT
!                OUT : RESUAS, RESULTATS SITU AVEC SEISME
!                OUT : RESUSS, RESULTATS SITU SANS SEISME
!                OUT : RESUCA, RESULTATS COMB AVEC SEISME
!                OUT : RESUCS, RESULTATS COMB SANS SEISME
!                OUT : FACTUS, RESULTATS FACTEURS USAGE
!                OUT : RESUMAX,RESULTATS TYPE MAXI
!     ------------------------------------------------------------------
!
    integer :: ifm, niv, nb, jcombi, nbsitu, nbsigr, jnsg, is1, is2
    real(kind=8) :: sm, snmax, snemax, spmax, kemax, spmecm, spthem
    real(kind=8) :: samax, sigpm, sij0(12), sp12ma(2), ppi, mse(12)
    character(len=8) :: typeke
    integer, pointer :: situ_numero(:) => null()
    real(kind=8), pointer :: situ_pres_a(:) => null()
    real(kind=8), pointer :: situ_pres_b(:) => null()
    integer, pointer :: situ_nb_occur(:) => null()
    integer :: is3, ns, nscy, nbsig2, ndim, jmfu, jmke, nsitup
    integer, pointer :: nb_occurr(:) => null()
    integer, pointer :: impr_situ(:) => null()
    real(kind=8), pointer :: matrice_fu_b(:) => null()
    real(kind=8), pointer :: matrice_fu_s(:) => null()
    integer :: nsituq, i, icas, icss, i1, ioc1, nocc
    real(kind=8) :: sn, snet, sp(2), spmeca(2), matpi(7), kemeca, kether
    real(kind=8) :: saltse(2), fuse(2), spthep(2), mpi(12), ppj, mpj(12)
    real(kind=8) :: sns, snets, spmecp, matpj(7)

    integer :: ioc2, inds, i2, indi, i4, nbthep, nbtheq
    real(kind=8) :: pqi, pqj, saltij(2), salijs(2), ug, smm
    real(kind=8) :: sps(2), spp, sqq(2), sqqs(2), mqi(12)
    real(kind=8) :: mqj(12), matqi(7)
    real(kind=8) :: mat1(7), mat2(7), matqj(7), vale(2)
    real(kind=8) :: sp2(2), fuij(2), spmeps, sp2s(2), spps
    real(kind=8) :: spmes2(2), spmeqs(2), spther(2)
    real(kind=8) :: spmecs(2), spthes(2), simpij
    real(kind=8) :: kemecs, kethes, spmec2(2), ugenv
    real(kind=8) :: spmecq(2)
    character(len=8) :: knumes, kbid
    real(kind=8) :: snpq, snpqs
    aster_logical :: lbid
    integer :: icodre(1)
    aster_logical :: endur, cmax, meca
    real(kind=8) :: nadm(1)
!
! DEB ------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
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
    call jeveuo('&&RC3200.SITU_NUMERO', 'L', vi=situ_numero)
    call jeveuo('&&RC3200.SITU_COMBINABLE', 'L', jcombi)
    call jeveuo('&&RC3200.SITU_PRES_A', 'L', vr=situ_pres_a)
    call jeveuo('&&RC3200.SITU_PRES_B', 'L', vr=situ_pres_b)
    call jeveuo('&&RC3200.SITU_NB_OCCUR', 'L', vi=situ_nb_occur)
    call jelira('&&RC3200.SITU_PRES_A', 'LONUTI', nbsitu)
    call jelira(jexnum('&&RC3200.LES_GROUPES', ig), 'LONUTI', nbsigr)
    call jeveuo(jexnum('&&RC3200.LES_GROUPES', ig), 'L', jnsg)
!
    do 11 is1 = 1, nbsigr
        do 12 is2 = 1, 10
            resuas(10*(is1-1)+is2) = r8vide()
            resuss(10*(is1-1)+is2) = r8vide()
 12     continue
 11 end do
!
    do 13 is3 = 1, 12
        sij0(is3) = 0.d0
 13 end do
!
    if (iocs .eq. 0) then
        nbsig2 = nbsigr
    else
        nbsig2 = nbsigr - 1
    endif
    ndim = nbsig2*nbsig2
!
    AS_ALLOCATE(vi=nb_occurr, size=nbsig2)
    AS_ALLOCATE(vi=impr_situ, size=nbsig2)
!
    call wkvect('&&RC3201.MATRICE_FU', 'V V R', ndim, jmfu)
    call wkvect('&&RC3201.MATRICE_KE', 'V V R', ndim, jmke)
    if (seisme) then
        AS_ALLOCATE(vr=matrice_fu_b, size=ndim)
        AS_ALLOCATE(vr=matrice_fu_s, size=ndim)
    endif
!
! ----------------------------------------------------------------------
!               ON TRAITE LE SEISME SEUL
! ----------------------------------------------------------------------
    if (seisme) then
        do 16 is1 = 1, nbsigr
            ioc1 = zi(jnsg+is1-1)-nbsitu
            if (ioc1 .eq. iocs) goto 18
 16     continue
        call utmess('F', 'POSTRCCM_30')
 18     continue
        ns = situ_nb_occur(1+2*(nbsitu+iocs)-2)
        nscy = situ_nb_occur(1+2*(nbsitu+iocs)-1)
        nsitup = situ_numero(1+zi(jnsg+is1-1)-1)
        ppi = 0.d0
        nsituq = 0
        call rcmo02('S', nsitup, mse)
        sn = 0.d0
        if (lsn) then
            call rcZ2sn('SN_SITU', lieu, nsitup, ppi, sij0,&
                        nsituq, ppi, sij0, seisme, mse, sn)
            resuas(10*(is1-1)+4) = sn
            snmax = max( snmax , sn )
        endif
!
        snet = 0.d0
        if (lsn .and. lther) then
            call rcZ2sn('SN*_SITU', lieu, nsitup, ppi, sij0,&
                        nsituq, ppi, sij0, seisme, mse, snet)
            resuas(10*(is1-1)+5) = snet
            snemax = max( snemax , snet )
        endif
!
        sp(1) = 0.d0
        sp(2) = 0.d0
        spmeca(1) = 0.d0
        spmeca(2) = 0.d0
        if (lfat) then
            call rcZ2sp('SP_SITU', lieu, nsitup, ppi, sij0,&
                        nsituq, ppi, sij0, seisme, mse, sn,&
                        sp, spmeca)
            call rcma02('A', iocs, matpi)
            call rc32sa('SITU', mater, matpi, matpi, sn,&
                        sp, spmeca, kemeca,&
                        kether, saltse, sm, fuse)
            resuas(10*(is1-1)+6) = sp(1)
            resuas(10*(is1-1)+7) = kemeca
            resuas(10*(is1-1)+8) = kether
            resuas(10*(is1-1)+9) = saltse(1)
        endif
!
        if (niv .ge. 2) then
            if (lsn) write (ifm,*) '  SEISME,   SN = ',sn
            if (lsn .and. lther) write (ifm,*) '  SEISME,  SN* = ',snet
            if (lfat) write (ifm,*) '  SEISME,   SP = ',sp(1)
            if (typeke .eq. 'KE_MIXTE' .and. lfat) then
                spthep(1)=max(0.0,sp(1)-spmeca(1))
                write (ifm,*) '            SPMECA = ',spmeca(1)
                write (ifm,*) '            SPTHER = ',spthep
                write (ifm,*) '            KEMECA = ',kemeca
                write (ifm,*) '            KETHER = ',kether
            endif
            if (lfat) write (ifm,*) '          SALT = ',saltse(1)
            if (lfat) write (ifm,*) '          FU = ',fuse
        endif
    else
        do 30 i = 1, 12
            mse(i) = 0.d0
 30     continue
    endif
!
! ----------------------------------------------------------------------
!               ON TRAITE LA SITUATION P SEULE
! ----------------------------------------------------------------------
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
        ppi = situ_pres_a(ioc1)
        call rcmo02('A', nsitup, mpi)
        nsituq = 0
        ppj = situ_pres_b(ioc1)
        call rcmo02('B', nsitup, mpj)
!
        sn = 0.d0
        snet = 0.d0
        sns = 0.d0
        snets = 0.d0
        sp(1) = 0.d0
        sp(2) = 0.d0
        spmeca(1) = 0.d0
        spmecs(1) = 0.d0
        spthes(1) = 0.d0
        spther(1) = 0.d0
        sps(1) = 0.d0
        sps(2) = 0.d0
!
        if (lsn) then
            call rcZ2sn('SN_SITU', lieu, nsitup, ppi, mpi,&
                        nsituq, ppj, mpj, .false._1, mse, sn)
            resuss(10*(is1-1)+4) = sn
            snmax = max(snmax,sn)
        endif
        if (lsn .and. lther) then
            call rcZ2sn('SN*_SITU', lieu, nsitup, ppi, mpi,&
                        nsituq, ppj, mpj, .false._1, mse, snet)
            resuss(10*(is1-1)+5) = snet
            snemax = max(snemax,snet)
        endif
        if (lther) then
            call rcZ2rt(ppi, ppj, simpij)
            sigpm = max ( sigpm, simpij )
        endif
!
        if (seisme) then
            if (lsn) then
                call rcZ2sn('SN_SITU', lieu, nsitup, ppi, mpi,&
                            nsituq, ppj, mpj, seisme, mse,&
                            sns)
                resuas(10*(is1-1)+4) = sns
                snmax = max(snmax,sns)
            endif
            if (lsn .and. lther) then
                call rcZ2sn('SN*_SITU', lieu, nsitup, ppi, mpi,&
                            nsituq, ppj, mpj, seisme, mse,&
                            snets)
                resuas(10*(is1-1)+5) = snets
                snemax = max(snemax,snets)
            endif
        endif
!
        if (niv .ge. 2) then
            if (lsn) then
                if (lther) then
                    if (seisme) then
                        write (ifm,1017) nsitup, snet, snets
                    else
                        write (ifm,1016) nsitup, snet
                    endif
                endif
                if (seisme) then
                    write (ifm,1015) nsitup, sn, sns
                else
                    write (ifm,1014) nsitup, sn
                endif
            endif
        endif
!
        if ((lsn .or. lther) .and. .not.lfat) goto 20
!
        nocc = situ_nb_occur(1+2*ioc1-2)
!

        call rcZ2sp('SP_SITU', lieu, nsitup, ppi, mpi, nsituq,&
                    ppj, mpj, .false._1, mse, sn, sp, spmeca)
        spmecp = spmeca(1)
!
        call rcma02('A', ioc1, matpi)
        call rcma02('B', ioc1, matpj)
!
        call rc32sa('SITU', mater, matpi, matpj, sn, sp,&
                    spmeca, kemeca, kether, saltij, smm, fuij)
        resuss(10*(is1-1)+6) = sp(1)
        resuss(10*(is1-1)+7) = kemeca
        resuss(10*(is1-1)+8) = kether
        resuss(10*(is1-1)+9) = saltij(1)
        kemax = max( kemax , kemeca )
!
        indi = nbsig2*(i1-1) + (i1-1)
        zr(jmfu-1+indi+1) = fuij(1)
        if (typeke .eq. 'KE_MECA') then
            zr(jmke-1+indi+1) = kemeca
        else
            zr(jmke-1+indi+1) = (kemeca*spmeca(1)+kether*(sp(1)-spmeca(1)))/(sp(1))
        endif
!
        if (saltij(1) .gt. samax) then
            samax = saltij(1)
            sm = smm
        endif
!
        if (seisme) then
            matrice_fu_b(indi+1) = fuij(1)
            call rcZ2sp('SP_SITU', lieu, nsitup, ppi, mpi, nsituq,&
                        ppj, mpj, seisme, mse, sns, sps, spmecs)
            call rc32sa('SITU', mater, matpi, matpj, sns, sps,&
                        spmecs, kemecs, kethes, salijs, smm, fuij)
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
            write (ifm,1018) nsitup, sp(1)
            if (seisme) write (ifm,1019) nsitup, sps(1)
            if (typeke .eq. 'KE_MIXTE') then
                spthep(1)=max(0.0,sp(1)-spmeca(1))
                write (ifm,1050) nsitup,spmeca(1),spthep(1),kemeca,&
                kether
                if (seisme) write (ifm,1051) nsitup,spmecs(1),kemecs
            endif
            write (ifm,1060) nsitup, saltij(1),zr(jmfu-1+indi+1)
            if (seisme) write (ifm,1061) nsitup, salijs(1), matrice_fu_s(indi+1)
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
            if (lther) then
                call rcZ2rt(pqi, pqj, simpij)
                sigpm = max ( sigpm, simpij )
                call rcZ2rt(ppi, pqi, simpij)
                sigpm = max ( sigpm, simpij )
                call rcZ2rt(ppi, pqj, simpij)
                sigpm = max ( sigpm, simpij )
                call rcZ2rt(ppj, pqi, simpij)
                sigpm = max ( sigpm, simpij )
                call rcZ2rt(ppj, pqj, simpij)
                sigpm = max ( sigpm, simpij )
            endif
!
! ------- CALCUL DU SN(P,Q), ON A 4 COMBINAISONS + SN(P,P) et SN(Q,Q)
            snpq = 0.d0
            call rcZ2sn('SN_SITU', lieu, nsitup, ppi, mpi,&
                        nsitup, ppj, mpj, .false._1, mse,&
                        snpq)
            call rcZ2sn('SN_COMB', lieu, nsitup, ppi, mpi,&
                        nsituq, pqi, mqi, .false._1, mse,&
                        snpq)
            call rcZ2sn('SN_COMB', lieu, nsitup, ppi, mpi,&
                        nsituq, pqj, mqj, .false._1, mse,&
                        snpq)
            call rcZ2sn('SN_COMB', lieu, nsitup, ppj, mpj,&
                        nsituq, pqj, mqj, .false._1, mse,&
                        snpq)
            call rcZ2sn('SN_COMB', lieu, nsitup, ppj, mpj,&
                        nsituq, pqi, mqi, .false._1, mse,&
                        snpq)
            call rcZ2sn('SN_SITU', lieu, nsituq, pqi, mqi,&
                        nsituq, pqj, mqj, .false._1, mse,&
                        snpq)
            icss = icss + 1
            resucs(icss) = snpq
            snmax = max(snmax,snpq)
            if (seisme) then
                snpqs = 0.d0
                call rcZ2sn('SN_SITU', lieu, nsitup, ppi, mpi,&
                            nsitup, ppj, mpj, seisme, mse,&
                            snpqs)
                call rcZ2sn('SN_COMB', lieu, nsitup, ppi, mpi,&
                            nsituq, pqi, mqi, seisme, mse,&
                            snpqs)
                call rcZ2sn('SN_COMB', lieu, nsitup, ppi, mpi,&
                            nsituq, pqj, mqj, seisme, mse,&
                            snpqs)
                call rcZ2sn('SN_COMB', lieu, nsitup, ppj, mpj,&
                            nsituq, pqj, mqj, seisme, mse,&
                            snpqs)
                call rcZ2sn('SN_COMB', lieu, nsitup, ppj, mpj,&
                            nsituq, pqi, mqi, seisme, mse,&
                            snpqs)
                call rcZ2sn('SN_SITU', lieu, nsituq, pqi, mqi,&
                            nsituq, pqj, mqj, seisme, mse,&
                            snpqs)
                icas = icas + 1
                resuca(icas) = snpqs
                snmax = max(snmax,snpqs)
            endif
            if (niv .ge. 2) write (ifm,1110) nsitup,nsituq,snpq
            if ((niv.ge.2) .and. seisme) write (ifm,1111) snpqs
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
            call rcZ2sp('SP_COMB', lieu, nsitup, ppi, mpi,&
                        nsituq, pqi, mqi, .false._1, mse, snpq,&
                        sp12ma, spmeca)
!
            do 119 i4 = 1, 7
                mat1(i4) = matpi(i4)
                mat2(i4) = matqi(i4)
119         continue
!
            if (seisme) then
                call rcZ2sp('SP_COMB', lieu, nsitup, ppi, mpi,&
                            nsituq, pqi, mqi, seisme, mse, snpqs,&
                            sps, spmecs)
            endif
!
! - DEUXIEME COMBINAISON : PI - QJ
            call rcZ2sp('SP_COMB', lieu, nsitup, ppi, mpi,&
                        nsituq, pqj, mqj, .false._1, mse, snpq,&
                        sp2, spmec2)
!
            if (typeke .eq. 'KE_MIXTE') then
                call rc32ms(.true._1, spmeca, spmec2, lbid)
            endif
!
            if (seisme) then
                call rcZ2sp('SP_COMB', lieu, nsitup, ppi, mpi,&
                            nsituq, pqj, mqj, seisme, mse, snpqs,&
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
            call rcZ2sp('SP_COMB', lieu, nsitup, ppj, mpj,&
                        nsituq, pqi, mqi, .false._1, mse, snpq,&
                        sp2, spmec2)
!
            if (typeke .eq. 'KE_MIXTE') then
                call rc32ms(.true._1, spmeca, spmec2, lbid)
            endif
!
            if (seisme) then
                call rcZ2sp('SP_COMB', lieu, nsitup, ppj, mpj,&
                            nsituq, pqi, mqi, seisme, mse, snpqs,&
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
                do 121 i4 = 1, 7
                    mat1(i4) = matpj(i4)
                    mat2(i4) = matqi(i4)
121             continue
            endif
!
! - QUATRIEME COMBINAISON : PJ - QJ
            call rcZ2sp('SP_COMB', lieu, nsitup, ppj, mpj,&
                        nsituq, pqj, mqj, .false._1, mse, snpq,&
                        sp2, spmec2)
!
            if (typeke .eq. 'KE_MIXTE') then
                call rc32ms(.true._1, spmeca, spmec2, lbid)
            endif
!
            if (seisme) then
                call rcZ2sp('SP_COMB', lieu, nsitup, ppj, mpj,&
                            nsituq, pqj, mqj, seisme, mse, snpqs,&
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
                do 122 i4 = 1, 7
                    mat1(i4) = matpj(i4)
                    mat2(i4) = matqi(i4)
122             continue
            endif
!
! -  CINQUIEME COMBINAISON : QI - QJ
            call rcZ2sp('SP_SITU', lieu, nsituq, pqi, mqi,&
                        0, pqj, mqj, .false._1, mse, snpq,&
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
                call rcZ2sp('SP_SITU', lieu, nsituq, pqi, mqi,&
                            nsituq, pqj, mqj, seisme, mse, snpqs,&
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
                kemax = max( kemax , kemeca )
                matrice_fu_s(inds+1) = fuij(1)+fuij(2)
                matrice_fu_s(indi+1) = fuij(1)+fuij(2)
            endif
            spmax = max(spmax,sps(1),sp12ma(1),sps(2),sp12ma(2))
            spmecm = max(spmecm,spmecs(1),spmeca(1))
            spthem = max(0.0,spmax-spmecm)
            if (niv .ge. 2) then
                write (ifm,1121) sp12ma(1), sp12ma(2)
                if (seisme) write (ifm,1122) sps(1), sps(2)
                if (typeke .eq. 'KE_MIXTE') then
                    write (ifm,1131) spmeca(1),spmeca(2),kemeca
                    spther(1)=max(0.0,sp12ma(1)-spmeca(1))
                    spther(2)=max(0.0,sp12ma(2)-spmeca(2))
                    write (ifm,1132) spther(1),spther(2),kether
                    if (seisme) write (ifm,1133) spmecs(1),spmecs(2), kemecs
                endif
                write (ifm,1231) saltij(1), saltij(2)
                if (seisme) write (ifm,1232) salijs(1), salijs(2)
                write (ifm,1331) fuij(1), fuij(2)
            endif
 10     continue
 20 end do
!
! --- CALCUL DU FACTEUR D'USAGE
!
    if (lfat) then
        if (seisme) then
            call rc32fs(nbsig2, nb_occurr, impr_situ, matrice_fu_s, matrice_fu_b,&
                        fuse(1), ns, nscy, ug)
            utot = utot + ug
        endif
        if (.not. yapass) then
            call rc32fu(nbsig2, nb_occurr, impr_situ, zr(jmfu), zr(jmke), &
                        lieu, ug, factus, ugenv, lefat)
        else
            call rc32fp(nbsig2, nb_occurr, impr_situ, zi(jnsg), zr(jmfu),&
                        zr(jmke), lieu, ug, factus, ugenv, lefat)
        endif
        utot = utot + ug
        utotenv = utotenv + ugenv
    endif
!
    resumax(1) = 0.d0
    resumax(2) = 0.d0
    resumax(3) = 0.d0
    resumax(4) = sm
    resumax(5) = snmax
    resumax(6) = snemax
    resumax(7) = spmax
    resumax(8) = kemax
    resumax(9) = samax
    resumax(10) = sigpm
    resumax(11) = spthem
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
    1014 format (1p,' SITUATION ',i4,' SN =',e12.5 )
    1015 format (1p,' SITUATION ',i4,' SN =',e12.5 ,&
     &    ' SN AVEC SEISME =',e12.5 )
    1016 format (1p,' SITUATION ',i4,' SN* =',e12.5 )
    1017 format (1p,' SITUATION ',i4,' SN* =',e12.5,&
     &    ' SN* AVEC SEISME =',e12.5 )
    1018 format (1p,' SITUATION ',i4,' SP =',e12.5)
    1019 format (1p,' SITUATION ',i4,' AVEC SEISME : SP =',e12.5)
    1050 format (1p,' SITUATION ',i4,' SPMECA=',e12.5,' SPTHER=',e12.5,&
     &                                ' KEMECA=',e12.5,' KETHER=',e12.5)
    1051 format (1p,' SITUATION ',i4,' AVEC SEISME : SPMECA =',e12.5,&
     &                                ' KEMECA=',e12.5)
    1060 format (1p,' SITUATION ',i4,' SALT =',e12.5,' FACT_USAGE =',e12.5)
    1061 format (1p,' SITUATION ',i4,' AVEC SEISME : SALT =',e12.5,&
     &                                   ' FACT_USAGE =',e12.5)
!
    1110 format (1p,' COMBINAISON DES SITUATIONS ',i4,3x,i4,'  SN =',e12.5)
    1111 format (1p,41x,'AVEC SEISME : SN =',e12.5)
    1121 format (1p,41x,'SP1 =',e12.5,2x,'SP2 =',e12.5)
    1122 format (1p,41x,'AVEC SEISME : SP1 =',e12.5,2x,'SP2 =',e12.5)
    1131 format (1p,41x,'SPMECA1=',e12.5,' SPMECA2=',e12.5,&
     &                                ' KEMECA=',e12.5)
    1132 format (1p,41x,'SPTHER1=',e12.5,' SPTHER2=',e12.5,&
     &                                ' KETHER=',e12.5)
    1133 format (1p,41x,'AVEC SEISME : SPMECA1=',e12.5,' SPMECA2=',e12.5,&
     &                                ' KEMECA=',e12.5)
    1231 format (1p,41x,'SALT1 =',e12.5,2x,'SALT2 =',e12.5)
    1232 format (1p,41x,'AVEC SEISME : SALT1 =',e12.5,2x,'SALT2 =',e12.5)
    1331 format (1p,41x,'FU1 =',e12.5,2x,'FU2 =',e12.5)
    call jedema()
end subroutine
