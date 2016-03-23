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
#include "asterfort/as_allocate.h"
#include "asterfort/wkvect.h"
#include "asterfort/utmess.h"
#include "asterfort/rcmo02.h"
#include "asterfort/rcZ2s0.h"
#include "asterfort/rcZ2sn1.h"
#include "asterfort/rcZ2sp1.h"
#include "asterfort/rcma02.h"
#include "asterfort/rc32sa.h"
#include "asterfort/rcZ2s2.h"
#include "asterfort/rcZ2rt.h"
#include "asterfort/limend.h"
#include "asterfort/rcvale.h"
#include "asterfort/rc32fs.h"
#include "asterfort/rc32fu.h"
#include "asterfort/rc32fp.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedema.h"
!
    integer :: ig, iocs
    aster_logical :: lsn, lther, lfat, lefat, yapass, seisme
    character(len=8) :: mater
    character(len=4) :: lieu
    real(kind=8) :: utot, utotenv, resuas(*), resuss(*), resuca(*)
    real(kind=8) :: resucs(*), factus(*), resumax(*)
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
    real(kind=8) :: sm, snmax, snemax, spmax, kemax, spthem, samax
    real(kind=8) :: sigpm, sn, snet, sp(2), spmeca(2), mse(12)
    character(len=8) :: typeke, kbid
    integer, pointer :: situ_numero(:) => null()
    real(kind=8), pointer :: situ_pres_a(:) => null()
    real(kind=8), pointer :: situ_pres_b(:) => null()
    integer, pointer :: situ_nb_occur(:) => null()
    integer :: nbsig2, ndim, jmfu, jmke, ioc1, ns, nscy, nsitup, i
    integer, pointer :: nb_occurr(:) => null()
    integer, pointer :: impr_situ(:) => null()
    real(kind=8), pointer :: matrice_fu_b(:) => null()
    real(kind=8), pointer :: matrice_fu_s(:) => null()
    real(kind=8) :: matpi(7), kemeca, kether, saltse(2), fuse(2)
    integer :: icas, icss, i1, nocc, indi, icodre(1), ioc2, i2
    real(kind=8) :: spthep(2), ppi, ppj, mpi(12), mpj(12), simpij, sns
    real(kind=8) :: snets, sn1, sn2, snet1, snet2, matpj(7), sp1(2)
    real(kind=8) :: sp2, saltij(2), smm, fuij(2), sps(2), spmecs(2)
    real(kind=8) :: kemecs, kethes, saltijs(2), ug, nadm(1), vale(2)
    aster_logical :: endur
    integer :: nsituq, j, inds
    real(kind=8) :: pqi, pqj, mqi(12), mqj(12), matqi(7), matqj(7) 
    real(kind=8) :: snpq, snpqs, spcomb(2), sp1comb(2), sp2comb 
    real(kind=8) :: mat1(7), mat2(7), spther(2), ugenv, spmeca1(2)
    real(kind=8) :: spmecs1(2),sp3, spmeca3, sp3bid, spmeca3bid
    real(kind=8) :: sp3s, spmeca3s, sp1s(2), mat1s(7), mat2s(7), sp2s
    real(kind=8) :: instsn(2), instsp(4), instsns(2), instsps(4)
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
    spthem = 0.d0
    samax = 0.d0
    sigpm = 0.d0
!
    instsp(1)=0.d0
    instsp(2) =0.d0
    instsp(3) =0.d0
    instsp(4) =0.d0
    instsps(1)=0.d0
    instsps(2) =0.d0
    instsps(3) =0.d0
    instsps(4) =0.d0
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
 11 continue
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
!
        sn = 0.d0
        snet = 0.d0
        sp(1) = 0.d0
        sp(2) = 0.d0
        spmeca(1) = 0.d0
        spmeca(2) = 0.d0
!
        ns = situ_nb_occur(1+2*(nbsitu+iocs)-2)
        nscy = situ_nb_occur(1+2*(nbsitu+iocs)-1)
        nsitup = situ_numero(1+zi(jnsg+is1-1)-1)
        call rcmo02('S', nsitup, mse)
! ----- Calcul du SN, SN*
        ppi = 0.d0
        do 13 i = 1, 12
            mpi(i) = 0.d0
 13    continue
!
        if (lsn) then
            call rcZ2s0('SN', ppi, mpi, ppi, mpi, mse, sn2)
            call rcZ2sn1(lieu, nsitup, nsitup, iocs,&
                         instsn, sn1, sp3bid, spmeca3bid)
            resuas(10*(is1-1)+4) = sn1+sn2
            snmax = max( snmax , sn )
        endif
        if (lsn .and. lther) then
            call rcZ2s0('SN', ppi, mpi, ppi, mpi, mse, snet2)
            call rcZ2sn1(lieu, nsitup, nsitup, iocs,&
                         instsn, snet1, sp3bid, spmeca3bid)
            snet=snet1+snet2
            resuas(10*(is1-1)+5) = snet
            snemax = max( snemax , snet )
        endif
! ----- Calcul de SP, SALT et FU
        if (lfat) then
            call rcZ2s0('SP', ppi, mpi, ppi, mpi, mse, sp(1))
            sp(1) = sp(1)
            spmeca(1) = sp(1)
            call rcma02('A', iocs, matpi)
            call rc32sa('SITU', mater, matpi, matpi, sn, sp, spmeca,&
                        kemeca, kether, saltse, sm, fuse)
            resuas(10*(is1-1)+6) = sp(1)
            resuas(10*(is1-1)+7) = kemeca
            resuas(10*(is1-1)+8) = kether
            resuas(10*(is1-1)+9) = saltse(1)
        endif
! ----- Ajout de commentaires dans le fichier mess 
        if (niv .ge. 2) then
            if (lsn) write (ifm,*) '  SEISME,   SN = ',sn
            if (lsn .and. lther) write (ifm,*) '  SEISME,  SN* = ',snet
            if (lfat) write (ifm,*) '  SEISME,   SP = ',sp(1)
            if (typeke .eq. 'KE_MIXTE' .and. lfat) then
                spthep(1)=max(0.0,sp(1)-spmeca(1))
                write (ifm,*) '            SPMECA = ',spmeca(1)
                write (ifm,*) '            SPTHER = ',spthep(1)
                write (ifm,*) '            KEMECA = ',kemeca
                write (ifm,*) '            KETHER = ',kether
            endif
            if (lfat) write (ifm,*) '          SALT = ',saltse(1)
            if (lfat) write (ifm,*) '          FU = ',fuse(1)
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
        nsitup = situ_numero(ioc1)
        ppi = situ_pres_a(ioc1)
        ppj = situ_pres_b(ioc1)
        call rcmo02('A', nsitup, mpi)
        call rcmo02('B', nsitup, mpj)
!
        sn = 0.d0
        snet = 0.d0
        simpij = 0.d0
        sns = 0.d0
        snets = 0.d0
!
!------ Calcul du SN, SN* et ROCHET THERMIQUE
        if (lsn) then
            call rcZ2sn1(lieu, nsitup, nsitup, 0,&
                         instsn, sn1, sp3, spmeca3)
            call rcZ2s2('SN', ppi, mpi, ppj, mpj, sn2)
            sn = sn1+sn2
            resuss(10*(is1-1)+4) = sn
            snmax = max(snmax,sn)
        endif
        if (lsn .and. lther) then
            call rcZ2sn1(lieu, nsitup, nsitup, 0,&
                         instsn, snet1, sp3bid, spmeca3bid)
            call rcZ2s2('SN', ppi, mpi, ppj, mpj, snet2)
            snet = snet1+snet2
            resuss(10*(is1-1)+5) = snet
            snemax = max(snemax,snet)
        endif
        if (lther) then
            call rcZ2rt(ppi, ppj, simpij)
            sigpm = max ( sigpm, simpij )
        endif
!------ Calcul du SN, SN* si SEISME
        if (seisme) then
            if (lsn) then
                call rcZ2sn1(lieu, nsitup, nsitup, iocs,&
                             instsn, sn1, sp3s, spmeca3s)
                call rcZ2s0('SN', ppi, mpi, ppj, mpj, mse, sn2)
                sns = sn1+sn2
                resuas(10*(is1-1)+4) = sns
                snmax = max(snmax,sns)
            endif
            if (lsn .and. lther) then
                call rcZ2sn1(lieu, nsitup, nsitup, iocs,&
                             instsn, snet1, sp3bid, spmeca3bid)
                call rcZ2s0('SN', ppi, mpi, ppj, mpj, mse, snet2)
                snets = snet1+snet2
                resuas(10*(is1-1)+5) = snets
                snemax = max(snemax,snets)
            endif
        endif
! ----- Ajout de commentaires dans le fichier mess
        if (niv .ge. 2) then
            if (lsn) then
                if (lther) then
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
! ----- Calcul de SP, SALT et FU
        if ((lsn .or. lther) .and. .not.lfat) goto 20
!
        sp(1) = 0.d0
        sp(2) = 0.d0
        spmeca(1) = 0.d0
        spmeca(2) = 0.d0
!
        nocc = situ_nb_occur(1+2*ioc1-2)
        call rcma02('A', ioc1, matpi)
        call rcma02('B', ioc1, matpj)
!
        call rcZ2sp1(lieu, nsitup, nsitup, 0,&
                     instsp, sp1, spmeca1)
        call rcZ2s2('SP', ppi, mpi, ppj, mpj, sp2)
        sp(1) = sp1(1)+sp2+sp3
        spmeca(1) = spmeca1(1)+sp2+spmeca3
        call rc32sa('SITU', mater, matpi, matpj, sn, sp, spmeca,&
                    kemeca, kether, saltij, smm, fuij)
        resuss(10*(is1-1)+6) = sp(1)
        resuss(10*(is1-1)+7) = kemeca
        resuss(10*(is1-1)+8) = kether
        resuss(10*(is1-1)+9) = saltij(1)
        kemax = max( kemax , kemeca )
!
        indi = nbsig2*(i1-1)+(i1-1)
! ----- on remplit la matrice des fu
        zr(jmfu-1+indi+1) = fuij(1)
! ----- on remplit la matrice des ke
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
! ----- Calcul de SP, SALT et FU si SEISME
        sps(1) = 0.d0
        sps(2) = 0.d0
        spmecs(1) = 0.d0
        spmecs(2) = 0.d0
!
        if (seisme) then
            matrice_fu_b(indi+1) = fuij(1)
            call rcZ2sp1(lieu, nsitup, nsitup,iocs,&
                         instsps, sp1s, spmecs1)
            call rcZ2s0('SP', ppi, mpi, ppj, mpj, mse, sp2)
            sps(1) = sp1s(1)+sp2+sp3s
            spmecs(1) = spmecs1(1)+sp2+spmeca3s
            call rc32sa('SITU', mater, matpi, matpj, sns, sps, spmecs,&
                        kemecs, kethes, saltijs, smm, fuij)
            resuas(10*(is1-1)+6) = sps(1)
            resuas(10*(is1-1)+7) = kemecs
            resuas(10*(is1-1)+8) = kethes
            resuas(10*(is1-1)+9) = saltijs(1)
            kemax = max( kemax , kemeca )
            matrice_fu_s(indi+1) = fuij(1)
        endif
!
        spmax = max(spmax,sps(1),sp(1))
        spthem = max(0.0,sps(1)-spmecs(1),sp(1)-spmeca(1))
! ----- Ajout de commentaires dans le fichier mess
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
            if (seisme) write (ifm,161) nsitup, saltijs(1), matrice_fu_s(indi+1)
        endif
! ----- Facteur d'usage élémentaire
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
! ----------------------------------------------------------------------
!               ON TRAITE LA COMBINAISON DES SITUATIONS P ET Q
! ----------------------------------------------------------------------
        i2 = i1
        do 10 is2 = is1 + 1, nbsigr
            ioc2 = zi(jnsg+is2-1)
            if (.not.zl(jcombi+ioc2-1)) goto 10
            if (ioc2 .gt. nbsitu) goto 10
            i2 = i2 + 1
!
            nsituq = situ_numero(ioc2)
            pqi = situ_pres_a(ioc2)
            pqj = situ_pres_b(ioc2)
            call rcmo02('A', nsituq, mqi)
            call rcmo02('B', nsituq, mqj)
            call rcma02('A', ioc2, matqi)
            call rcma02('B', ioc2, matqj)
! --------- Calcul du rochet thermique
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
! ------- CALCUL DU SN
            snpq = 0.d0
            sn1 = 0.d0
            sn2 = 0.d0
            sp3 = 0.d0
            spmeca3 = 0.d0
!---------- On maximise la partie B3200
            call rcZ2sn1(lieu, nsitup, nsituq, 0,&
                         instsn, sn1, sp3, spmeca3)
!---------- On maximise la partie B3600
            call rcZ2s2('SN', ppi, mpi, ppj, mpj, sn)
            sn2 = max(sn, sn2)
            call rcZ2s2('SN', ppi, mpi, pqi, mqi, sn)
            sn2 = max(sn, sn2)
            call rcZ2s2('SN', ppi, mpi, pqj, mqj, sn)
            sn2 = max(sn, sn2)
            call rcZ2s2('SN', ppj, mpj, pqj, mqj, sn)
            sn2 = max(sn, sn2)
            call rcZ2s2('SN', ppj, mpj, pqi, mqi, sn)
            sn2 = max(sn, sn2)
            call rcZ2s2('SN', pqi, mqi, pqj, mqj, sn)
            sn2 = max(sn, sn2)
!
            snpq=sn1+sn2
            icss = icss + 1
            resucs(icss) = snpq
            snmax = max(snmax,snpq)
!
! ------- CALCUL DU SN SI SEISME (A MODFIFIER)
            if (seisme) then
                snpqs = 0.d0
                sn1 = 0.d0
                sn2 = 0.d0
!---------- On maximise la partie B3200
                call rcZ2sn1(lieu, nsitup, nsituq, iocs,&
                             instsns, sn1, sp3s, spmeca3s)
!---------- On maximise la partie B3600
                call rcZ2s0('SN', ppi, mpi, ppj, mpj, mse, sn)
                sn2 = max(sn, sn2)
                call rcZ2s0('SN', ppi, mpi, pqi, mqi, mse, sn)
                sn2 = max(sn, sn2)
                call rcZ2s0('SN', ppi, mpi, pqj, mqj, mse, sn)
                sn2 = max(sn, sn2)
                call rcZ2s0('SN', ppj, mpj, pqj, mqj, mse, sn)
                sn2 = max(sn, sn2)
                call rcZ2s0('SN', ppj, mpj, pqi, mqi, mse, sn)
                sn2 = max(sn, sn2)
                call rcZ2s0('SN', pqi, mqi, pqj, mqj, mse, sn)
                sn2 = max(sn, sn2)
!
                snpqs=sn1+sn2
                icas = icas + 1
                resuca(icas) = snpqs
                snmax = max(snmax,snpqs)
            endif
!
            if (niv .ge. 2) write (ifm,110) nsitup,nsituq,snpq
            if ((niv.ge.2) .and. seisme) write (ifm,111) snpqs
!
! ------- CALCUL DU SP
            spcomb(1)= 0.d0
            spcomb(2)= 0.d0
            sp1comb(1)   = 0.d0
            sp1comb(2)   = 0.d0
            sp2comb      = 0.d0
            spmeca(1)= 0.d0
            spmeca(2)= 0.d0
            spmeca1(1)= 0.d0
            spmeca1(2)= 0.d0
!---------- On maximise la partie B3200
            call rcZ2sp1(lieu, nsitup, nsituq, 0,&
                         instsp, sp1comb, spmeca1)
!---------- On maximise la partie B3600
            do 19 j = 1, 7
                mat1(j) = matpi(j)
                mat2(j) = matpj(j)
19         continue
            call rcZ2s2('SP', ppi, mpi, ppj, mpj, sp2)
            sp2comb = sp2
            call rcZ2s2('SP', ppi, mpi, pqi, mqi, sp2)
            if (sp2 .gt. sp2comb) then
                sp2comb=sp2
                do 129 j = 1, 7
                mat1(j) = matpi(j)
                mat2(j) = matqi(j)
129             continue
            endif
            call rcZ2s2('SP', ppi, mpi, pqj, mqj, sp2)
            if (sp2 .gt. sp2comb) then
                sp2comb=sp2
                do 139 j = 1, 7
                mat1(j) = matpi(j)
                mat2(j) = matqj(j)
139             continue
            endif
            call rcZ2s2('SP', ppj, mpj, pqj, mqj, sp2)
            if (sp2 .gt. sp2comb) then
                sp2comb=sp2
                do 149 j = 1, 7
                mat1(j) = matpj(j)
                mat2(j) = matqj(j)
149             continue
            endif
            call rcZ2s2('SP', ppj, mpj, pqi, mqi, sp2)
            if (sp2 .gt. sp2comb) then
                sp2comb=sp2
                do 159 j = 1, 7
                mat1(j) = matpj(j)
                mat2(j) = matqi(j)
159             continue
            endif
            call rcZ2s2('SP', pqi, mqi, pqj, mqj, sp2)
            if (sp2 .gt. sp2comb) then
                sp2comb=sp2
                do 169 j = 1, 7
                mat1(j) = matqi(j)
                mat2(j) = matqj(j)
169             continue
            endif
!
            spcomb(1)=sp1comb(1)+sp2comb+sp3
            spcomb(2)=sp1comb(2)+sp3
            spmeca(1)=spmeca1(1)+sp2comb+spmeca3
            spmeca(2)=spmeca1(2)+spmeca3
!
! ------- CALCUL DU SP SI SEISME
!
            if (seisme) then
                sps(1)= 0.d0
                sps(2)= 0.d0
                spmecs(1)= 0.d0
                spmecs(2)= 0.d0
                sp1s(1)   = 0.d0
                sp1s(2)   = 0.d0
                spmecs1(1)= 0.d0
                spmecs1(2)= 0.d0
                sp2s      = 0.d0
!---------- On maximise la partie B3200
                call rcZ2sp1(lieu, nsitup, nsituq, iocs,&
                             instsps, sp1s, spmecs1)
!---------- On maximise la partie B3600
                do 28 j = 1, 7
                    mat1s(j) = matpi(j)
                    mat2s(j) = matpj(j)
28             continue
                call rcZ2s0('SP', ppi, mpi, ppj, mpj, mse, sp2)
                sp2s = sp2
                call rcZ2s0('SP', ppi, mpi, pqi, mqi, mse, sp2)
                if (sp2 .gt. sp2s) then
                    sp2s=sp2
                    do 128 j = 1, 7
                        mat1s(j) = matpi(j)
                        mat2s(j) = matqi(j)
128                 continue
                endif
                call rcZ2s0('SP', ppi, mpi, pqj, mqj, mse, sp2)
                if (sp2 .gt. sp2s) then
                    sp2s=sp2
                    do 138 j = 1, 7
                        mat1s(j) = matpi(j)
                        mat2s(j) = matqj(j)
138                 continue
                endif
                call rcZ2s0('SP', ppj, mpj, pqj, mqj, mse, sp2)
                if (sp2 .gt. sp2s) then
                    sp2s=sp2
                    do 148 j = 1, 7
                        mat1s(j) = matpj(j)
                        mat2s(j) = matqj(j)
148                 continue
                endif
                call rcZ2s0('SP', ppj, mpj, pqi, mqi, mse, sp2)
                if (sp2 .gt. sp2s) then
                    sp2s=sp2
                    do 158 j = 1, 7
                        mat1s(j) = matpj(j)
                        mat2s(j) = matqi(j)
158                 continue
                 endif
                 call rcZ2s0('SP', pqi, mqi, pqj, mqj, mse, sp2)
                 if (sp2 .gt. sp2s) then
                     sp2s=sp2
                     do 168 j = 1, 7
                        mat1s(j) = matqi(j)
                        mat2s(j) = matqj(j)
168                  continue
                 endif
!
                sps(1)=sp1s(1)+sp2s+sp3s
                sps(2)=sp1s(2)+sp3s
                spmecs(1)=spmecs1(1)+sp2s+spmeca3s
                spmecs(2)=spmecs1(2)+spmeca3s
            endif
!
! ------- CALCUL DU SALT et du FU partiel
            call rc32sa('COMB', mater, mat1, mat2, snpq, spcomb, spmeca,&
                        kemeca, kether, saltij, smm, fuij)
            icss = icss + 1
            resucs(icss) = spcomb(1)
            icss = icss + 1
            resucs(icss) = spcomb(2)
            icss = icss + 1
            resucs(icss) = saltij(1)
            icss = icss + 1
            resucs(icss) = saltij(2)
            icss = icss + 1
            resucs(icss) = instsn(1)
            icss = icss + 1
            resucs(icss) = instsn(2)
            icss = icss + 1
            resucs(icss) = instsp(1)
            icss = icss + 1
            resucs(icss) = instsp(2)
            icss = icss + 1
            resucs(icss) = instsp(3)
            icss = icss + 1
            resucs(icss) = instsp(4)
            icss = icss + 1
            resucs(icss) = fuij(1)+fuij(2)
            kemax = max( kemax , kemeca )
            if (saltij(1) .gt. samax) then
                samax = saltij(1)
                sm = smm
            else if (saltij(2).gt.samax) then
                samax = saltij(2)
                sm = smm
            endif
!
            inds = nbsig2*(i1-1) + (i2-1)
            indi = nbsig2*(i2-1) + (i1-1)
! --------- on remplit la matrice des fu
            zr(jmfu-1+indi+1) = fuij(1)+fuij(2)
            zr(jmfu-1+inds+1) = fuij(1)+fuij(2)
! --------- on remplit la matrice des ke
            if (typeke .eq. 'KE_MECA') then
                zr(jmke-1+indi+1) = kemax
                zr(jmke-1+inds+1) = kemax
            else
                zr(jmke-1+indi+1) = (kemeca*spmeca(1)+kether*(spcomb(1)-spmeca(1)))/(spcomb(1))
                zr(jmke-1+inds+1) = (kemeca*spmeca(1)+kether*(spcomb(1)-spmeca(1)))/(spcomb(1))
            endif
! ------- CALCUL DU SALT et du FU partiel si SEISME
            if (seisme) then
                matrice_fu_b(inds+1) = fuij(1)+fuij(2)
                matrice_fu_b(indi+1) = fuij(1)+fuij(2)
                call rc32sa('COMB', mater, mat1, mat2, snpqs, sps, spmecs,&
                             kemecs, kethes, saltijs, smm, fuij)
                icas = icas + 1
                resuca(icas) = sps(1)
                icas = icas + 1
                resuca(icas) = sps(2)
                icas = icas + 1
                resuca(icas) = saltijs(1)
                icas = icas + 1
                resuca(icas) = saltijs(2)
                icas = icas + 1
                resuca(icas) = instsns(1)
                icas = icas + 1
                resuca(icas) = instsns(2)
                icas = icas + 1
                resuca(icas) = instsps(1)
                icas = icas + 1
                resuca(icas) = instsps(2)
                icas = icas + 1
                resuca(icas) = instsps(3)
                icas = icas + 1
                resuca(icas) = instsps(4)
                icas = icas + 1
                resuca(icas) = fuij(1)+fuij(2)
                kemax = max( kemax , kemeca )
                matrice_fu_s(inds+1) = fuij(1)+fuij(2)
                matrice_fu_s(indi+1) = fuij(1)+fuij(2)
            endif
! ----- Ajout de commentaires dans le fichier mess
            spmax = max(spmax,sps(1),spcomb(1),sps(2),spcomb(2))
            spthem = max(0.0,sps(1)-spmecs(1),spcomb(1)-spmeca(1))
            if (niv .ge. 2) then
                write (ifm,121) spcomb(1), spcomb(2)
                if (seisme) write (ifm,122) sps(1), sps(2)
                if (typeke .eq. 'KE_MIXTE') then
                    write (ifm,131) spmeca(1),spmeca(2),kemeca
                    spther(1)=max(0.0,spcomb(1)-spmeca(1))
                    spther(2)=max(0.0,spcomb(2)-spmeca(2))
                    write (ifm,132) spther(1),spther(2),kether
                    if (seisme) write (ifm,133) spmecs(1),spmecs(2), kemecs
                endif
                write (ifm,231) saltij(1), saltij(2)
                if (seisme) write (ifm,232) saltijs(1), saltijs(2)
                write (ifm,331) fuij(1), fuij(2)
            endif
 10     continue
 20 continue
!
! ----------------------------------------------------------------------
!               CALCUL DU FACTEUR D'USAGE TOTAL
!   (on parcourt la matrice des facteurs élémentaires)
! ----------------------------------------------------------------------
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
    resumax(1)  = 0.d0
    resumax(2)  = 0.d0
    resumax(3)  = 0.d0
    resumax(4)  = sm
    resumax(5)  = snmax
    resumax(6)  = snemax
    resumax(7)  = spmax
    resumax(8)  = kemax
    resumax(9)  = samax
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
