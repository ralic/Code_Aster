subroutine rc3201(ze200, ig, lpmpb, lsn, lther, lfat, lefat,&
                  yapass, seisme, iocs, mater, lieu,&
                  utot, utotenv, resuas, resuss,&
                  resuca, resucs, factus, factus2, resumax)
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
#include "asterfort/rc32pm.h"
#include "asterfort/rc32sn.h"
#include "asterfort/rcZ2sn.h"
#include "asterfort/rc32sp.h"
#include "asterfort/rcma02.h"
#include "asterfort/rc32sa.h"
#include "asterfort/rcZ2s2.h"
#include "asterfort/rc32rt.h"
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
    aster_logical :: ze200, lpmpb, lsn, lther, lfat, lefat, yapass, seisme
    character(len=8) :: mater
    character(len=4) :: lieu
    real(kind=8) :: utot, utotenv, resuas(*), resuss(*), resuca(*)
    real(kind=8) :: resucs(*), factus(*), resumax(*)
    character(len=24) :: factus2(*)
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
    real(kind=8) :: pmmax, pbmax, pmbmax, pm, pb, pmpb, sij0(12)
    real(kind=8) :: pms, pbs, pmpbs
    real(kind=8) :: snmax, snemax, spmax, spthem, samax
    real(kind=8) :: sigpm, sn, snet, sp(2), spmeca(2), mse(12)
    character(len=8) :: typeke, kbid
    integer, pointer :: situ_numero(:) => null()
    real(kind=8), pointer :: situ_pres_a(:) => null()
    real(kind=8), pointer :: situ_pres_b(:) => null()
    integer, pointer :: situ_nb_occur(:) => null()
    character(len=24), pointer :: situ_nom(:) => null()
    integer :: nbsig2, ndim, jmfu, jmcomb, ioc1, ns, nscy, nsitup, i
    integer, pointer :: nb_occurr(:) => null()
    character(len=24), pointer :: nom_situ(:) => null()
    integer, pointer :: impr_situ(:) => null()
    real(kind=8), pointer :: matrice_fu_b(:) => null()
    real(kind=8), pointer :: matrice_fu_s(:) => null()
    real(kind=8) :: matpi(7), kemeca, kether, saltse(2), fuse(2)
    integer :: icas, icss, i1, nocc, indi, icodre(1), ioc2, i2, k
    real(kind=8) :: spthep(2), ppi, ppj, mpi(12), mpj(12), simpij, sns
    real(kind=8) :: snets, matpj(7), sp1(2)
    real(kind=8) :: sp2, saltij(2), smm, fuij(2), sps(2), spmecs(2)
    real(kind=8) :: kemecs, kethes, saltijs(2), ug, nadm(1), vale(2)
    aster_logical :: endur
    integer :: nsituq, j, inds, ndim2
    real(kind=8) :: pqi, pqj, mqi(12), mqj(12), matqi(7), matqj(7) 
    real(kind=8) :: snpq, snpqs, spcomb(2), sp1comb(2) 
    real(kind=8) :: mat1(7), mat2(7), spther(2), ugenv, spmeca1(2)
    real(kind=8) :: spmecs1(2),sp3, spmeca3, sp3bid, spmeca3bid
    real(kind=8) :: sp3s, spmeca3s, sp1s(2), mat1s(7), mat2s(7)
    real(kind=8) :: instsn(2), instsp(4), instsns(2), instsps(4)
    real(kind=8) :: sp1combs(2), spmecomb(2)
    real(kind=8) :: spmecombs(2)
    real(kind=8) :: propi(20), propj(20), proqi(20)
    real(kind=8) :: proqj(20)
!
! DEB ------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
    pmmax = 0.d0
    pbmax = 0.d0
    pmbmax = 0.d0
    snmax = 0.d0
    snemax = 0.d0
    spmax = 0.d0
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
    call jeveuo('&&RC3200.SITU_NOM', 'L', vk24=situ_nom)
    call jelira('&&RC3200.SITU_PRES_A', 'LONUTI', nbsitu)
    call jelira(jexnum('&&RC3200.LES_GROUPES', ig), 'LONUTI', nbsigr)
    call jeveuo(jexnum('&&RC3200.LES_GROUPES', ig), 'L', jnsg)
!
    do 11 is1 = 1, nbsigr
        do 12 is2 = 1, 9
            resuas(9*(is1-1)+is2) = r8vide()
            resuss(9*(is1-1)+is2) = r8vide()
 12     continue
 11 continue
!
    if (iocs .eq. 0) then
        nbsig2 = nbsigr
    else
        nbsig2 = nbsigr - 1
    endif
    ndim = nbsig2*nbsig2
    ndim2 = 12*ndim
!
    AS_ALLOCATE(vi=nb_occurr, size=nbsig2)
    AS_ALLOCATE(vk24=nom_situ, size=nbsig2)
    AS_ALLOCATE(vi=impr_situ, size=nbsig2)
!
    call wkvect('&&RC3201.MATRICE_FU', 'V V R', ndim, jmfu)
    call wkvect('&&RC3201.MATRICE_COMB', 'V V R', ndim2, jmcomb)
    do 100 i = 1, ndim2
        zr(jmcomb+i-1)=r8vide()
100 continue
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
        pm = 0.d0
        pb = 0.d0
        pmpb = 0.d0
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
        ppi = 0.d0
        do 13 i = 1, 12
            mpi(i) = 0.d0
 13    continue
        do 113 k = 1, 12
                sij0(k) = 0.d0
    113 continue
!
! ----- Calcul du PMPB
        if (lpmpb) then
            call rc32pm(lieu, seisme, ppi, sij0, mse,&
                        pm, pb, pmpb)
            resuas(9*(is1-1)+1) = pm
            resuas(9*(is1-1)+2) = pb
            resuas(9*(is1-1)+3) = pmpb
            pmmax = max ( pmmax , pm )
            pbmax = max ( pbmax , pb )
            pmbmax = max ( pmbmax , pmpb )
        endif
! ----- Calcul du SN
        if (lsn) then
            call rcma02('A', iocs, matpi)
            propi(1)=ppi
            do 223 k = 1,12
              propi(1+k) = mpi(k)             
223         continue
            do 222 j = 1,7
              propi(13+j) = matpi(j)                   
222         continue
            do 221 j = 1,20
              proqi(j) = 0.d0                   
221         continue
            call rcZ2sn(ze200, lieu, nsitup, nsitup,iocs, mse,&
                        propi, propi, proqi, proqi, instsn, sn, sp3bid, spmeca3bid)
            resuas(9*(is1-1)+4) = sn
            snmax = max( snmax , sn )
        endif
! ----- Calcul du SN*
        if (lsn .and. lther) then
            call rc32sn('SN*_SITU', lieu, nsitup, ppi, sij0,&
                        0, ppi, sij0, seisme, mse, snet)
            resuas(9*(is1-1)+5) = snet
            snemax = max( snemax , snet )
        endif
! ----- Calcul de SP, SALT et FU
        if (lfat) then
            call rc32sp(ze200, lieu, nsitup, nsitup, iocs, mse,&
                         propi, propi, proqi, proqi, instsp, sp1, spmeca1, mat1, mat2)
            sp(1) = sp1(1)
            spmeca(1) = spmeca1(1)
            call rc32sa('SITU', mater, matpi, matpi, sn, sp, spmeca,&
                        kemeca, kether, saltse, smm, fuse)
            resuas(9*(is1-1)+6) = kemeca
            resuas(9*(is1-1)+7) = kether
            resuas(9*(is1-1)+8) = saltse(1)
        endif
! ----- Ajout de commentaires dans le fichier mess 
        if (niv .ge. 2) then
            if (lpmpb) then
                write (ifm,*) '  SEISME,   PM = ',pm
                write (ifm,*) '            PB = ',pb
                write (ifm,*) '          PMPB = ',pmpb
            endif
            if (lsn) then
                write (ifm,*) '  SEISME,   SN = ',sn
                if (instsn(1) .lt. 0.d0) then
                    write (ifm,*) '                    (SN SANS INSTANTS)'
                else
                    write (ifm,*) '                    INSTANTS SN  : ',instsn
                endif
            endif
            if (lsn .and. lther) write (ifm,*) '  SEISME,  SN* = ',snet
            if (lfat) then
                write (ifm,*) '  SEISME,   SP = ',sp(1)
                if (instsp(1) .lt. 0.d0) then
                    write (ifm,*) '                    (SP SANS INSTANTS)'
                else
                    write (ifm,*) '                    INSTANTS SP  : ',instsp(1),',', instsp(2)
                endif
            endif
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
        indi = nbsig2*(i1-1)+(i1-1)
        nb_occurr(i1) = situ_nb_occur(1+2*ioc1-2)
        nom_situ(i1) = situ_nom(ioc1)
        impr_situ(i1) = situ_numero(ioc1)
        nsitup = situ_numero(ioc1)
        ppi = situ_pres_a(ioc1)
        ppj = situ_pres_b(ioc1)
        call rcmo02('A', nsitup, mpi)
        call rcmo02('B', nsitup, mpj)
!
        pm = 0.d0
        pb = 0.d0
        pmpb = 0.d0
        sn = 0.d0
        snet = 0.d0
        simpij = 0.d0
        pms = 0.d0
        pbs = 0.d0
        pmpbs = 0.d0
        sns = 0.d0
        snets = 0.d0
!
! ----- Calcul du PMPB
        if (lpmpb) then
            call rc32pm(lieu, .false._1, ppi, mpi, mse,&
                        pm, pb, pmpb)
            call rc32pm(lieu, .false._1, ppj, mpj, mse,&
                        pm, pb, pmpb)
            resuss(9*(is1-1)+1) = pm
            resuss(9*(is1-1)+2) = pb
            resuss(9*(is1-1)+3) = pmpb
            pmmax = max ( pmmax , pm )
            pbmax = max ( pbmax , pb )
            pmbmax = max ( pmbmax , pmpb )
        endif
!
! ----- Calcul du SN
        if (lsn) then
            propi(1)=ppi
            propj(1)=ppj
            do 323 k = 1,12
              propi(1+k) = mpi(k)     
              propj(1+k) = mpj(k)         
323         continue
            do 322 j = 1,7
              propi(13+j) = 0.d0   
              propj(13+j) = 0.d0              
322         continue
            do 321 j = 1,20
               proqi(j) = 0.d0                   
321          continue
            call rcZ2sn(ze200, lieu, nsitup, nsitup, 0, mse,&
                     propi, propj, proqi, proqi, instsn, sn, sp3, spmeca3)
            resuss(9*(is1-1)+4) = sn
            snmax = max(snmax,sn)
        endif
! ----- Calcul du SN*
        if (lsn .and. lther) then
            call rc32sn('SN*_SITU', lieu, nsitup, ppi, mpi,&
                        0, ppj, mpj, .false._1, mse, snet)
            resuss(9*(is1-1)+5) = snet
            snemax = max(snemax,snet)
        endif
! ----- Calcul du Rochet Thermique
        if (lther) then
            call rc32rt(lieu, ppi, ppj, simpij)
            sigpm = max ( sigpm, simpij )
        endif
!------ Calcul du PMPB, SN, SN* si SEISME
        if (seisme) then
            if (lpmpb) then
                call rc32pm(lieu, seisme, ppi, mpi, mse,&
                            pms, pbs, pmpbs)
                call rc32pm(lieu, seisme, ppj, mpj, mse,&
                            pms, pbs, pmpbs)
                resuas(9*(is1-1)+1) = pms
                resuas(9*(is1-1)+2) = pbs
                resuas(9*(is1-1)+3) = pmpbs
                pmmax = max ( pmmax , pms )
                pbmax = max ( pbmax , pbs )
                pmbmax = max ( pmbmax , pmpbs )
            endif
            if (lsn) then
                call rcZ2sn(ze200, lieu, nsitup, nsitup, iocs, mse,&
                           propi, propj, proqi, proqi, instsns, sns, sp3s, spmeca3s)
                resuas(9*(is1-1)+4) = sns
                snmax = max(snmax,sns)
            endif
            if (lsn .and. lther) then
                call rc32sn('SN*_SITU', lieu, nsitup, ppi, mpi,&
                            0, ppj, mpj, seisme, mse, snets)
                resuas(9*(is1-1)+5) = snets
                snemax = max(snemax,snets)
            endif
        endif
! ----- Ajout de commentaires dans le fichier mess
        if (niv .ge. 2) then
          if (lpmpb) write (ifm,112) nsitup, pm, pb, pmpb
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
              if (instsn(1) .lt. 0.d0) then
                write (ifm,*) '                    (SN SANS INSTANTS)'
              else
                write (ifm,*) '                                         INSTANTS SN  : '
                write (ifm,444) instsn(1), instsn(2)
                write (ifm,445) instsns(1), instsns(2)
              endif
            else
              write (ifm,114) nsitup, sn
              if (instsn(1) .lt. 0.d0) then
                write (ifm,*) '                    (SN SANS INSTANTS)'
              else
                write (ifm,*) '                                         INSTANTS SN  : ',instsn
              endif
            endif
          endif
        endif
!
! ----- Calcul de SP, SALT et FU
        if ((lpmpb .or. lsn .or. lther) .and. .not.lfat) goto 20
!
        sp(1) = 0.d0
        sp(2) = 0.d0
        sp1 = 0.d0
        sp2 = 0.d0
        spmeca(1) = 0.d0
        spmeca(2) = 0.d0
!
        nocc = situ_nb_occur(1+2*ioc1-2)
        call rcma02('A', ioc1, matpi)
        call rcma02('B', ioc1, matpj)
        do 324 j = 1,7
          propi(13+j) = matpi(j)    
          propj(13+j) = matpj(j)               
324     continue
!
        call rc32sp(ze200, lieu, nsitup, nsitup, 0, mse,&
                     propi, propj, proqi, proqi, instsp, sp1, spmeca1, mat1, mat2)
        sp(1) = sp1(1)+sp3
        spmeca(1) = spmeca1(1)+spmeca3
        call rc32sa('SITU', mater, matpi, matpj, sn, sp, spmeca,&
                    kemeca, kether, saltij, smm, fuij)
        resuss(9*(is1-1)+6) = kemeca
        resuss(9*(is1-1)+7) = kether
        resuss(9*(is1-1)+8) = saltij(1)
!
! ----- on remplit la matrice des fu
        zr(jmfu-1+indi+1) = fuij(1)
! ----- on remplit la matrice des grandeurs de la situation p
        if(instsn(1) .ge. 0) then
            zr(jmcomb-1+indi+1) = instsn(1)
            zr(jmcomb+ndim-1+indi+1) = instsn(2)
        endif
        zr(jmcomb+2*ndim-1+indi+1) = sn
        zr(jmcomb+ndim*3-1+indi+1) = kemeca
        zr(jmcomb+ndim*4-1+indi+1) = kether
        if (typeke .eq. 'KE_MECA') then
            zr(jmcomb+ndim*5-1+indi+1) = kemeca
        else
            zr(jmcomb+ndim*5-1+indi+1) = (kemeca*spmeca(1)+kether*(sp(1)-spmeca(1)))/(sp(1))
        endif
        if(instsp(1) .ge. 0) then
            zr(jmcomb+ndim*6-1+indi+1) = instsp(1)
            zr(jmcomb+ndim*7-1+indi+1) = instsp(2)
        endif
        zr(jmcomb+ndim*10-1+indi+1) = saltij(1)
!
        samax = max(samax,saltij(1))
! ----- Calcul de SP, SALT et FU si SEISME
        sps(1) = 0.d0
        sps(2) = 0.d0
        spmecs(1) = 0.d0
        spmecs(2) = 0.d0
!
        if (seisme) then
            matrice_fu_b(indi+1) = fuij(1)
            propi(1)=ppi
            propj(1)=ppj
            do 423 k = 1,12
              propi(1+k) = mpi(k)     
              propj(1+k) = mpj(k)         
423         continue
            do 422 j = 1,7
              propi(13+j) = matpi(j)    
              propj(13+j) = matpj(j)               
422         continue
            do 421 j = 1,20
              proqi(j) = 0.d0                   
421         continue
            call rc32sp(ze200, lieu, nsitup, nsitup,iocs, mse,&
                         propi, propj, proqi, proqi, instsps, sp1s, spmecs1, mat1, mat2)
            sps(1) = sp1s(1)+sp3s
            spmecs(1) = spmecs1(1)+spmeca3s
            call rc32sa('SITU', mater, matpi, matpj, sns, sps, spmecs,&
                        kemecs, kethes, saltijs, smm, fuij)
            resuas(9*(is1-1)+6) = kemecs
            resuas(9*(is1-1)+7) = kethes
            resuas(9*(is1-1)+8) = saltijs(1)
            matrice_fu_s(indi+1) = fuij(1)
        endif
!
        spmax = max(spmax,sps(1),sp(1))
        spthem = max(0.0,sps(1)-spmecs(1),sp(1)-spmeca(1))
! ----- Ajout de commentaires dans le fichier mess
        if (niv .ge. 2) then
          if (seisme) then
            write (ifm,119) nsitup, sp(1), sps(1)
            if (instsp(1) .lt. 0.d0) then
              write (ifm,*) '                   (SP SANS INSTANTS)'
            else
              write (ifm,*) '                                         INSTANTS SP   : '
              write (ifm,444) instsp(1), instsp(2)
              write (ifm,445) instsps(1), instsps(2)
            endif
          else
            write (ifm,118) nsitup, sp(1)
            if (instsp(1) .lt. 0.d0) then
              write (ifm,*) '                   (SP SANS INSTANTS)'
            else
              write (ifm,*) '                                         INSTANTS SP   : ',&
              instsp(1),',',instsp(2)
            endif
          endif
          if (typeke .eq. 'KE_MIXTE') then
            spthep(1)=max(0.0,sp(1)-spmeca(1))
            write (ifm,150) nsitup,spmeca(1),spthep(1),kemeca,kether
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
        if (seisme) resuas(9*(is1-1)+9) = ug
        resuss(9*(is1-1)+9) = ug
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!               ON TRAITE LA COMBINAISON DES SITUATIONS P ET Q
! ---------------------------------------------------------------------
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
! --------------------------------------------
!               CALCUL DU SN SANS SEISME
! --------------------------------------------
            propi(1) = ppi
            propj(1) = ppj
            proqi(1) = pqi
            proqj(1) = pqj
            do 523 k = 1,12
                propi(1+k) = mpi(k)     
                propj(1+k) = mpj(k)
                proqi(1+k) = mqi(k)     
                proqj(1+k) = mqj(k)           
523         continue
            do 522 j = 1,7
                propi(13+j) = matpi(j)     
                propj(13+j) = matpj(j)
                proqi(13+j) = matqi(j)     
                proqj(13+j) = matqj(j)                 
522         continue
!
            snpq = 0.d0
            sp3 = 0.d0
            spmeca3 = 0.d0
            call rcZ2sn(ze200, lieu, nsitup, nsituq, 0, mse,&
                     propi, propj, proqi, proqj, instsn, snpq, sp3, spmeca3)
!
            icss = icss + 1
            if (instsn(1) .ge. 0) then
                resucs(icss) = instsn(1)
            else
                resucs(icss) = r8vide()
            endif
            resuca(icss) = r8vide()
            icss = icss + 1
            if (instsn(2) .ge. 0) then
                resucs(icss) = instsn(2)
            else
                resucs(icss) = r8vide()
            endif
            resuca(icss) = r8vide()
            icss = icss + 1
            resucs(icss) = snpq
            resuca(icss) = r8vide()
            snmax = max(snmax,snpq)
!
! --------------------------------------------
!               CALCUL DU SN AVEC SEISME
! --------------------------------------------
            if (seisme) then
                snpqs = 0.d0
                sp3s = 0.d0
                spmeca3s = 0.d0
!---------- On maximise les parties B3200 et ZE200
                call rcZ2sn(ze200, lieu, nsitup, nsituq, iocs, mse,&
                           propi, propj, proqi, proqj, instsns, snpqs, sp3s, spmeca3s)
!
                icas = icas + 1
                if(instsns(1) .ge. 0) then
                    resuca(icas) = instsns(1)
                else
                    resuca(icas) = r8vide()
                endif
                icas = icas + 1
                if(instsns(2) .ge. 0) then
                    resuca(icas) = instsns(2)
                else
                    resuca(icas) = r8vide()
                endif
                icas = icas + 1
                resuca(icas) = snpqs
                snmax = max(snmax,snpqs)
            endif
!
            if (niv .ge. 2) then
              write (ifm,110) nsitup,nsituq,snpq
              if (seisme) write (ifm,111) snpqs
              if (instsn(1) .lt. 0) then
                write (ifm,*) '                                             (SN SANS INSTANTS)'
              else
                write (ifm,*) '                                        INSTANTS SN   : '
                write (ifm,444) instsn(1), instsn(2)
                if (seisme) write (ifm,445) instsns(1), instsns(2)
              endif
            endif
!
! --------------------------------------------
!               CALCUL DU SP SANS SEISME
! --------------------------------------------
            spcomb(1)= 0.d0
            spcomb(2)= 0.d0
            sp1comb(1)   = 0.d0
            sp1comb(2)   = 0.d0
            spmeca(1)= 0.d0
            spmeca(2)= 0.d0
            spmecomb(1)= 0.d0
            spmecomb(2)= 0.d0
!
            call rc32sp(ze200, lieu, nsitup, nsituq, 0, mse,&
                        propi, propj, proqi, proqj,&
                        instsp, sp1comb, spmecomb, mat1, mat2)
!
            spcomb(1)=sp1comb(1)+sp3
            spcomb(2)=sp1comb(2)+sp3
            spmeca(1)=spmecomb(1)+spmeca3
            spmeca(2)=spmecomb(2)+spmeca3
!
! --------------------------------------------
!               CALCUL DU SP AVEC SEISME
! --------------------------------------------
            if (seisme) then
                sps(1)= 0.d0
                sps(2)= 0.d0
                spmecs(1)= 0.d0
                spmecs(2)= 0.d0
                sp1combs(1)   = 0.d0
                sp1combs(2)   = 0.d0
                spmecombs(1)= 0.d0
                spmecombs(2)= 0.d0  
!
                call rc32sp(ze200, lieu, nsitup, nsituq, iocs, mse,&
                            propi, propj, proqi, proqj,&
                            instsps, sp1combs, spmecombs, mat1s, mat2s)
!
                sps(1)=sp1combs(1)+sp3s
                sps(2)=sp1combs(2)+sp3s
                spmecs(1)=spmecombs(1)+spmeca3s
                spmecs(2)=spmecombs(2)+spmeca3s
            endif
!
! ------- CALCUL DU SALT et du FU partiel
            call rc32sa('COMB', mater, mat1, mat2, snpq, spcomb, spmeca,&
                        kemeca, kether, saltij, smm, fuij)
            icss = icss + 1
            resucs(icss) = kemeca
            resuca(icss) = r8vide()
            icss = icss + 1
            resucs(icss) = kether
            resuca(icss) = r8vide()
            icss = icss + 1
            if(instsp(1) .ge. 0) then
                resucs(icss) = instsp(1)
            else
                resucs(icss) = r8vide()
            endif
            resuca(icss) = r8vide()
            icss = icss + 1
            if(instsp(2) .ge. 0) then
                resucs(icss) = instsp(2)
            else
                resucs(icss) = r8vide()
            endif
            resuca(icss) = r8vide()
            icss = icss + 1
            if(instsp(3) .ge. 0) then
                resucs(icss) = instsp(3)
            else
                resucs(icss) = r8vide()
            endif
            resuca(icss) = r8vide()
            icss = icss + 1
            if(instsp(4) .ge. 0) then
                resucs(icss) = instsp(4)
            else
                resucs(icss) = r8vide()
            endif
            resuca(icss) = r8vide()
            icss = icss + 1
            resucs(icss) = saltij(1)
            resuca(icss) = r8vide()
            icss = icss + 1
            resucs(icss) = saltij(2)
            resuca(icss) = r8vide()
            icss = icss + 1
            resucs(icss) = fuij(1)+fuij(2)
            resuca(icss) = r8vide()
!
            samax = max(samax,saltij(1))
            samax = max(samax,saltij(2))
!
            inds = nbsig2*(i1-1) + (i2-1)
            indi = nbsig2*(i2-1) + (i1-1)
! --------- on remplit la matrice des fu
            zr(jmfu-1+indi+1) = fuij(1)+fuij(2)
            zr(jmfu-1+inds+1) = fuij(1)+fuij(2)
! --------- on remplit la matrice des grandeurs de la combinaisons
            if(instsn(1) .ge. 0) then
                zr(jmcomb-1+indi+1) = instsn(1)
                zr(jmcomb-1+inds+1) = instsn(1)
                zr(jmcomb+ndim-1+indi+1) = instsn(2)
                zr(jmcomb+ndim-1+inds+1) = instsn(2)
            endif
            zr(jmcomb+2*ndim-1+indi+1) = snpq
            zr(jmcomb+2*ndim-1+inds+1) = snpq
            zr(jmcomb+3*ndim-1+indi+1) = kemeca
            zr(jmcomb+3*ndim-1+inds+1) = kemeca
            zr(jmcomb+4*ndim-1+indi+1) = kether
            zr(jmcomb+4*ndim-1+inds+1) = kether
            if (typeke .eq. 'KE_MECA') then
              zr(jmcomb+ndim*5-1+indi+1) = kemeca
              zr(jmcomb+ndim*5-1+inds+1) = kemeca
            else
              zr(jmcomb+ndim*5-1+indi+1) = (kemeca*spmeca(1)+&
                                            kether*(spcomb(1)-spmeca(1)))/(spcomb(1))
              zr(jmcomb+ndim*5-1+inds+1) = (kemeca*spmeca(1)+&
                                            kether*(spcomb(1)-spmeca(1)))/(spcomb(1))
            endif
            if(instsp(1) .ge. 0) then
                zr(jmcomb+6*ndim-1+indi+1) = instsp(1)
                zr(jmcomb+6*ndim-1+inds+1) = instsp(1)
                zr(jmcomb+7*ndim-1+indi+1) = instsp(2)
                zr(jmcomb+7*ndim-1+inds+1) = instsp(2)
            endif
            if(instsp(3) .ge. 0) then
                zr(jmcomb+8*ndim-1+indi+1) = instsp(3)
                zr(jmcomb+8*ndim-1+inds+1) = instsp(3)
                zr(jmcomb+9*ndim-1+indi+1) = instsp(4)
                zr(jmcomb+9*ndim-1+inds+1) = instsp(4)
            endif
            zr(jmcomb+10*ndim-1+indi+1) = saltij(1)
            zr(jmcomb+10*ndim-1+inds+1) = saltij(1)
            zr(jmcomb+11*ndim-1+indi+1) = saltij(2)
            zr(jmcomb+11*ndim-1+inds+1) = saltij(2)
! ------- CALCUL DU SALT et du FU partiel si SEISME
            if (seisme) then
                matrice_fu_b(inds+1) = fuij(1)+fuij(2)
                matrice_fu_b(indi+1) = fuij(1)+fuij(2)
                call rc32sa('COMB', mater, mat1s, mat2s, snpqs, sps, spmecs,&
                             kemecs, kethes, saltijs, smm, fuij)
                icas = icas + 1
                resuca(icas) = kemecs
                icas = icas + 1
                resuca(icas) = kethes
                icas = icas + 1
                if(instsps(1) .ge. 0) then
                    resuca(icas) = instsps(1)
                else
                    resuca(icas) = r8vide()
                endif
                icas = icas + 1
                if(instsps(2) .ge. 0) then
                    resuca(icas) = instsps(2)
                else
                    resuca(icas) = r8vide()
                endif
                icas = icas + 1
                if(instsps(3) .ge. 0) then
                    resuca(icas) = instsps(3)
                else
                    resuca(icas) = r8vide()
                endif
                icas = icas + 1
                if(instsps(4) .ge. 0) then
                    resuca(icas) = instsps(4)
                else
                    resuca(icas) = r8vide()
                endif
                icas = icas + 1
                resuca(icas) = saltijs(1)
                icas = icas + 1
                resuca(icas) = saltijs(2)
                icas = icas + 1
                resuca(icas) = fuij(1)+fuij(2)
                matrice_fu_s(inds+1) = fuij(1)+fuij(2)
                matrice_fu_s(indi+1) = fuij(1)+fuij(2)
            endif
! ----- Ajout de commentaires dans le fichier mess
            spmax = max(spmax,sps(1),spcomb(1),sps(2),spcomb(2))
            spthem = max(0.0,sps(1)-spmecs(1),spcomb(1)-spmeca(1))
!
            if (niv .ge. 2) then
              write (ifm,121) spcomb(1), spcomb(2)
              if (seisme) write (ifm,122) sps(1), sps(2)
              if (instsp(1) .lt. 0) then
                write (ifm,*) '                                         (SP1 SANS INSTANTS)      '
              else
                write (ifm,*) '                                        INSTANTS  SP1 : '
                write (ifm,444) instsp(1), instsp(2)
                if (seisme) write (ifm,445) instsps(1), instsps(2)
              endif
              if (instsp(3) .lt. 0) then
                write (ifm,*) '                                         (SP2 SANS INSTANTS)      '
              else
                write (ifm,*) '                                        INSTANTS  SP2 : '
                write (ifm,444) instsp(3), instsp(4)
                if (seisme) write (ifm,445) instsps(3), instsps(4)
              endif
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
            call rc32fu(nbsig2, nb_occurr, nom_situ, impr_situ, zi(jnsg), zr(jmfu), &
                        zr(jmcomb), lieu, ug, factus, factus2, ugenv, lefat)
        else
            call rc32fp(nbsig2, nb_occurr, nom_situ, impr_situ, zi(jnsg), zr(jmfu),&
                        zr(jmcomb), lieu, ug, factus, factus2, ugenv, lefat)
        endif
        utot = utot + ug
        utotenv = utotenv + ugenv
    endif
!
    resumax(1)  = pmmax
    resumax(2)  = pbmax
    resumax(3)  = pmbmax
    resumax(4)  = snmax
    resumax(5)  = snemax
    resumax(6)  = spmax
    resumax(7)  = samax
    resumax(8) = sigpm
    resumax(9) = spthem
!
    if (seisme) then
        AS_DEALLOCATE(vr=matrice_fu_b)
        AS_DEALLOCATE(vr=matrice_fu_s)
    endif
    call jedetr('&&RC3201.MATRICE_FU')
    call jedetr('&&RC3201.MATRICE_COMB')
    AS_DEALLOCATE(vi=nb_occurr)
    AS_DEALLOCATE(vi=impr_situ)
    AS_DEALLOCATE(vk24=nom_situ)
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
    119 format (1p,' SITUATION ',i4,' SP =',e12.5 ,&
     &    ' SP AVEC SEISME =',e12.5 )
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
    444 format (1p,'                                           SANS SEISME = ',e12.5,',',e12.5 )
    445 format (1p,'                                           AVEC SEISME = ',e12.5,',',e12.5 )
!
    call jedema()
!
end subroutine
