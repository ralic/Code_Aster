subroutine rc32ac(lpmpb, lsn, lsnet, lfatig, lrocht,&
                  mater, fatiguenv)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterc/r8vide.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexnom.h"
#include "asterfort/rc3201.h"
#include "asterfort/rc32pm.h"
#include "asterfort/rc32rt.h"
#include "asterfort/rc32sa.h"
#include "asterfort/rc32sn.h"
#include "asterfort/rcZ2env.h"
#include "asterfort/rc32sp.h"
#include "asterfort/rcma02.h"
#include "asterfort/rcmo02.h"
#include "asterfort/wkvect.h"
#include "asterfort/codent.h"
    aster_logical :: lpmpb, lsn, lsnet, lfatig, lrocht, fatiguenv
    character(len=8) :: mater
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
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     CALCUL DES AMPLITUDES DE CONTRAINTES
!     CALCUL DU FACTEUR D'USAGE
!
!     Pour chaque extremite :
!
!     pour une situation P, on a 2 états stabilisés
!     pour une situation Q, on a 2 états stabilisés
!
!     Soit 2 états stabilisés I et J appartenant respectivement aux
!     situations P et Q :
!
!     on calcule le SALT(I,J) = 0,5*(EC/E)*Ke*Sn(P,Q)*Sp(I,J)
!
!     avec Sn(P,Q) = Max( Sn(I,J) )
!          Sn(I,J) = Max( Max(Sn(I,J,ThP)), Max(Sn(I,J,ThQ)) )
!
!     avec Sp(I,J) = Max( Max(Sp(I,J,ThP)), Max(Sp(I,J,ThQ)) )
!
!
! Etape 1 : on calcule le SALT qui correspond aux combinaisons de tous
!           les états stabilisés appartenant aux situations d'un groupe
!           donné.
!
! Etape 2 : on calcule le SALT pour les situations non combinables
!
! Etape 3 : traitement des situations de passage
!           on calcule le SALT(I,J)
!              - avec I appartenant au premier groupe
!              - avec J appartenant au deuxieme groupe
!              - on lui associe le nombre d'occurrences de la
!                situation de passage
!     ------------------------------------------------------------------
!
    integer :: ig, nbgr, nbsigr, jnsg, is1, ioc1, nocc, numgr, jcombi
    integer :: im, npass, ifm, niv, iocs, jresu
    integer :: nsitup, nsituq, iret, i1, jfact, i, j, jreas, jress
    integer :: jreca, jrecs, ndim, nbp12, nbp23, nbp13
    real(kind=8) :: ppi, ppj, snmax, spmax, samax, utot, saltij(2), typeke, ug
    real(kind=8) :: pmbmax, fuij(2), mpi(12), mpj(12), sm, sn, snet, sp(2), smm
    real(kind=8) :: matpi(8), matpj(8), mse(12), spmeca(2), spthem
    real(kind=8) :: spmecm, kemeca, kether, pm, pb, pmpb, sipmax, simpij, snemax
    real(kind=8) :: kemax, pmmax, pbmax, utotenv, feni, ke
    aster_logical :: seisme, cfait
    character(len=4) :: lieu(2)
    character(len=24) :: k24as, k24ss, k24ca, k24cs, k24fu
    integer, pointer :: situ_numero(:) => null()
    real(kind=8), pointer :: situ_pres_a(:) => null()
    integer, pointer :: situ_nb_occur(:) => null()
    real(kind=8), pointer :: situ_pres_b(:) => null()
    integer, pointer :: situ_seisme(:) => null()
    integer, pointer :: situ_nume_group(:) => null()
!
    data lieu / 'ORIG' , 'EXTR' /
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    call jeveuo('&&RC3200.SITU_NUMERO', 'L', vi=situ_numero)
    call jelira('&&RC3200.SITU_NUME_GROUP', 'LONMAX', nbgr)
    call jeveuo('&&RC3200.SITU_NUME_GROUP', 'L', vi=situ_nume_group)
    call jeveuo('&&RC3200.SITU_SEISME', 'L', vi=situ_seisme)
!
    call jeveuo('&&RC3200.SITU_COMBINABLE', 'L', jcombi)
    call jeveuo('&&RC3200.SITU_PRES_A', 'L', vr=situ_pres_a)
    call jeveuo('&&RC3200.SITU_PRES_B', 'L', vr=situ_pres_b)
    call jeveuo('&&RC3200.SITU_NB_OCCUR', 'L', vi=situ_nb_occur)
!
    call jelira('&&RC32SI.PASSAGE_1_2', 'LONUTI', nbp12)
    call jelira('&&RC32SI.PASSAGE_2_3', 'LONUTI', nbp23)
    call jelira('&&RC32SI.PASSAGE_1_3', 'LONUTI', nbp13)
!
! --- IL FAUT CALCULER LE FACTEUR D'USAGE A CHAQUE EXTREMITE
!
    do 10 im = 1, 2
!
! ------ POUR CHAQUE SITUATION, ON ARCHIVE :
!         * 10 QUANTITES AVEC LA PRISE EN COMPTE DU SEISME
!         * 10 QUANTITES SANS LA PRISE EN COMPTE DU SEISME
!         1  : PM
!         2  : PB
!         3  : PMPB
!         4  : SN
!         5  : SN*
!         6  : SP
!         7  : KE_MECA
!         8  : KE_THER
!         9  : SALT
!         10 : UG
!
! ------ POUR CHAQUE COMBINAISON, ON ARCHIVE :
!         1  : SN(P,Q)
!         2  : SP1_IJ
!         3  : SP2_IJ
!         4  : SALT1(P,Q)
!         5  : SALT2(P,Q)
!
        k24as = '&&RC3200.AVEC_SEISME'//lieu(im)
        call jecrec(k24as, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbgr)
!
        k24ss = '&&RC3200.SANS_SEISME'//lieu(im)
        call jecrec(k24ss, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbgr)
!
        k24ca = '&&RC3200.COMBI_A_SEI'//lieu(im)
        call jecrec(k24ca, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbgr)
!
        k24cs = '&&RC3200.COMBI_S_SEI'//lieu(im)
        call jecrec(k24cs, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbgr)
!
        k24fu = '&&RC3200.FACT_USAGE '//lieu(im)
        call jecrec(k24fu, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbgr)
!
        if (niv .ge. 2) then
            if (im .eq. 1) then
                write(ifm,*)'  '
                write(ifm,*)'******* ORIGINE DU SEGMENT *******'
            else
                write(ifm,*)'  '
                write(ifm,*)'******* EXTREMITE DU SEGMENT *******'
            endif
            write(ifm,*) ' '
            write(ifm,*)&
     &       '=> ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE'
        endif
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
        utot = 0.d0
        utotenv = 0.d0
        sipmax = 0.d0
!
! ----------------------------------------------------------------------
!                           E T A P E   1
! ----------------------------------------------------------------------
!
! ------ ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE
!        -----------------------------------------------------
!
        do 100 ig = 1, nbgr
!
            numgr = situ_nume_group(ig)
            if (numgr .lt. 0) goto 100
!
            iocs = situ_seisme(ig)
!
            call jelira(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONUTI', nbsigr)
            call jeveuo(jexnum('&&RC3200.LES_GROUPES', numgr), 'L', jnsg)
            if (niv .ge. 2) then
                write (ifm,3000) numgr,nbsigr
                write (ifm,3002) (situ_numero(1+zi(jnsg+i1-1)-1),i1=1,&
                nbsigr)
            endif
            call jecroc(jexnum(k24as, ig))
            call jeecra(jexnum(k24as, ig), 'LONMAX', 10*nbsigr)
            call jeveuo(jexnum(k24as, ig), 'E', jreas)
!
            call jecroc(jexnum(k24ss, ig))
            call jeecra(jexnum(k24ss, ig), 'LONMAX', 10*nbsigr)
            call jeveuo(jexnum(k24ss, ig), 'E', jress)
!
            ndim = max(5,int(5*nbsigr*(nbsigr-1)/2))
            call jecroc(jexnum(k24ca, ig))
            call jeecra(jexnum(k24ca, ig), 'LONMAX', ndim)
            call jeveuo(jexnum(k24ca, ig), 'E', jreca)
!
            call jecroc(jexnum(k24cs, ig))
            call jeecra(jexnum(k24cs, ig), 'LONMAX', ndim)
            call jeveuo(jexnum(k24cs, ig), 'E', jrecs)
!
            call jecroc(jexnum(k24fu, ig))
            call jeecra(jexnum(k24fu, ig), 'LONMAX', 6*50)
            call jeveuo(jexnum(k24fu, ig), 'E', jfact)
!
            if (ig .eq. 1) then
                if (nbp12 .ne. 0 .or. nbp13 .ne. 0) goto 100
            else if (ig .eq. 2) then
                if (nbp12 .ne. 0 .or. nbp23 .ne. 0) goto 100
            else if (ig .eq. 3) then
                if (nbp13 .ne. 0 .or. nbp23 .ne. 0) goto 100
            endif
!
            npass = 0
            if (iocs .eq. 0) then
                seisme = .false.
            else
                seisme = .true.
            endif
!
            call rc3201(lpmpb, lsn, lsnet, lfatig, lrocht,&
                        lieu(im), numgr, iocs, seisme, npass,&
                        mater, snmax, snemax, spmax, kemax,&
                        spmecm, spthem, samax, utot, utotenv, sm,&
                        sipmax, zr(jreas), zr(jress), zr(jreca), zr(jrecs),&
                        zr(jfact), pmmax, pbmax, pmbmax, fatiguenv)
!
100     continue
!
! ----------------------------------------------------------------------
!                           E T A P E   2
! ----------------------------------------------------------------------
!
        seisme = .false.
        do 220 i = 1, 12
            mse(i) = 0.d0
220     continue
!
! ------ ON TRAITE LES SITUATIONS NON COMBINABLES
!        ----------------------------------------
!
        cfait = .false.
        do 200 ig = 1, nbgr
!
            numgr = situ_nume_group(ig)
            if (numgr .lt. 0) goto 200
!
            call jelira(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONUTI', nbsigr)
            call jeveuo(jexnum('&&RC3200.LES_GROUPES', numgr), 'L', jnsg)
!
            npass = 0
!
            do 210 is1 = 1, nbsigr
                ioc1 = zi(jnsg+is1-1)
                if (zl(jcombi+ioc1-1)) goto 210
!
                call jeexin(jexnum(k24as, ig), iret)
                if (iret .eq. 0) then
                    call jecroc(jexnum(k24as, ig))
                    call jeecra(jexnum(k24as, ig), 'LONMAX', 10*nbsigr)
                endif
                call jeveuo(jexnum(k24as, ig), 'E', jreas)
                do 212 j = 1, 10
                    zr(jreas-1+10*(is1-1)+j) = r8vide()
212             continue
!
                call jeexin(jexnum(k24ss, ig), iret)
                if (iret .eq. 0) then
                    call jecroc(jexnum(k24ss, ig))
                    call jeecra(jexnum(k24ss, ig), 'LONMAX', 10*nbsigr)
                endif
                call jeveuo(jexnum(k24ss, ig), 'E', jress)
!
                if (.not.cfait .and. niv .ge. 2) then
                    cfait = .true.
                    write(ifm,*) ' '
                    write(ifm,*)&
     &   '=> ON TRAITE LES SITUATIONS NON COMBINABLES DANS LEUR GROUPE'
                endif
                if (niv .ge. 2) write (ifm,2000) ig, ioc1
!
                nsitup = situ_numero(ioc1)
!
                nocc = situ_nb_occur(1+2*ioc1-2)
!
                ppi = situ_pres_a(ioc1)
                call rcmo02('A', nsitup, mpi)
                call rcma02('A', ioc1, matpi)
!
                ppj = situ_pres_b(ioc1)
                call rcmo02('B', nsitup, mpj)
                call rcma02('B', ioc1, matpj)
!
                nsituq = 0
!
! ----------- CALCUL DU PM_PB
!
                if (lpmpb) then
                    pm = 0.d0
                    pb = 0.d0
                    pmpb = 0.d0
                    call rc32pm(lieu(im), seisme, ppi, mpi, mse,&
                                pm, pb, pmpb)
                    call rc32pm(lieu(im), seisme, ppj, mpj, mse,&
                                pm, pb, pmpb)
                    zr(jress-1+10*(is1-1)+1) = pm
                    zr(jress-1+10*(is1-1)+2) = pb
                    zr(jress-1+10*(is1-1)+3) = pmpb
                    if (niv .ge. 2) then
                        write (ifm,2020) nsitup, pm, pb, pmpb
                    endif
                    pmmax = max ( pmmax , pm )
                    pbmax = max ( pbmax , pb )
                    pmbmax = max ( pmbmax , pmpb )
                endif
!
! ----------- CALCUL DU SN
!
                if (lsn) then
                    sn = 0.d0
                    call rc32sn('SN_SITU', lieu(im), nsitup, ppi, mpi,&
                                nsituq, ppj, mpj, seisme, mse,&
                                sn)
                    snmax = max ( snmax , sn )
                    zr(jress-1+10*(is1-1)+4) = sn
                    if (niv .ge. 2) then
                        write (ifm,2030) nsitup, sn
                    endif
                endif
!
! ----------- CALCUL DU SN*
!
                if (lsn .and. lsnet) then
                    snet = 0.d0
                    call rc32sn('SN*_SITU', lieu(im), nsitup, ppi, mpi,&
                                nsituq, ppj, mpj, seisme, mse,&
                                snet)
                    snemax = max ( snemax , snet )
                    zr(jress-1+10*(is1-1)+5) = snet
                    if (niv .ge. 2) then
                        write (ifm,2032) nsitup, snet
                    endif
                endif
!
! ----------- CALCUL DU ROCHET THERMIQUE
!
                if (lrocht) then
                    call rc32rt(lieu(im), ppi, ppj, simpij)
                    sipmax = max ( sipmax, simpij )
                    write (ifm,2034) nsitup, simpij
                endif
!
                if (.not.lfatig) goto 210
!
! ----------- CALCUL DU SP
!
                sp(1) = 0.d0
                sp(2) = 0.d0
                typeke=matpi(8)
                call rc32sp('SP_SITU', lieu(im), nsitup, ppi, mpi,&
                            nsituq, ppj, mpj, seisme, mse,&
                            sp, typeke, spmeca)
                spmax = max ( spmax , sp(1) )
                spthem = max(0.0,spmax-spmeca(1))
                if (niv .ge. 2) write (ifm,2040) nsitup, sp(1)
                zr(jress-1+10*(is1-1)+6) = sp(1)
!
! ----------- CALCUL DU SALT
!
                call rc32sa('SITU', mater, matpi, matpj, sn,&
                            sp, typeke, spmeca, kemeca,&
                            kether, saltij, smm, fuij)
                kemax = max ( kemax , kemeca )
                if (niv .ge. 2) then
                    write (ifm,2050) nsitup, saltij(1)
                endif
                zr(jress-1+10*(is1-1)+7) = kemeca
                zr(jress-1+10*(is1-1)+8) = kether
                zr(jress-1+10*(is1-1)+9) = saltij(1)
!
                if (saltij(1) .gt. samax) then
                    samax = saltij(1)
                    sm = smm
                endif
!
! ----------- CALCUL DU FACTEUR D'USAGE
!
                ug = dble( nocc ) * fuij(1)
!
                zr(jress-1+10*(is1-1)+10) = ug
                if (niv .ge. 2) then
                    write (ifm,2060) nsitup, ug
                endif
!
                if (typeke .lt. 0) then
                    ke = kemeca
                else
                    ke = (kemeca*spmeca(1)+kether*(sp(1)-spmeca(1)))/(sp(1))
                endif
                if (fatiguenv) then
                    call rcZ2env(nsitup, nsitup, ke, lieu(im), feni)
                    utotenv = utotenv + ug*feni
                endif
!
                utot = utot + ug
!
210         continue
!
200     continue
!
! ----------------------------------------------------------------------
!                           E T A P E   3
! ----------------------------------------------------------------------
!
! ------ ON TRAITE LES SITUATIONS DE PASSAGE
!        -----------------------------------
!
        do 310 ig = 1, nbgr
!
            numgr = situ_nume_group(ig)
            if (numgr .ge. 0) goto 310
            numgr = -numgr
            iocs = situ_seisme(ig)
            if (iocs .eq. 0) then
                seisme = .false.
            else
                seisme = .true.
            endif
!
            call jelira(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONUTI', nbsigr)
            call jeveuo(jexnum('&&RC3200.LES_GROUPES', numgr), 'L', jnsg)
            if (niv .ge. 2) then
                write (ifm,3004)
                write (ifm,3002) (situ_numero(1+zi(jnsg+i1-1)-1),i1=1,&
                nbsigr)
            endif
!
            call jecroc(jexnum(k24as, ig))
            call jeecra(jexnum(k24as, ig), 'LONMAX', 10*nbsigr)
            call jeveuo(jexnum(k24as, ig), 'E', jreas)
!
            call jecroc(jexnum(k24ss, ig))
            call jeecra(jexnum(k24ss, ig), 'LONMAX', 10*nbsigr)
            call jeveuo(jexnum(k24ss, ig), 'E', jress)
!
            ndim = max(5,5*nbsigr*(nbsigr-1)/2)
            call jecroc(jexnum(k24ca, ig))
            call jeecra(jexnum(k24ca, ig), 'LONMAX', ndim)
            call jeveuo(jexnum(k24ca, ig), 'E', jreca)
!
            call jecroc(jexnum(k24cs, ig))
            call jeecra(jexnum(k24cs, ig), 'LONMAX', ndim)
            call jeveuo(jexnum(k24cs, ig), 'E', jrecs)
!
            call jecroc(jexnum(k24fu, ig))
            call jeecra(jexnum(k24fu, ig), 'LONMAX', 6*50)
            call jeveuo(jexnum(k24fu, ig), 'E', jfact)
!
            npass = 7
!
            call rc3201(lpmpb, lsn, lsnet, lfatig, lrocht,&
                        lieu(im), numgr, iocs, seisme, npass,&
                        mater, snmax, snemax, spmax, kemax,&
                        spmecm, spthem, samax, utot, utotenv, sm,&
                        sipmax, zr(jreas), zr(jress), zr(jreca), zr(jrecs),&
                        zr(jfact), pmmax, pbmax, pmbmax, fatiguenv)
!
!
310     continue
!
! ----------------------------------------------------------------------
!
! ------ ON STOCKE LES RESULTATS DE CALCUL
!        ---------------------------------
!
        call wkvect('&&RC3200.RESULTAT  .'//lieu(im), 'V V R', 14, jresu)
!        - LE PM
        zr(jresu ) = pmmax
!        - LE PB
        zr(jresu+1) = pbmax
!        - LE PMPB
        zr(jresu+2) = pmbmax
!        - LE SM
        zr(jresu+3) = sm
!        - LE SN/3SM
        if (sm .eq. 0.d0) then
            zr(jresu+4) = 0.d0
        else
            zr(jresu+4) = snmax / ( 3 * sm )
        endif
!        - LE SN
        zr(jresu+5) = snmax
!        - LE SN*
        zr(jresu+6) = snemax
!        - LE SP
        zr(jresu+7) = spmax
!        - LE KE
        zr(jresu+8) = kemax
!        - LE SALT
        zr(jresu+9) = samax
!        - LE FACTEUR D'USAGE TOTAL
        zr(jresu+10) = utot
!        - LE FACTEUR D'USAGE TOTAL AVEC ENVIRONNEMENT
        zr(jresu+11) = utotenv
!        - LE ROCHET THERMIQUE
        zr(jresu+12) = sipmax
        zr(jresu+13) = spthem
!
        if (lfatig) write (ifm,2070) utot
!
 10 end do
!
    3000 format (/,'=> GROUPE: ',i4,' , NOMBRE DE SITUATIONS: ',i4)
    3002 format ('=> LISTE DES NUMEROS DE SITUATION: ',100 (i4,1x))
    3004 format (/,'=> SITUATION DE PASSAGE')
    2000 format ('=> GROUPE: ',i4,' , SITUATION: ',i4)
    2020 format (1p,' SITUATION ',i4,' PM =',e12.5,&
     &                            ' PB =',e12.5,' PMPB =',e12.5)
    2030 format (1p,' SITUATION ',i4,' SN =',e12.5 )
    2032 format (1p,' SITUATION ',i4,' SN* =',e12.5 )
    2034 format (1p,' SITUATION ',i4,' ROCHET THERMIQUE =',e12.5 )
    2040 format (1p,' SITUATION ',i4,' SP =',e12.5)
    2050 format (1p,' SITUATION ',i4,' SALT =',e12.5)
    2060 format (1p,' SITUATION ',i4,' FACT_USAGE =',e12.5)
    2070 format (1p,' SOMME(FACT_USAGE) =',e12.5)
!
    call jedema()
end subroutine
