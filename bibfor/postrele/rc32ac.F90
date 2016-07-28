subroutine rc32ac(ze200, mater, lpmpb, lsn,&
                  lther, lfat, lefat)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jecrec.h"
#include "asterfort/wkvect.h"
#include "asterfort/jexnum.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/rc3201.h"
#include "asterfort/jeexin.h"
#include "asterc/r8vide.h"
#include "asterfort/rcma02.h"
#include "asterfort/rcmo02.h"
#include "asterfort/rc32pm.h"
#include "asterfort/rc32sn.h"
#include "asterfort/rcZ2sn1.h"
#include "asterfort/rcZ2s2.h"
#include "asterfort/rc32rt.h"
#include "asterfort/rc32sp.h"
#include "asterfort/rc32sa.h"
#include "asterfort/getvtx.h"
#include "asterfort/rc32env.h"
#include "asterfort/jedema.h"
!
    aster_logical :: ze200, lpmpb, lsn, lther, lfat, lefat
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
!     OPERATEUR POST_RCCM, CALCUL DES AMPLITUDES DE CONTRAINTES SN et SP 
!     et du FACTEUR D'USAGE
!
!     Pour chaque extremite :
!     on calcule le SALT = 0,5*(EC/E)*Ke(SN)*SP
!
! Etape 1 : on calcule le SALT qui correspond aux combinaisons des 
!           situations d'un groupe donné.
!
! Etape 2 : on calcule le SALT pour les situations non combinables
!
! Etape 3 : traitement des situations de passage
!     ------------------------------------------------------------------
!
    character(len=4) :: lieu(2)
    integer :: ifm, niv, nbgr, jcombi, nbp12, nbp23, nbp13, im, jresu1
    integer, pointer :: situ_numero(:) => null()
    integer, pointer :: situ_nume_group(:) => null()
    integer, pointer :: situ_seisme(:) => null()
    real(kind=8), pointer :: situ_pres_a(:) => null()
    real(kind=8), pointer :: situ_pres_b(:) => null()
    integer, pointer :: situ_nb_occur(:) => null()
    character(len=24) :: k24as, k24ss, k24ca, k24cs, k24fu
    real(kind=8) :: utot, utotenv, mse(12), ppi, ppj, matpi(7) 
    integer :: l, ig, numgr, nbsigr, jnsg, i1, jreas, jress, ndim 
    integer :: jrecs, jreca, jfact, iocs, i, jresu2, kk, is1
    aster_logical :: yapass, seisme, cfait, noth
    integer :: ioc1, iret, j, nsitup, nsituq, nocc, nb, jresu3, ll
    real(kind=8) :: matpj(7), mpi(12), mpj(12), pm, pb, pmpb, sn, snet
    real(kind=8) :: simpij, sp(2), sp1(2), sp2, spmeca(2), kemeca
    real(kind=8) :: kether, saltij(2), smm, fuij(2), upart, ke, feni
    real(kind=8) :: sn1, sn2, spmeca1(2), sp3
    character(len=8) :: typeke
    integer :: jresu
    real(kind=8) :: spmeca3, instsn(2), instsp(4)
!
! DEB ------------------------------------------------------------------
    data lieu / 'ORIG' , 'EXTR' /  
!  
    call jemarq()
    call infniv(ifm, niv)
!
    call jeveuo('&&RC3200.SITU_NUMERO', 'L', vi=situ_numero)
    call jelira('&&RC3200.SITU_NUME_GROUP', 'LONMAX', nbgr)
    call jeveuo('&&RC3200.SITU_NUME_GROUP', 'L', vi=situ_nume_group)
    call jeveuo('&&RC3200.SITU_SEISME', 'L', vi=situ_seisme)
    call jeveuo('&&RC3200.SITU_COMBINABLE', 'L', jcombi)
    call jeveuo('&&RC3200.SITU_PRES_A', 'L', vr=situ_pres_a)
    call jeveuo('&&RC3200.SITU_PRES_B', 'L', vr=situ_pres_b)
    call jeveuo('&&RC3200.SITU_NB_OCCUR', 'L', vi=situ_nb_occur)
    call jelira('&&RC32SI.PASSAGE_1_2', 'LONUTI', nbp12)
    call jelira('&&RC32SI.PASSAGE_2_3', 'LONUTI', nbp23)
    call jelira('&&RC32SI.PASSAGE_1_3', 'LONUTI', nbp13)
!
! --- POUR CHAQUE EXTREMITE
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
!         10 : UPART
!
! ------ POUR CHAQUE COMBINAISON, ON ARCHIVE :
!         1  : SN
!         2  : SP1
!         3  : SP2
!         4  : SALT1
!         5  : SALT2
!         6   : INST SN 1
!         7   : INST SN 2
!         8   : INST SP1 1
!         9   : INST SP1 2
!         10   : INST SP2 1
!         11   : INST SP2 2
!         12  : FU ELEMENTAIRE
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
        utot = 0.d0
        utotenv = 0.d0
!
! ----------------------------------------------------------------------
!                           E T A P E   1
!         ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE
! ----------------------------------------------------------------------
!
        call wkvect('&&RC3200.RESU1.'//lieu(im), 'V V R', 11, jresu1)
!
        do 60 l = 1, 11
            zr(jresu1-1+l) = 0.d0
 60     continue
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
!
        do 100 ig = 1, nbgr
!
            numgr = situ_nume_group(ig)
!---------- s'il y a situation de passage 
            if (numgr .lt. 0) goto 100
!---------- sinon
!------------- on liste les situations du groupe
            call jelira(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONUTI', nbsigr)
            call jeveuo(jexnum('&&RC3200.LES_GROUPES', numgr), 'L', jnsg)
            if (niv .ge. 2) then
                write (ifm,300) numgr,nbsigr
                write (ifm,302) (situ_numero(1+zi(jnsg+i1-1)-1),i1=1,&
                nbsigr)
            endif
!------------- on dimensionne les vecteurs en fonction du nombre 
!------------- de situations dans le groupe 
            call jecroc(jexnum(k24as, ig))
            call jeecra(jexnum(k24as, ig), 'LONMAX', 10*nbsigr)
            call jeveuo(jexnum(k24as, ig), 'E', jreas)
!
            call jecroc(jexnum(k24ss, ig))
            call jeecra(jexnum(k24ss, ig), 'LONMAX', 10*nbsigr)
            call jeveuo(jexnum(k24ss, ig), 'E', jress)
!
            ndim = max(12,int(12*nbsigr*(nbsigr-1)/2))
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
            yapass = .false.
!
            iocs = situ_seisme(ig)
            if (iocs .eq. 0) then
                seisme = .false.
            else
                seisme = .true.
            endif
!
            call rc3201(ze200, numgr, lpmpb, lsn, lther, lfat, lefat, yapass,&
                        seisme, iocs, mater, lieu(im), utot, utotenv,&
                        zr(jreas), zr(jress), zr(jreca), zr(jrecs),&
                        zr(jfact), zr(jresu1))
!
100     continue
!
! ----------------------------------------------------------------------
!                           E T A P E   2
!               ON TRAITE LES SITUATIONS NON COMBINABLES
! ----------------------------------------------------------------------
!
        seisme = .false.
        iocs =0
        do 220 i = 1, 12
            mse(i) = 0.d0
220     continue
!
        call wkvect('&&RC3200.RESU2.'//lieu(im), 'V V R', 11, jresu2)
        do 40 kk = 1, 11
            zr(jresu2-1+kk) = 0.d0
 40     continue
!
        cfait = .false.
        do 20 ig = 1, nbgr
!
            numgr = situ_nume_group(ig)
            if (numgr .lt. 0) goto 20
!
            call jelira(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONUTI', nbsigr)
            call jeveuo(jexnum('&&RC3200.LES_GROUPES', numgr), 'L', jnsg)
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
                if (niv .ge. 2) write (ifm,200) ig, ioc1
!
                nsitup = situ_numero(ioc1)
                nsituq = 0
                nocc = situ_nb_occur(1+2*ioc1-2)
                ppi = situ_pres_a(ioc1)
                ppj = situ_pres_b(ioc1)
                call rcma02('A', ioc1, matpi)
                call rcma02('B', ioc1, matpj)
                call rcmo02('A', nsitup, mpi)
                call rcmo02('B', nsitup, mpj)
!
! ----------- CALCUL DU PM_PB
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
                        write (ifm,202) nsitup, pm, pb, pmpb
                    endif
                    zr(jresu2)=pm
                    zr(jresu2+1)=pb
                    zr(jresu2+2)=pmpb
                endif
!
! ----------- CALCUL DU SN
                if (lsn) then
                    sn = 0.d0
                    sn1 = 0.d0
                    sn2 = 0.d0
                    sp3 = 0.d0
                    spmeca3 = 0.d0
                    call rcZ2sn1(ze200, lieu(im), nsitup, nsitup, iocs, mse,&
                                 ppi, mpi, ppj, mpj, instsn, sn1, sp3, spmeca3)
                    if (ze200) call rcZ2s2('SN', ppi, mpi, ppj, mpj,&
                                           seisme, mse, sn2)
                    sn=sn1+sn2
                    zr(jress-1+10*(is1-1)+4) = sn
                    if (niv .ge. 2) then
                        write (ifm,203) nsitup, sn
                        if (instsn(1) .lt. 0.d0) then
                            write (ifm,*) '                SANS INSTANTS'
                        else
                            write (ifm,*) '                INSTANTS     : ',instsn
                        endif
                    endif
                    zr(jresu2+4)=sn
                endif
!
! ----------- CALCUL DU SN*
                if (lsn .and. lther) then
                    snet = 0.d0
                    call rc32sn('SN*_SITU', lieu(im), nsitup, ppi, mpi,&
                                nsituq, ppj, mpj, seisme, mse, snet)
                    zr(jress-1+10*(is1-1)+5) = snet
                    if (niv .ge. 2) then
                        write (ifm,232) nsitup, snet
                    endif
                    zr(jresu2+5)=snet
                endif
!
! ----------- CALCUL DU ROCHET THERMIQUE
                if (lther) then
                    call rc32rt(lieu(im), ppi, ppj, simpij)
                    write (ifm,234) nsitup, simpij
                    zr(jresu2+9)=simpij
                endif
                if (.not.lfat) goto 210
!
! ----------- CALCUL DU SP
                sp(1) = 0.d0
                sp(2) = 0.d0
                spmeca(1) = 0.d0
                spmeca(2) = 0.d0
                spmeca1(1) = 0.d0
                spmeca1(2) = 0.d0
                sp2 = 0.d0
                call rc32sp(ze200, lieu(im), nsitup, nsitup, iocs, mse,&
                             ppi, mpi, ppj, mpj, instsp, sp1, spmeca1, noth)
                if (ze200) call rcZ2s2('SP', ppi, mpi, ppj, mpj,&
                                       seisme, mse, sp2)
                sp(1)=sp1(1)+sp2+sp3
                spmeca(1)=spmeca1(1)+sp2+spmeca3
                if (niv .ge. 2) write (ifm,240) nsitup, sp(1)
                if (instsp(1) .lt. 0.d0) then
                    write (ifm,*) '                SANS INSTANTS'
                else
                    write (ifm,*) '                INSTANTS     : ',instsp(1),',', instsp(2)
                endif
                zr(jress-1+10*(is1-1)+6) = sp(1)
                zr(jresu2+6) = sp(1)
                zr(jresu2+10) = max(0.0,sp(1)-spmeca(1))
!
! ----------- CALCUL DU SALT
                call rc32sa('SITU', mater, matpi, matpj, sn, sp, spmeca,&
                            kemeca, kether, saltij, smm, fuij)
                if (niv .ge. 2) then
                    write (ifm,250) nsitup, saltij(1)
                endif
                zr(jress-1+10*(is1-1)+7) = kemeca
                zr(jress-1+10*(is1-1)+8) = kether
                zr(jress-1+10*(is1-1)+9) = saltij(1)
!
                zr(jresu2+7)= kemeca
                zr(jresu2+8) = saltij(1)
                zr(jresu2+3) = smm
!
! ----------- CALCUL DU FACTEUR D'USAGE
                upart = dble(nocc) * fuij(1)
                zr(jress-1+10*(is1-1)+10) = upart
                if (niv .ge. 2) then
                    write (ifm,260) nsitup, upart
                endif
!
                if (lefat) then
                    call getvtx(' ', 'TYPE_KE', scal=typeke, nbret=nb)
                    if (typeke .eq. 'KE_MECA') then
                        ke = kemeca
                    else
                        ke = (kemeca*spmeca(1)+kether*(sp(1)-spmeca(1)))/sp(1)
                    endif
                    call rc32env(nsitup, nsitup, ke, lieu(im),feni)
                    utotenv = utotenv + upart*feni
                endif
                utot = utot + upart
!
210         continue
20     continue
!
! ----------------------------------------------------------------------
!                           E T A P E   3
!               ON TRAITE LES SITUATIONS DE PASSAGE
! ----------------------------------------------------------------------
!
     call wkvect('&&RC3200.RESU3.'//lieu(im), 'V V R', 11, jresu3)
     do 50 ll = 1, 11
        zr(jresu3-1+ll) = 0.d0
 50 continue
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
                write (ifm,304)
                write (ifm,302) (situ_numero(1+zi(jnsg+i1-1)-1),i1=1,&
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
            ndim = max(12,12*nbsigr*(nbsigr-1)/2)
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
            yapass = .true.
!
            call rc3201(ze200, numgr, lpmpb, lsn, lther, lfat, lefat, yapass,&
                        seisme, iocs, mater, lieu(im), utot, utotenv,&
                        zr(jreas), zr(jress), zr(jreca), zr(jrecs),&
                        zr(jfact), zr(jresu3))
!
310     continue
        if (lfat) write (ifm,270) utot
!
! ----------------------------------------------------------------------
!               ON STOCKE LES RESULTATS TYPE MAXI :
! PM, PB, PMPB, SM, SN/3SM, SN, SN*, SP, KE, SALT, FACTEUR D'USAGE TOTAL, 
!      FACTEUR D'USAGE ENVIRONNEMENTAL TOTAL, ROCHET THERMIQUE(2)
! ----------------------------------------------------------------------
!
        call wkvect('&&RC3200.RESU.'//lieu(im), 'V V R', 14, jresu)
!------ PM
        zr(jresu) = max(zr(jresu1), zr(jresu2), zr(jresu3))
!------ PB
        zr(jresu+1) = max(zr(jresu1+1), zr(jresu2+1), zr(jresu3+1))
!------ PMPB
        zr(jresu+2) = max(zr(jresu1+2), zr(jresu2+2), zr(jresu3+2))
!------ SN
        zr(jresu+5) = max(zr(jresu1+4), zr(jresu2+4), zr(jresu3+4))
!------ SN*
        zr(jresu+6) = max(zr(jresu1+5), zr(jresu2+5), zr(jresu3+5))
!------ SP
        zr(jresu+7) = max(zr(jresu1+6), zr(jresu2+6), zr(jresu3+6))
!------ KE
        zr(jresu+8) = max(zr(jresu1+7), zr(jresu2+7), zr(jresu3+7))
!------ SALT
        zr(jresu+9) = max(zr(jresu1+8), zr(jresu2+8), zr(jresu3+8))
!------ FU TOTAL
        zr(jresu+10) = utot
!------ FU TOTAL ENV
        zr(jresu+11) = utotenv
!------ ROCHET THERMIQUE SIMPIJ
        zr(jresu+12) = max(zr(jresu1+9), zr(jresu2+9), zr(jresu3+9))
!------ ROCHET THERMIQUE SPTHER
        zr(jresu+13) = max(zr(jresu1+10), zr(jresu2+10), zr(jresu3+10))
!------ SM en fonction du SALT MAX
        if (zr(jresu+9) .eq. zr(jresu1+8)) then
            zr(jresu+3) = zr(jresu1+3)
        else if(zr(jresu+9) .eq.  zr(jresu2+8)) then
            zr(jresu+3) = zr(jresu2+3)
        else
            zr(jresu+3) = zr(jresu3+3)
        endif
!------ SN/3SM
        if (zr(jresu+3) .eq. 0) then
            zr(jresu+4) = 0.d0
        else
            zr(jresu+4) = zr(jresu+5) / ( 3 * zr(jresu+3))
        endif
!
 10 continue
!
    300 format (/,'=> GROUPE: ',i4,' , NOMBRE DE SITUATIONS: ',i4)
    302 format ('=> LISTE DES NUMEROS DE SITUATION: ',100 (i4,1x))
    304 format (/,'=> SITUATION DE PASSAGE')
    200 format ('=> GROUPE: ',i4,' , SITUATION: ',i4)
    202 format (1p,' SITUATION ',i4,' PM =',e12.5,&
     &                            ' PB =',e12.5,' PMPB =',e12.5)
    203 format (1p,' SITUATION ',i4,' SN =',e12.5 )
    232 format (1p,' SITUATION ',i4,' SN* =',e12.5 )
    234 format (1p,' SITUATION ',i4,' ROCHET THERMIQUE =',e12.5 )
    240 format (1p,' SITUATION ',i4,' SP =',e12.5)
    250 format (1p,' SITUATION ',i4,' SALT =',e12.5)
    260 format (1p,' SITUATION ',i4,' FACT_USAGE =',e12.5)
    270 format (1p,' SOMME(FACT_USAGE) =',e12.5)
!
    call jedema()
end subroutine
