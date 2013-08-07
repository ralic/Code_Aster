subroutine op0091()
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
    implicit none
!---------------------------------------------------------------------C
!--                                                                 --C
!--   OPERATEUR CALC_CORR_SSD   M. CORUS - AOUT 2011                --C
!--                                                                 --C
!--    CALCUL DES TRAVAUX AUX INTERFACEs ET SUR L'INTERIEUR DES     --C
!--       SOUS STRUCTURES DANS LE CAS D'UN CALCUL MODAL AVEC UN     --C
!--       MODELE REDUIT                                             --C
!--    CALCUL DES ENRICHISSEMENTS ASSOCIES POUR AMELIORER LA        --C
!--       QUALITE DU MODELE REDUIT                                  --C
!--                                                                 --C
!---------------------------------------------------------------------C
!
!
!
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/r8pi.h"
#include "asterfort/arch93.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lceqvn.h"
#include "asterfort/libint.h"
#include "asterfort/lipsrb.h"
#include "asterfort/modexp.h"
#include "asterfort/mtdscr.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/rotlir.h"
#include "asterfort/rsadpa.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/titre.h"
#include "asterfort/traint.h"
#include "asterfort/trasst.h"
#include "asterfort/u2mesk.h"
#include "asterfort/vecind.h"
#include "asterfort/vecomo.h"
#include "asterfort/vpcrea.h"
#include "asterfort/wkvect.h"
!
!
    character(len=4) :: k4bid, num4l
    character(len=8) :: nomres, modgen, resgen, sst1, sst2, intf1, intf2, kb
    character(len=8) :: rest1, mraid, mmass, vk(3)
    character(len=16) :: nomcmd, typres
    character(len=14) :: nume14
    character(len=19) :: imped, nume91, solveu, nompar(4), typpar(4)
    character(len=24) :: indin1, indin2, lino1, lino2, tramo1, tramo2
    integer :: i1, l1, ibid, lmodge, lresge, nblia, lllia, iret, nbsst, llipr
    integer :: j1, k1, imast1, imast2, nbeq1, nbeq2, ddla1, ddla2, nbmod, lnosst
    integer :: lnusst, isst1, nl, nc, nbddl1, tach1, nbexp, lomeg, lmod1, lmass
    integer :: lbid, ltrsst, nbsla, lraid, leff1, leff2, ltrain, lintf, nbint
    integer :: lcopy1, lsecme, limped, unit, lmasst, nbmas, lslast, nindep, jadr
    integer :: ltramo, lmatro, lobsro, kk1, ll1
    real(kind=8) :: trvint, rbid, vr(2), temp
    complex(kind=8) :: cbid
    character(len=24) :: lisint, modet
    integer :: iarg
!     -----------------------------------------------------------------
    call jemarq()
    call getres(nomres, typres, nomcmd)
    call getvis(' ', 'UNITE', 1, iarg, 1,&
                unit, ibid)
    call getvid(' ', 'MODELE_GENE', 1, iarg, 1,&
                modgen, lmodge)
    call getvid(' ', 'RESU_GENE', 1, iarg, 1,&
                resgen, lresge)
!
!-- INITIALISATION DE LA TABLE_CONTAINER
    call detrsd('TABLE_CONTAINER', nomres)
    call tbcrsd(nomres, 'G')
    nompar(1)='NOM_SST'
    typpar(1)='K8'
    nompar(2)='INTERF'
    typpar(2)='K8'
    nompar(3)='NOM_SD'
    typpar(3)='K8'
    call tbajpa(nomres, 3, nompar, typpar)
!
!-- RECUPERATION DES INFOS
    call jelira(modgen//'      .MODG.LIDF', 'NMAXOC', nblia)
    call jeveuo(modgen//'      .MODG.LIPR', 'L', llipr)
    call jelira(resgen//'           .ORDR', 'LONMAX', nbmod)
    call wkvect('&&OP0091.PULSA_PROPRES', 'V V R', nbmod, lomeg)
    do 30 i1 = 1, nbmod
        call rsadpa(resgen, 'L', 1, 'FREQ', i1,&
                    0, jadr, kb)
        zr(lomeg+i1-1)=2*r8pi()*zr(jadr)
30  end do
!
    call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst)
    call wkvect('&&OP0091.NOM_SST', 'V V K8', nbsst, lnosst)
    call wkvect('&&OP0091.NUME_SST', 'V V I', nbsst+1, lnusst)
    call wkvect('&&OP0091.MATRICE_MASS', 'V V I', nbsst, lmass)
    call wkvect('&&OP0091.MATRICE_RAID', 'V V I', nbsst, lraid)
    call wkvect('&&OP0091.TRAV_INTERF', 'V V R', nblia*nbmod, ltrain)
    call wkvect('&&OP0091.TRAV_SST', 'V V R', nbsst*nbmod, ltrsst)
    call wkvect('&&OP0091.NUM_SST_ESCLAVE', 'V V I', nblia, lslast)
    call wkvect('&&OP0091.NUM_SST_MAITRE', 'V V I', nblia, lmasst)
!
!----------------------------------------------------C
!--                                                --C
!--      CALCUL DES TRAVAUX SUR LES INTERFACES     --C
!--                                                --C
!----------------------------------------------------C
    do 20 i1 = 1, nblia
!-- RECUPERATION DES INFOS DE LA LIAISON
        call jeveuo(jexnum(modgen//'      .MODG.LIDF', i1), 'L', lllia)
        sst1=zk8(lllia)
        intf1=zk8(lllia+1)
        sst2=zk8(lllia+2)
        intf2=zk8(lllia+3)
        write(unit,*)' LIAISON',i1,' : ',sst1,'/',sst2
        write(unit,*)'     ->',intf1,'/',intf2
!-- GESTION DE L'INCOMPATIBILITE
        iret=i1
        call vecomo(modgen, sst1, sst2, intf1, intf2,&
                    iret, 'REDUIT  ')
!-- SOUS STRUCTURE 1
        lino1='&&VECT_NOEUD_INTERF1'
        indin1='&&VEC_DDL_INTF_'//intf1
        tramo1='&&MATR_TRACE_MODE_INT1'
        nbeq1=zi(llipr+(i1-1)*9+1)
!-- SOUS STRUCTURE 2
        lino2='&&VECT_NOEUD_INTERF2'
        indin2='&&VEC_DDL_INTF_'//intf2
        tramo2='&&MATR_TRACE_MODE_INT2'
        nbeq2=zi(llipr+(i1-1)*9+4)
!-- RECHERCHE DE LA SOUS STRUCTURE ESCLAVE DE LA LIAISON EN CHERCHANT LE
!-- FACTEUR MULTIPLICATEUR DANS LA MATRICE DE LIAISON LAGRANGE/LAGRANGE
        call jeveuo(jexnum(modgen//'      .MODG.LIMA', zi(llipr+(i1-1)*9+8)), 'L', ibid)
!-- L'INTERFACE MAITRE EST CELLE DONT LE NUMERO EST NEGATIF
        if (int(zr(ibid)) .eq. -2) then
            imast1=1
            imast2=-2
        else
            imast2=2
            imast1=-1
        endif
!--
!-- CONSTRUCTION DES MATRICES D'OBSERVATION DE L'INTERFACE
!--
!-- RECUPERATION DES MOUVEMENTS DE L'INTERFACE MAITRE, ET PROJECTION
!--   SUR L'INTERFACE ESCLAVE POUR RELEVEMENT STATIQUE ULTERIEUR
!-- POUR CONSTRUIRE C, IL FAUT D'ABORD CONSTRUIRE LA MATRICE
!--   D'OBSERVATION A PARTIR D'UNE IDENTITE ET ENSUITE APPLIQUER
!--   LA ROTATION
        call rotlir(modgen, sst1, intf1, lino1, 0,&
                    indin1, tramo1, ddla1, nbeq1, min(imast1, 0),&
                    i1)
        call rotlir(modgen, sst2, intf2, lino2, iret,&
                    indin2, tramo2, ddla2, nbeq2, min(imast2, 0),&
                    i1)
        kb='        '
        if (imast1 .eq. -1) then
            if (iret .eq. 0) then
                nbeq1=ddla1
                nc=ddla1
                nl=ddla2
                call lipsrb(modgen, kb, sst1, sst2, intf1,&
                            intf2, lino1, lino2, indin1, indin2,&
                            ddla1, ddla2, nbeq1, nbeq2, -imast1,&
                            tramo1)
                call jeveuo(tramo1, 'E', ltramo)
            endif
        else
            if (iret .eq. 0) then
                nbeq2=ddla2
                nc=ddla2
                nl=ddla1
                call lipsrb(modgen, kb, sst1, sst2, intf1,&
                            intf2, lino1, lino2, indin1, indin2,&
                            ddla2, ddla1, nbeq2, nbeq1, -imast2,&
                            tramo2)
                call jeveuo(tramo2, 'E', ltramo)
            endif
        endif
!
        call wkvect('&&OP0091.VEC_OBS_TEMP_RO', 'V V R', nl*nc, lobsro)
        call jeveuo('&&ROTLIR.MATR_ROTATION', 'L', lmatro)
        do 455 j1 = 1, nc
            do 465 k1 = 1, nl
                temp=zr(ltramo+(j1-1)*nl + k1-1 )
                do 475 l1 = 1, nl
                    kk1=(k1-1)/3
                    ll1=(l1-1)/3
                    if (kk1 .eq. ll1) then
                        rbid=zr(lmatro+ (k1-kk1*3 -1)*3+ (l1-ll1*3 -1)&
                        )
                        zr(lobsro+(j1-1)*nl+ l1-1 )= zr(lobsro+(j1-1)*&
                        nl+ l1-1 )+temp*rbid
                    endif
475              continue
465          continue
455      continue
        call lceqvn(nl*nc, zr(lobsro), zr(ltramo))
        call jedetr('&&OP0091.VEC_OBS_TEMP_RO')
        call jedetr('&&ROTLIR.MATR_ROTATION')
!-- CALCUL DES TRAV. D'INTERF. ET DES EFFORTS POUR CORRECTION ULTERIEURE
        call traint(resgen, modgen, i1, sst1, sst2,&
                    intf1, intf2, nbmod, nl, nc)
!-- EXPANSION DES MODES ESCLAVES SUR L'INTERFACE MAITRE
        call codent(i1, 'D0', k4bid)
        if (imast1 .eq. -1) then
            modet='&&OP0091.MET'//k4bid//sst1
            call modexp(modgen, sst1, indin1, lino1, nbmod,&
                        i1, tramo1, modet)
        else
            modet='&&OP0091.MET'//k4bid//sst2
            call modexp(modgen, sst2, indin2, lino2, nbmod,&
                        i1, tramo2, modet)
        endif
!-- DESTRUCTION DES CONCEPTS TEMPORAIRES
        call jedetr(lino1)
        call jedetr(tramo1)
        call jedetr(lino2)
        call jedetr(tramo2)
        call jedetr('&&OP0091.MODE_SST1')
        call jedetr('&&OP0091.MODE_SST2')
        call jedetr('&&OP0091.MODE_SST1_EFF')
        call jedetr('&&OP0091.MODE_SST2_EFF')
        call jedetr('&&OP0091.MODE_SST1_EFFWI')
        call jedetr('&&OP0091.MODE_SST2_EFFWI')
        call jedetr('&&OP0091.MODE_SST1_COPY')
        call jedetr('&&OP0091.MODE_SST2_COPY')
        call jedetr('&&OP0091.OBS_01')
        call jedetr('&&OP0091.OBS_02')
        call jedetr('&&OP0091.LAGRANGES_1')
        call jedetr('&&OP0091.LAGRANGES_2')
        write(unit,*)'  '
!-- FIN DE LA BOUCLE SUR LES LIAISONS
20  end do
!
!-------------------------------------------------------------C
!--                                                         --C
!--      CALCUL DES TRAVAUX SUR LES SOUS STRUCTURES         --C
!--                                                         --C
!-------------------------------------------------------------C
    call wkvect('&&OP0091.INTERFACES', 'V V K8', 2*nblia, lintf)
    do 40 i1 = 1, nbsst
        nbint=0
        sst1=zk8(lnosst+i1-1)
        call jenonu(jexnom(modgen//'      .MODG.SSNO', sst1), isst1)
        write(unit,*)' '
        write(unit,*)' SOUS STRUCTURE : ',sst1
!-- RECHERCHE DES INTERFACES ASSOCIEES A CETTE SST
        do 50 j1 = 1, nblia
            call jeveuo(jexnum(modgen//'      .MODG.LIDF', j1), 'L', lllia)
            if (zk8(lllia) .eq. sst1) then
                zk8(lintf+nbint)=zk8(lllia+1)
                write(unit,*)'   ->',zk8(lintf+nbint)
                nbint=nbint+1
            endif
            if (zk8(lllia+2) .eq. sst1) then
                zk8(lintf+nbint)=zk8(lllia+3)
                write(unit,*)'   ->',zk8(lintf+nbint)
                nbint=nbint+1
            endif
50      continue
!-- RECUPERATION DES MODES
        call jelira('&&VEC_DDL_INTF_'//zk8(lintf), 'LONMAX', nbddl1)
        call codent(i1, 'D0', k4bid)
        rest1='&&91'//k4bid
        call jeveuo(jexnum(rest1//'           .TACH', 1), 'L', tach1)
!-- CONSTRUCTION DES OBJETS TEMPORAIRES
        call jelira(zk24(tach1)(1:19)//'.VALE', 'LONMAX', nbeq1)
        call wkvect('&&OP0091.MODE_SST1', 'V V R', nbeq1, lmod1)
        call wkvect('&&OP0091.MODE_SST1_EFF1', 'V V R', nbeq1, leff1)
        call wkvect('&&OP0091.MODE_SST1_EFF2', 'V V R', nbeq1, leff2)
        call wkvect('&&OP0091.MODE_SST1_COPY', 'V V R', nbeq1, lcopy1)
!-- ALLOCATION POUR PERMETTRE L'ACHIVAGE
        nbsla=0
        nbmas=0
        call wkvect('&&OP0091.ESCLAVE_LIASON', 'V V I', nblia, lbid)
        call wkvect('&&OP0091.MAITRE_LIASON', 'V V I', nblia, ibid)
        do 440 j1 = 1, nblia
            if (zi(lslast+j1-1) .eq. i1) then
                nbsla=nbsla+1
                zi(lbid+nbsla-1)=j1
            endif
            if (zi(lmasst+j1-1) .eq. i1) then
                nbmas=nbmas+1
                zi(ibid+nbmas-1)=j1
            endif
440      continue
        call wkvect('&&OP0091.MODE_INTF_DEPL', 'V V R', (2+nbsla+nbmas) *nbeq1*nbmod, lsecme)
!-- RECOPIE DES DEPLACEMENTS IMPOSES
        do 460 j1 = 1, nbsla
            call codent(zi(lbid+j1-1), 'D0', num4l)
            call jeveuo('&&OP0091.DEPL_IMPO_'//num4l, 'L', ibid)
            do 470 k1 = 1, nbmod
                call lceqvn(nbeq1, zr(ibid+(k1-1)*nbeq1),&
                            zr(lsecme + ( 2+j1-1)*nbmod*nbeq1 + (k1-1)*nbeq1))
470          continue
460      continue
        call jedetr('&&OP0091.ESCLAVE_LIASON')
        call jedetr('&&OP0091.MAITRE_LIASON')
!-- CALCUL DES TRAVAUX POUR L'INTERIEUR DES SST ET EFFORTS ASSOCIES
        lisint='&&OP0091.INTERFACES'
        call trasst(modgen, i1, isst1, lisint, nbeq1,&
                    nbmod, nbint)
        call jedetr('&&OP0091.MODE_SST1')
        call jedetr('&&OP0091.MODE_SST1_EFF1')
        call jedetr('&&OP0091.MODE_SST1_EFF2')
        call jedetr('&&OP0091.MODE_SST1_COPY')
!-- CALCUL DES LA REPONSE AUX EFFORTS "INTERIEURS"
        call codent(i1, 'D0', k4bid)
        imped='&&OP0091.IMPED'//k4bid
        call jeveuo(jexnum(modgen//'      .MODG.SSME', isst1), 'L', ibid)
        call jeveuo(zk8(ibid)//'.MAEL_MASS_REFE', 'L', lbid)
        mmass=zk24(lbid+1)(1:8)
        call jeveuo(zk8(ibid)//'.MAEL_RAID_REFE', 'L', lbid)
        mraid=zk24(lbid+1)(1:8)
        call dismoi('F', 'SOLVEUR', mraid, 'MATR_ASSE', ibid,&
                    solveu, ibid)
        call resoud(imped, ' ', solveu, ' ', nbmod,&
                    ' ', ' ', ' ', zr(lsecme), cbid,&
                    ' ', .true., 0, iret)
!-- CALCUL DE LA REPONSE AUX DEPLACEMENTS D'INTERFACE
        do 450 k1 = 1, nbsla
            lbid=lsecme+2*nbeq1*nbmod
            call resoud(imped, ' ', solveu, ' ', nbmod,&
                        ' ', ' ', ' ', zr(lbid+nbeq1*nbmod*(k1-1)), cbid,&
                        ' ', .true., 0, iret)
450      continue
!-- "DEBLOQUAGE" DES DDL DE LAGRANGE ASSOCIES AUX INTERFACES DE LISINT
!--   DANS LA MATRICE IMPED
        call dismoi('F', 'NOM_NUME_DDL', mraid, 'MATR_ASSE', ibid,&
                    nume91, ibid)
        call libint(imped, nume91, nbint, lisint, nbeq1)
!-- FACTORISATION DE LA MATRICE INTERFACE LIBRE
        call mtdscr(imped)
        call jeveuo(imped(1:19)//'.&INT', 'E', limped)
        call preres(solveu, 'V', iret, '&&OP0091.MATPRE', imped,&
                    ibid, -9999)
        if (iret .eq. 2) call u2mesk('F', 'ALGELINE4_37', 1, imped)
!-- CALCUL DE LA REPONSE AUX EFFORTS D'INTERFACE
        call resoud(imped, ' ', solveu, ' ', nbmod,&
                    ' ', ' ', ' ', zr(lsecme+nbeq1*nbmod), cbid,&
                    ' ', .true., 0, iret)
!-- RECOPIE DES MODES ETENDUS A LA SUITE DES CORRECTIONS A INTERF. LIBRE
        call jedetc('V', imped, 1)
        nbexp=0
        do 260 j1 = 1, nblia
            call codent(j1, 'D0', k4bid)
            call jeexin('&&OP0091.MET'//k4bid//sst1, iret)
            if (iret .gt. 0) then
                call jeveuo('&&OP0091.MET'//k4bid//sst1, 'L', lbid)
                call lceqvn(nbeq1*nbmod, zr(lbid), zr(lsecme+(2+nbsla+ nbexp)*nbeq1*nbmod))
                nbexp=nbexp+1
            endif
260      continue
!---------------C
!--           --C
!-- ARCHIVAGE --C
!--           --C
!---------------C
!--
!-- VECTEURS A INTERFACE FIXE
!--
!-- ORTHONORMALISATION DES MODES A INTERFACE FIXE
        call mtdscr(mmass)
        call vecind(mmass//'           ', lsecme, nbeq1, nbmod, 1,&
                    nindep)
!-- RECOPIE DANS LA MATICE POUR ARCHIVAGE
        call wkvect('&&MOIN93.MODE_INTF_DEPL', 'V V R', nbeq1*nindep, lbid)
        call lceqvn(nbeq1*nindep, zr(lsecme), zr(lbid))
        call wkvect('&&MOIN93.FREQ_INTF_DEPL', 'V V R', nindep, lbid)
        do 270 j1 = 1, nindep
            zr(lbid+j1-1)=j1
270      continue
!  NOMMAGE AUTOMATIQUE DU CONCEPT RESULTAT
        vk(1)=sst1
        vk(2)='ENCAS'
        call gcncon('_', vk(3))
        call vpcrea(0, vk(3), mmass, ' ', mraid,&
                    nume91, ibid)
        nume14=nume91(1:14)
        call arch93(vk(3), 'MODE_MECA       ', nume14, mraid//'           ', 0,&
                    0, 0, 0, nindep, 0)
        call jedetr('&&MOIN93.MODE_INTF_DEPL')
        call jedetr('&&MOIN93.FREQ_INTF_DEPL')
!
        call tbajli(nomres, 3, nompar, ibid, rbid,&
                    cbid, vk, 0)
!--
!-- VECTEURS A INTERFACE LIBRE
!--
!-- ORTHONORMALISATION DES MODES A INTERFACE LIBRE
        ibid=nbmod*(1+nbsla+nbmas)
        call vecind(mmass//'           ', lsecme+nbeq1*nbmod, nbeq1, ibid, 1,&
                    nindep)
!-- RECOPIE DANS LA MATICE POUR ARCHIVAGE
        call wkvect('&&MOIN93.MODE_INTF_DEPL', 'V V R', nbeq1*nindep, lbid)
        call lceqvn(nbeq1*nindep, zr(lsecme+nbeq1*nbmod), zr(lbid))
!
        call wkvect('&&MOIN93.FREQ_INTF_DEPL', 'V V R', nindep, lbid)
        do 370 j1 = 1, nindep
            zr(lbid+j1-1)=j1
370      continue
!--  NOMMAGE AUTOMATIQUE DU CONCEPT RESULTAT
        vk(1)=sst1
        vk(2)='LIBRE'
        call gcncon('_', vk(3))
        call vpcrea(0, vk(3), mmass, ' ', mraid,&
                    nume91, ibid)
        call arch93(vk(3), 'MODE_MECA       ', nume14, mraid//'           ', 0,&
                    0, 0, 0, nindep, 0)
        call tbajli(nomres, 3, nompar, ibid, rbid,&
                    cbid, vk, 0)
!-- PETIT MENAGE
        call jedetr('&&MOIN93.MODE_INTF_DEPL')
        call jedetr('&&MOIN93.FREQ_INTF_DEPL')
        call jedetr('&&OP0091.MODE_INTF_DEPL')
!-- FIN DE LA BOUCLE SUR LES SOUS STRUCTURES
40  end do
!-- VERIFICATION D'UN TRAVAIL TOTAL NUL
    nompar(1)='TRAV_TOTAL'
    typpar(1)='R'
    nompar(2)='NUM_MODE'
    typpar(2)='I'
    call tbajpa(nomres, 2, nompar, typpar)
    write(unit,*)' '
    do 200 i1 = 1, nbmod
        trvint=0
        do 210 j1 = 1, nbsst
            trvint=trvint+zr(ltrsst+nbmod*(j1-1)+i1-1)
210      continue
        do 220 j1 = 1, nblia
            trvint=trvint+zr(ltrain+nbmod*(j1-1)+i1-1)
220      continue
        write(unit,*)'MODE ',i1,' - TRAVAIL TOTAL :',trvint
        call tbajli(nomres, 2, nompar, i1, trvint,&
                    cbid, kb, 0)
200  end do
!-- ARCHIVAGE DES TRAVAUX INTERFACES
    nompar(1)='TRAV_INTERF'
    typpar(1)='R'
    nompar(2)='NOM_INTERF'
    typpar(2)='K8'
    nompar(3)='NUM_MODE'
    typpar(3)='I'
    call tbajpa(nomres, 3, nompar, typpar)
    do 300 j1 = 1, nbmod
        do 310 i1 = 1, nblia
            call jeveuo(jexnum(modgen//'      .MODG.LIDF', i1), 'L', lllia)
            intf1=zk8(lllia+1)
            intf2=zk8(lllia+3)
            vr(1)=zr(ltrain+nbmod*(i1-1)+j1-1)
            call tbajli(nomres, 3, nompar, j1, vr,&
                        cbid, intf1, 0)
            call tbajli(nomres, 3, nompar, j1, vr,&
                        cbid, intf2, 0)
310      continue
300  end do
!-- ARCHIVAGE DES TRAVAUX SOUS STRUCTURES
    nompar(1)='TRAV_SST'
    typpar(1)='R'
    nompar(2)='NOM_SST'
    typpar(2)='K8'
    nompar(3)='NUM_MODE'
    typpar(3)='I'
    call tbajpa(nomres, 3, nompar, typpar)
    do 320 j1 = 1, nbmod
        do 330 i1 = 1, nbsst
            vk(1)=zk8(lnosst+i1-1)
            vr(1)=zr(ltrsst+nbmod*(i1-1)+j1-1)
            call tbajli(nomres, 3, nompar, j1, vr,&
                        cbid, vk, 0)
330      continue
320  end do
!-- MENAGE DANS LES CONCEPTS TEMPORAIRES
    call jedetr('&&OP0091.INTERFACES')
    call jedetr('&&OP0091.NOM_SST')
    call jedetr('&&OP0091.MATRICE_MASS')
    call jedetr('&&OP0091.MATRICE_RAID')
    call jedetr('&&OP0091.TRAV_INTERF')
    call jedetr('&&OP0091.TRAV_SST')
    call jedetr('&&OP0091.NUM_SST_ESCLAVE')
    call jedetr('&&OP0091.PULSA_PROPRES')
!-- DESTRUCTION DES CONCEPTS "RESTITUTION"
    do 70 i1 = 1, zi(lnusst)
        call codent(i1, 'D0', k4bid)
        call jedetc('G', '&&91'//k4bid, 1)
70  end do
!
    call jedetc('V', '&&OP0091', 1)
    call jedetc('V', '&&VEC_DDL_INTF', 1)
!
    call titre()
    call jedema()
end subroutine
