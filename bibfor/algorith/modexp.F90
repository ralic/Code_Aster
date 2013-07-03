subroutine modexp(modgen, sst1, indin1, lino1, nbmod,&
                  numlia, tramod, modet)
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
!-------------------------------------------------------------C
!--                                                         --C
!--  ROUTINE QUI REALISE L'EXPANSION DES MODES D'INTERFACE  --C
!--    DE L'INTERFACE ESCLAVE VERS L'INTERFACE MAITRESSE    --C
!--                                                         --C
!-------------------------------------------------------------C
!--   VARIABLES E/S  :
!--   MODGEN   /IN/  : NOM DU MODELE GENERALISE
!--   SST1     /IN/  : NOM DE LA SOUS STRUCTURE
!--   INDIN1   /IN/  : NOM DU VECTEUR CONTENANT LES DDL D'INTERFACE
!--   LINO1    /IN/  : NOM DU VECTEUR CONTENANT LES NOEUDS D'INTERFACE
!--   NBMOD    /IN/  : NOMBRE DE MODE A ETENDRE
!--   NUMLIA   /IN/  : NUMERO DE LA LIAISON ASSOCIEE A L'INTERFACE
!--   TRAMOD   /IN/  : TRACE DES MODES SUR L'INTERFACE ESCLAVE
!--   MODET    /OUT/ : MODES ETENDUS SUR L'INTERFACE MAITRE
!
    implicit none
!
!
!
#include "jeveux.h"
!
#include "asterc/getvr8.h"
#include "asterc/matfpe.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/conint.h"
#include "asterfort/ddllag.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/modint.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/tri.h"
#include "asterfort/wkvect.h"
#include "blas/dgelss.h"
!
!
!
    integer :: nbmod, lbid, i1, j1, k1, isst1, ibid, lclef, lnum, nbeq1, nl, nc
    integer :: lmast, numlia, nbno, lnres, lmodet, sizeco, connec, lconnc, nbec
    integer :: lprno, ipos1, lcphi, nbddl, lnoint, lindin, llino, lindno, lipos
    integer :: ik, lddld, linlag, lintrf, linddl, nddlin, nbvect, ltramo, lmatmo
    integer :: lclin, rank, lwork, jwork, info, lphiex, lcpet
    real(kind=8) :: shift, swork(1)
    complex(kind=8) :: cbid
    character(len=1) :: k1bid
    character(len=4) :: k4bid
    character(len=8) :: modgen, sst1, kb
    character(len=14) :: nume, nume91
    character(len=19) :: raide, masse, solveu, prno, ssami, raiint
    character(len=24) :: coint, noddli, matmod, vefreq, indin1, lino1, tramod
    character(len=24) :: modet
    integer :: iarg, iret
!
!---------------------------------------------------C
!--                                               --C
!-- CALCUL DES MODES D'INTERFACE POUR L'EXPANSION --C
!--                                               --C
!---------------------------------------------------C
!
!-- RECUPERATION DES MATRICES DE MASSE ET RAIDEUR
    call jenonu(jexnom(modgen//'      .MODG.SSNO', sst1), isst1)
    call jeveuo(jexnum(modgen//'      .MODG.SSME', isst1), 'L', ibid)
    call jeveuo(zk8(ibid)//'.MAEL_RAID_REFE', 'L', lbid)
    raide=zk24(lbid+1)(1:19)
    call jeveuo(zk8(ibid)//'.MAEL_MASS_REFE', 'L', lbid)
    masse=zk24(lbid+1)(1:19)
!
!-- RECUPERATION DU NUME_DDL
    call dismoi('F', 'NOM_NUME_DDL', masse(1:8), 'MATR_ASSE', ibid,&
                nume, ibid)
    call dismoi('F', 'PROF_CHNO', nume, 'NUME_DDL', ibid,&
                prno, ibid)
    call jeveuo(jexnum(prno//'.PRNO', 1), 'L', lprno)
!
    call dismoi('F', 'NB_EQUA', raide, 'MATR_ASSE', nbeq1,&
                kb, ibid)
    call dismoi('F', 'NB_EC', 'DEPL_R', 'GRANDEUR', nbec,&
                kb, ibid)
!
!-- REMPLISSAGE DES VECTEURS D'INDICES ASSOCIES AUX DDL D'INTERFACE
!--      POUR LA CREATION DES MATRICES
    call jelira(indin1, 'LONMAX', nbddl, k1bid)
    call jeveuo(indin1, 'L', lindin)
    noddli='&&MOIN93.NOEUDS_DDL_INT'
    call jelira(lino1, 'LONMAX', nbno, k1bid)
    call jeveuo(lino1, 'L', llino)
!
    call wkvect(noddli, 'V V I', 9*nbno, lnoint)
    call wkvect('&&MOIN93.V_IND_LAG', 'V V I', 2*nbddl, linlag)
    call wkvect('&&MOIN93.DDL_ACTIF_INT', 'V V I', nbddl, lintrf)
    call wkvect('&&MOIN93.V_IND_DDL_INT', 'V V I', nbddl, linddl)
!
!-- LA LISTE DES NOEUDS D'INTERFACE DOIT ETRE ORDONNEE
    call wkvect('&&MODEXP.VECT_CLEFS', 'V V I', nbno, lclef)
    call wkvect('&&MODEXP.VECT_NUM', 'V V I', nbno, lnum)
!
    do 10 k1 = 1, nbno
        zi(lclef+k1-1)=k1
        zi(lnum+k1-1)=zi(llino+k1-1)
10  end do
    call tri(zi(lnum), zi(lclef), 1, nbno)
    do 30 k1 = 1, nbno
        ik=zi(lclef+k1-1)
        zi(lnoint+(k1-1))=zi(llino+ik-1)
        zi(lnoint+(k1-1)+nbno)=zi(lindin+(ik-1)*6)
        zi(lnoint+(k1-1)+2*nbno)=6
        do 40 j1 = 1, 6
            zi(lnoint+(k1-1)+((2+j1)*nbno))=j1
40      continue
30  end do
    call jedetr('&&MODEXP.VECT_CLEFS')
    call jedetr('&&MODEXP.VECT_NUM')
!
    call wkvect('&&MOIN93.IS_DDL_INTERF', 'V V I', nbeq1, lddld)
    k1=1
    do 50 i1 = 1, nbddl
        if (zi(lindin+i1-1) .gt. 0) zi(lddld+zi(lindin+i1-1)-1)=1
        ipos1=zi(lindin+i1-1)
        if (ipos1 .gt. 0) then
            zi(linddl+k1-1)=ipos1
            call ddllag(nume, ipos1, nbeq1, zi(linlag+(k1-1)*2), zi( linlag+(k1-1)*2+1))
            zi(lintrf+k1-1)=i1
            k1=k1+1
        endif
50  end do
    nddlin=k1-1
!
!-- CONSTRUCTION DES MATRICES DE MASSE ET DE RAIDEUR DU PROBLEME
!--    D'INTERFACE
    sizeco=36*nbno
    coint='&&MODEXP.CONNEC_INTERF'
    nume91='&&NUME91'
    raiint='&&RAID91'
    ssami='&&MASS91'
    call wkvect(coint, 'V V I', sizeco, lconnc)
    call wkvect('&&MOIN93.IND_NOEUD', 'V V I', zi(lnoint+nbno-1), lindno)
    call wkvect('&&MOIN93.IPOS_DDL_INTERF', 'V V I', nbno, lipos)
    call conint(nume, raide, coint, sizeco, connec,&
                noddli, nbno, nume91, raiint, ssami)
!
!-- CALCUL DES MODES DU MODELE D'INTERFACE
    call getvr8(' ', 'SHIFT', 1, iarg, 1,&
                shift, ibid)
    shift=-((shift*2.d0*3.1415927d0)**2)
    matmod='&&MODEXP.MATRICE_MODES'
    vefreq='&&MODEXP.VECTEUR_FREQ'
!
    call codent(numlia, 'D0', k4bid)
!-- MOUVEMENTS DE L'INTERFACE ESCLAVE A ETENDRE
    call jeveuo('&&OP0091.MAS'//k4bid, 'L', lmast)
    call jelira('&&OP0091.MAS'//k4bid, 'LONMAX', ibid, kb)
    nl=int(ibid/nbmod)
!
!-- MATRICE D'OBSERVATION
    call jelira(tramod, 'LONMAX', ibid, kb)
    nc=int(ibid/nl)
    call jeveuo(tramod, 'L', ltramo)
!
    nbvect=1
!
!-- ON BOUCLE POUR AVOIR UNE EXPANSION CORRECTE. TANT QUE C'EST PAS BON,
!-- ON ENRICHIT LA BASE DES MODES D'INTERFACE
500  continue
    call modint(ssami, raiint, nddlin, nbvect, shift,&
                matmod, masse, raide, nbeq1, coint,&
                noddli, nbno, vefreq, 0)
    call jeveuo(matmod, 'L', lmatmo)
!
!---------------------------C
!--                       --C
!-- EXPANSION DES DONNEES --C
!--                       --C
!---------------------------C
!
!-- TAILLES DES MATRICES
!--
!--  C      : NL x NC
!--  MATMOD : NC x NBVECT
!--  CPHI   : NL x NBVECT
!
!
!-- PRODUIT C*PHI_mast
!
    call matfpe(-1)
    call wkvect('&&MODEXP.CPHI', 'V V R', nl*nbvect, lcphi)
!
    do 300 j1 = 1, nbvect
        do 310 k1 = 1, nc
            do 320 i1 = 1, nl
                zr(lcphi+(j1-1)*nl+i1-1)=zr(lcphi+(j1-1)*nl+i1-1)+&
                zr(ltramo+(k1-1)*nl+(i1-1))*zr(lmatmo+(j1-1)*nc+(k1-1)&
                )
320          continue
310      continue
300  end do
!      CALL DGEMM('N','N',NL,NBVECT,NC,1.,ZR(LTRAMO),
!     &            NL,ZR(LMATMO),NC,0.,ZR(LCPHI),NL)
!
!-- EXPANSION DE DONNEES
    ibid=max(nl,nbvect)
    call wkvect('&&MODEXP.COMB_LIN', 'V V R', ibid*nbmod, lclin)
!
!-- RECOPIE A LA MAIN POUR ETRE COMPATIBLE AVEC LES TAILLES
    do 60 j1 = 1, nbmod
        do 70 i1 = 1, nl
            zr(lclin+(j1-1)*max(nl,nbvect)+(i1-1)) = zr(lmast+(j1-1)* nl+(i1-1) )
70      continue
60  end do
!
    call wkvect('&&MODEXP.VEC_VAL_SING', 'V V R', min(nl, nbvect), ibid)
!
    call dgelss(nl, nbvect, nbmod, zr(lcphi), nl,&
                zr(lclin), max(nl, nbvect), zr(ibid), -1.0d0, rank,&
                swork, -1, info)
!
    lwork=int(swork(1))
    call wkvect('&&MODEXP.MAT_AXB', 'V V R', lwork, jwork)
!
    call dgelss(nl, nbvect, nbmod, zr(lcphi), nl,&
                zr(lclin), max(nl, nbvect), zr(ibid), 1.0d-12, rank,&
                zr(jwork), lwork, info)
!
!-- MOUVEMENTS DE L'INTERFACE
    call wkvect('&&MODEXP.PHI_EXP', 'V V R', nc*nbmod, lphiex)
!
    do 450 j1 = 1, nbmod
        do 460 k1 = 1, nbvect
            do 470 i1 = 1, nc
                zr(lphiex+(j1-1)*nc+i1-1)=zr(lphiex+(j1-1)*nc+i1-1)+&
                zr(lmatmo+(k1-1)*nc+(i1-1))*zr(lclin+(j1-1)*nbvect+(&
                k1-1))
470          continue
460      continue
450  end do
!
!-- PROJECTION POUR VERIFIER L'EXPANSION
    call wkvect('&&MODEXP.C_PHI_EXP', 'V V R', nl*nbmod, lcpet)
!
    do 150 j1 = 1, nbmod
        do 160 k1 = 1, nc
            do 170 i1 = 1, nl
                zr(lcpet+(j1-1)*nl+i1-1)=zr(lcpet+(j1-1)*nl+i1-1)+&
                zr(ltramo+(k1-1)*nl+(i1-1))*zr(lphiex+(j1-1)*nc+(k1-1)&
                )
170          continue
160      continue
150  end do
!
!-- VERIFICATION DE LA QUALITE DE L'EXPANSION
!
    call wkvect('&&MODEXP.NORM_RESIDU', 'V V R', nbmod, lnres)
!
!-- NORME DE LA DIFFERENCE
    swork(1)=0
    do 80 j1 = 1, nbmod
        do 90 i1 = 1, nl
            zr(lnres+j1-1)=zr(lnres+j1-1)+( zr(lmast+(j1-1)*nl+(i1-1))&
            - zr(lcpet+(j1-1)*nl+(i1-1)) )**2
90      continue
        zr(lnres+j1-1)=sqrt(zr(lnres+j1-1))/nl
        swork(1)=max(swork(1),zr(lnres+j1-1))
80  end do
!
    call matfpe(1)
!
    if (swork(1) .gt. 1.d-3) then
!
        if (nbvect .gt. nddlin) then
!-- ON N'ARRIVE PAS A AVOIR UNE EXPANSION CORRECTE
            call assert(.false.)
        endif
        nbvect=nbvect+nbmod
!
        call jedetr('&&OP0091.MAT_SM1XUT')
        call jedetr('&&OP0091.MAT_VXSM1XUT')
        call jedetr('&&OP0091.MAT_S')
        call jedetr('&&OP0091.MAT_U')
        call jedetr('&&OP0091.MAT_V')
        call jedetr('&&MODINT.INTERFACES_SST')
        call jedetr('&&MODEXP.CPHI')
        call jedetr('&&MODEXP.COMB_LIN')
        call jedetr('&&MODEXP.VEC_VAL_SING')
        call jedetr('&&MODEXP.MAT_AXB')
        call jedetr('&&MODEXP.PHI_EXP')
        call jedetr('&&MODEXP.MATRICE_MODES')
        call jedetr('&&MODEXP.C_PHI_EXP')
        call jedetr('&&MODEXP.NORM_RESIDU')
        goto 500
    endif
!
!-------------------------C
!--                     --C
!-- RELEVEMENT STATIQUE --C
!--                     --C
!-------------------------C
!
    call wkvect(modet, 'V V R', nbeq1*nbmod, lmodet)
!
    if (nc .ne. nddlin) then
        call assert(.false.)
    endif
!
!-- EXPANSION STATIQUE
!
    do 100 j1 = 1, nbmod
        do 110 i1 = 1, nddlin
            zr(lmodet + (j1-1)*nbeq1 + zi(linlag+(i1-1)*2) -1 ) =&
            zr(lphiex + (j1-1)*nc + (i1-1))
            zr(lmodet + (j1-1)*nbeq1 + zi(linlag+(i1-1)*2+1) -1 ) =&
            zr(lphiex + (j1-1)*nc + (i1-1))
110      continue
100  end do
!
    call dismoi('F', 'SOLVEUR', raide, 'MATR_ASSE', ibid,&
                solveu, ibid)
    call preres(solveu, 'V', info, '&&OP0091.MATPRE', raide,&
                ibid, 1)
    call resoud(raide, '&&MOIN93.MATPRE', solveu, ' ', nbmod,&
                ' ', ' ', ' ', zr(lmodet), cbid,&
                ' ', .true., 0, iret)
!
!------------C
!-- MENAGE --C
!------------C
    call jedetr('&&MODEXP.CPHI')
    call jedetr('&&MODEXP.COMB_LIN')
    call jedetr('&&MODEXP.PHI_EXP')
    call jedetr('&&MODEXP.VEC_VAL_SING')
    call jedetr('&&MODEXP.MAT_AXB')
    call jedetr('&&MODEXP.CONNEC_INTERF')
    call jedetr('&&MOIN93.NOEUDS_DDL_INT')
    call jedetr('&&MOIN93.V_IND_LAG')
    call jedetr('&&MOIN93.DDL_ACTIF_INT')
    call jedetr('&&MOIN93.V_IND_DDL_INT')
    call jedetr('&&MOIN93.IS_DDL_INTERF')
    call jedetr('&&MOIN93.IND_NOEUD')
    call jedetr('&&MOIN93.IPOS_DDL_INTERF')
    call jedetr('&&MODL91      .MODG.SSNO')
    call jedetr('&&MODL91      .MODG.SSME')
    call jedetr('&&MODINT.INTERFACES_SST')
    call jedetr('&&MODEXP.C_PHI_EXP')
    call jedetr('&&MODEXP.NORM_RESIDU')
!
    call detrsd('MATR_ASSE', '&&RAID91')
    call detrsd('MATR_ASSE', '&&MASS91')
    call jedetc('V', '&&NUME91', 1)
    call jedetr('&&MODEXP.MATRICE_MODES')
    call jedetr('&&MODEXP.VECTEUR_FREQ')
!
end subroutine
