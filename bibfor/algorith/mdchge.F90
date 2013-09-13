subroutine mdchge(numddl, typnum, imode, iamor, pulsat,&
                  masgen, amogen, lflu, nbnli, noecho,&
                  logcho, parcho, intitu, ddlcho, ier)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/mdchan.h"
#include "asterfort/mdchdl.h"
#include "asterfort/mdchre.h"
#include "asterfort/mgutdm.h"
#include "asterfort/orient.h"
#include "asterfort/tbliva.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/vechbn.h"
!
    integer :: nbnli, iamor, imode, ier, logcho(nbnli, *), ddlcho(*)
    real(kind=8) :: parcho(nbnli, *), pulsat(*), masgen(*), amogen(*)
    logical :: lflu
    character(len=8) :: noecho(nbnli, *), intitu(*)
    character(len=14) :: numddl
    character(len=16) :: typnum
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     ROUTINE APPELEE PAR MDCHOC
!     TRAITEMENT DU CAS OU NUME_DDL = 'NUME_DDL_GENE'
!
! IN  : NUMDDL : NOM DE LA NUMEROTATION
! IN  : TYPNUM : TYPE DE LA NUMEROTATION
! IN  : IMODE  : NUMERO DU MODE DE MASSE LA PLUS ELEVEE
! IN  : IAMOR  : NUMERO DE L'AMORTISSEMENT ASSOCIE
! IN  : PULSAT : PULSATIONS DES MODES
! IN  : MASGEN : MASSES GENERALISEES DES MODES
! IN  : AMOGEN : MATRICE DES AMORTISSEMENTS GENERALISES
! IN  : LFLU   : LOGIQUE INDIQUANT LA PRESENCE DE LAME FLUIDE
! IN  : NBNLI  : DIMENSION DES TABLEAUX (NBCHOC+NBSISM+NBFLAM)
! OUT : NOECHO : NOEUD DE CHOC (VOIR MDCHOC)
! OUT : LOGCHO : LOGIQUE CHOC (VOIR MDCHOC)
! OUT : PARCHO : PARAMETRE DE CHOC (VOIR MDCHOC)
! OUT : INTITU : INTITULE DE CHOC
! OUT : DDLCHO : TABLEAU DES NUMEROTATIONS DES NOEUDS DE CHOC
! OUT : IER    : NIVEAU D'ERREUR
!     ------------------------------------------------------------------
!
    integer :: nbchoc, i, j, ibid, jcoor1, jcoor2, irett, nn1, nn2, ino1, ino2
    integer :: n1, n2, iret, llrefe
    real(kind=8) :: ktang, ctang, k, coor1(3), coor2(3), xjeu, r8bid
    complex(kind=8) :: cbid
    logical :: lnoue2
    character(len=8) :: kbid, nomno1, nomno2, sst1, sst2, maya1, maya2, repere
    character(len=8) :: k8typ
    character(len=10) :: motfac
    character(len=14) :: nume1, nume2
    character(len=24) :: mdgene, mdssno, refo, nomgr1, nomgr2
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!
    call getfac('CHOC', nbchoc)
!
    call jeveuo(numddl//'.NUME.REFN', 'L', llrefe)
    mdgene = zk24(llrefe)
    mdssno = mdgene(1:14)//'.MODG.SSNO'
!
    motfac = 'CHOC'
    do 100 i = 1, nbchoc
!
        lnoue2 = .false.
!
        call getvtx(motfac, 'SOUS_STRUC_1', iocc=i, scal=sst1, nbret=n1)
        if (n1 .eq. 0) then
            call utmess('F', 'ALGORITH5_31')
        endif
        call jenonu(jexnom(mdssno, sst1), iret)
        if (iret .eq. 0) then
            call utmess('F', 'ALGORITH5_32')
        endif
        call mgutdm(mdgene, sst1, ibid, 'NOM_NUME_DDL', ibid,&
                    nume1)
        call mgutdm(mdgene, sst1, ibid, 'NOM_MAILLAGE', ibid,&
                    maya1)
!
        call getvtx(motfac, 'NOEUD_1', iocc=i, scal=nomno1, nbret=ibid)
        if (ibid .ne. 0) then
            noecho(i,1) = nomno1
        else
            call getvtx(motfac, 'GROUP_NO_1', iocc=i, scal=nomgr1, nbret=ibid)
            call utnono(' ', maya1, 'NOEUD', nomgr1, nomno1,&
                        iret)
            if (iret .eq. 10) then
                call utmess('F', 'ELEMENTS_67', sk=nomgr1)
            else if (iret.eq.1) then
                valk(1) = nomgr1
                valk(2) = nomno1
                call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
            endif
            noecho(i,1) = nomno1
        endif
        noecho(i,2) = sst1
        noecho(i,3) = nume1(1:8)
        noecho(i,4) = maya1
!
        call getvtx(motfac, 'NOEUD_2', iocc=i, scal=nomno2, nbret=nn1)
        call getvtx(motfac, 'GROUP_NO_2', iocc=i, scal=nomgr2, nbret=nn2)
        if (nn1 .ne. 0 .or. nn2 .ne. 0) then
            lnoue2 = .true.
            call getvtx(motfac, 'SOUS_STRUC_2', iocc=i, scal=sst2, nbret=n2)
            if (n2 .eq. 0) then
                call utmess('F', 'ALGORITH5_33')
            endif
            call jenonu(jexnom(mdssno, sst2), iret)
            if (iret .eq. 0) then
                call utmess('F', 'ALGORITH5_34')
            endif
            call mgutdm(mdgene, sst2, ibid, 'NOM_NUME_DDL', ibid,&
                        nume2)
            call mgutdm(mdgene, sst2, ibid, 'NOM_MAILLAGE', ibid,&
                        maya2)
            if (nn1 .ne. 0) then
                call getvtx(motfac, 'NOEUD_2', iocc=i, scal=nomno2, nbret=nn1)
                noecho(i,5) = nomno2
            else
                call utnono(' ', maya2, 'NOEUD', nomgr2, nomno2,&
                            iret)
                if (iret .eq. 10) then
                    call utmess('F', 'ELEMENTS_67', sk=nomgr2)
                else if (iret.eq.1) then
                    valk(1) = nomgr2
                    valk(2) = nomno2
                    call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
                endif
                noecho(i,5) = nomno2
            endif
            call vechbn(mdgene, nomno1, sst1, nomno2, sst2)
            noecho(i,6) = sst2
            noecho(i,7) = nume2(1:8)
            noecho(i,8) = maya2
        else
            noecho(i,5) = nomno1
            noecho(i,6) = sst1
            noecho(i,7) = nume1(1:8)
            noecho(i,8) = maya1
        endif
!
        call mdchdl(nbnli, noecho, lnoue2, i, ddlcho,&
                    ier)
!
        call jenonu(jexnom(noecho(i, 4)//'.NOMNOE', noecho(i, 1)), ino1)
        call jenonu(jexnom(noecho(i, 8)//'.NOMNOE', noecho(i, 5)), ino2)
        call jeveuo(noecho(i, 4)//'.COORDO    .VALE', 'L', jcoor1)
        call jeveuo(noecho(i, 8)//'.COORDO    .VALE', 'L', jcoor2)
        call orient(mdgene, noecho(i, 2), jcoor1, ino1, coor1,&
                    1)
        call orient(mdgene, noecho(i, 6), jcoor2, ino2, coor2,&
                    1)
        do 110 j = 1, 3
            parcho(i,7+j) = coor1(j)
            parcho(i,10+j) = coor2(j)
110      continue
!
        ktang = 0.d0
        ctang = 0.d0
        call getvtx(motfac, 'INTITULE', iocc=i, scal=intitu(i), nbret=n1)
        call getvr8(motfac, 'JEU', iocc=i, scal=parcho(i, 1), nbret=n1)
        call getvr8(motfac, 'DIST_1', iocc=i, scal=parcho(i, 30), nbret=n1)
        call getvr8(motfac, 'DIST_2', iocc=i, scal=parcho(i, 31), nbret=n1)
        call getvr8(motfac, 'RIGI_NOR', iocc=i, scal=parcho(i, 2), nbret=n1)
        call getvr8(motfac, 'AMOR_NOR', iocc=i, scal=parcho(i, 3), nbret=n1)
        call getvr8(motfac, 'RIGI_TAN', iocc=i, scal=ktang, nbret=n1)
        call getvr8(motfac, 'COULOMB', iocc=i, scal=parcho(i, 6), nbret=n1)
        parcho(i,7) = parcho(i,6)
        call getvr8(motfac, 'COULOMB_DYNA', iocc=i, scal=parcho(i, 6), nbret=n1)
        call getvr8(motfac, 'COULOMB_STAT', iocc=i, scal=parcho(i, 7), nbret=n1)
        call getvr8(motfac, 'AMOR_TAN', iocc=i, scal=ctang, nbret=n1)
        call getvtx(motfac, 'LAME_FLUIDE', iocc=i, scal=kbid, nbret=n1)
        if (kbid(1:3) .eq. 'OUI') then
            lflu = .true.
            logcho(i,2) = 1
            call getvr8(motfac, 'ALPHA   ', iocc=i, scal=parcho(i, 32), nbret=n1)
            call getvr8(motfac, 'BETA    ', iocc=i, scal=parcho(i, 33), nbret=n1)
            call getvr8(motfac, 'CHI     ', iocc=i, scal=parcho(i, 34), nbret=n1)
            call getvr8(motfac, 'DELTA   ', iocc=i, scal=parcho(i, 35), nbret=n1)
        endif
!
        call getvid(motfac, 'OBSTACLE', iocc=i, scal=noecho(i, 9), nbret=n1)
!
        call tbliva(noecho(i, 9), 1, 'LIEU', ibid, r8bid,&
                    cbid, 'DEFIOBST', kbid, r8bid, 'TYPE',&
                    k8typ, ibid, r8bid, cbid, refo,&
                    irett)
        ASSERT(irett.eq.0)
        if (refo(1:9) .eq. 'BI_PLAN_Y') then
            noecho(i,9) = 'BI_PLANY'
        else if (refo(1:9).eq.'BI_PLAN_Z') then
            noecho(i,9) = 'BI_PLANZ'
        else if (refo(1:11).eq.'BI_CERC_INT') then
            noecho(i,9) = 'BI_CERCI'
        else if (refo(1:7).ne.'DISCRET') then
            noecho(i,9) = refo(1:8)
        endif
        if (noecho(i,9) .eq. 'BI_CERCI' .and. parcho(i,31) .lt. parcho(i, 30)) then
            call utmess('F', 'ALGORITH5_35')
        endif
! ------ SI CTANG NON PRECISE ON CALCULE UN AMORTISSEMENT CRITIQUE
        if (ctang .eq. 0.d0 .and. ktang .ne. 0.d0) then
            k = sqrt( pulsat(imode) ) * masgen(imode)
            ctang = 2.d0*sqrt(&
                    masgen(imode)*(k+ktang) ) - 2.d0* amogen(iamor)*sqrt( k*masgen(imode))
        endif
        parcho(i,4) = ktang
        parcho(i,5) = ctang
!
        if (noecho(i,9)(1:2) .eq. 'BI') then
            xjeu = (&
                   parcho(i,11)-parcho(i,8))**2 + (parcho(i,12)- parcho(i,9))**2 + (parcho(i,13)-&
                   &parcho(i,10)&
                   )**2
        endif
!
        call mdchre(motfac, i, i, mdgene, typnum,&
                    repere, nbnli, parcho, lnoue2)
!
        call mdchan(motfac, i, i, mdgene, typnum,&
                    repere, xjeu, nbnli, noecho, parcho)
!
100  end do
!
end subroutine
