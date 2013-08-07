subroutine rehaec(nomres, resgen, nomsst)
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
!***********************************************************************
!    T. KERBER     DATE 14/05/93
!-----------------------------------------------------------------------
!  BUT:      < RESTITUTION HARMONIQUE ECLATEE >
    implicit none
!
!      RESTITUER EN BASE PHYSIQUE SUR UNE SOUS-STRUCTURE LES RESULTATS
!                ISSU DE LA SOUS-STRUCTURATION GENERALE
!  LE CONCEPT RESULTAT EST UN RESULTAT COMPOSE "DYNA_HARMO"
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM K8 DU CONCEPT DYNA HARMO RESULTAT
! RESGEN   /I/: NOM K8 DU HARM_GENE AMONT
! NOMSST   /I/: NOM K8 DE LA SOUS-STRUCTURE SUR LAQUELLE ON RESTITUE
!
!
!
#include "jeveux.h"
!
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/dcapno.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rstran.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/vtcrea.h"
#include "asterfort/wkvect.h"
!
!
    real(kind=8) :: epsi
    character(len=4) :: champ(8)
    character(len=8) :: chmp(3), crit, interp, k8b, nomres, basmod, mailla
    character(len=8) :: lint, nomsst, modgen, resgen, soutr, k8rep
    character(len=19) :: numddl, numgen, knume, kfreq, harmge
    character(len=24) :: crefe(2), chamba, chamno, seliai, sizlia, sst
    character(len=24) :: valk(2)
    integer :: itresu(3), elim, iret
    character(len=8) :: k8bid, kbid
    integer :: iarg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i1, iad, iarchi, ibid, ich
    integer :: idresu, ieq, ier, ire1, ire2, ire3
    integer :: iretou, j, jfreq, jnume, k, k1, ldnew
    integer :: lfreq, llchab, llnequ, llnueq, llors, llprs, llref1
    integer :: llref2, llrefe, lmapro, lmoet, lrefe, lsilia, lsst
    integer :: n1, nbcham, nbddg, nbfreq, nbsst, neq
    integer :: neqet, neqgen, neqred, nusst, nutars
!-----------------------------------------------------------------------
    data soutr  /'&SOUSSTR'/
!-----------------------------------------------------------------------
!
! --- ECRITURE DU TITRE
!
    call jemarq()
    call titre()
!
! --- DETERMINATION DES CHAMPS A RESTITUER, PARMI DEPL, VITE ET ACCE
!
    harmge = resgen
! --- CALCUL DU NOMBRE DE CHAMPS A RESTITUER ET LEURS ADDRESSES
!
    call jeexin(resgen//'           .DEPL', ire1)
    call jeexin(resgen//'           .VITE', ire2)
    call jeexin(resgen//'           .ACCE', ire3)
!
    if (ire1 .eq. 0 .and. ire2 .eq. 0 .and. ire3 .eq. 0) then
        valk (1) = resgen
        call u2mesg('F', 'ALGORITH14_35', 1, valk, 0,&
                    0, 0, 0.d0)
    endif
!
    call getvtx(' ', 'TOUT_CHAM', 0, iarg, 0,&
                k8rep, n1)
    if (n1 .ne. 0) then
        call getvtx(' ', 'TOUT_CHAM', 0, iarg, 1,&
                    k8rep, n1)
    else
        k8rep=' '
    endif
    if (k8rep(1:3) .eq. 'OUI') then
        if (ire1 .eq. 0) then
            call u2mess('F', 'ALGORITH10_44')
        endif
        if (ire2 .eq. 0) then
            call u2mess('F', 'ALGORITH10_45')
        endif
        if (ire3 .eq. 0) then
            call u2mess('F', 'ALGORITH10_46')
        endif
        nbcham = 3
        chmp(1) = 'DEPL'
        chmp(2) = 'VITE'
        chmp(3) = 'ACCE'
        call jeveuo(harmge//'.DEPL', 'L', itresu(1))
        call jeveuo(harmge//'.VITE', 'L', itresu(2))
        call jeveuo(harmge//'.ACCE', 'L', itresu(3))
    else
! ----  ON RECHERCHE LES CHAMPS QU'IL FAUT RESTITUER
        call getvtx(' ', 'NOM_CHAM', 1, iarg, 0,&
                    champ, n1)
        nbcham = -n1
        call getvtx(' ', 'NOM_CHAM', 1, iarg, nbcham,&
                    champ, n1)
! ----   BOUCLE SUR LES CHAMPS DEMANDES
        do 69 i = 1, nbcham
!
            if (champ(i) .eq. 'DEPL') then
                chmp(i) = 'DEPL'
                call jeexin(harmge//'.DEPL', iret)
                if (iret .eq. 0) then
                    call u2mess('F', 'ALGORITH10_11')
                else
                    call jeveuo(harmge//'.DEPL', 'L', itresu(i))
                endif
            else if (champ(i).eq.'VITE') then
                chmp(i) = 'VITE'
                call jeexin(harmge//'.VITE', iret)
                if (iret .eq. 0) then
                    call u2mess('F', 'ALGORITH10_12')
                else
                    call jeveuo(harmge//'.VITE', 'L', itresu(i))
                endif
            else if (champ(i).eq.'ACCE') then
                chmp(i) = 'ACCE'
                call jeexin(harmge//'.ACCE', iret)
                if (iret .eq. 0) then
                    call u2mess('F', 'ALGORITH10_13')
                else
                    call jeveuo(harmge//'.ACCE', 'L', itresu(i))
                endif
            else
! ----        SI LE CHAMP N'EST PAS DEPL,VITE OU ACCE ON PLANTE
                call u2mess('F', 'ALGORITH10_16')
            endif
69      continue
    endif
!
!     NOMBRE DE CHAMPS SYMBOLIQUES CALCULES.
!     ON S'ASSURE QUE LEUR NOMBRE EST NON NUL.
!
    if (nbcham .eq. 0) then
        valk (1) = resgen
        call u2mesg('F', 'ALGORITH14_35', 1, valk, 0,&
                    0, 0, 0.d0)
    endif
!
! --- RECUPERATION DE LA NUMEROTATION ET DU MODELE GENERALISE
!
    call jeveuo(harmge//'.REFD', 'L', llref1)
    numgen(1:14) = zk24(llref1+3)(1:14)
    call jelibe(harmge//'.REFD')
    numgen(15:19) = '.NUME'
    call jeveuo(numgen//'.REFN', 'L', llref2)
    modgen = zk24(llref2)(1:8)
    call jelibe(numgen//'.REFN')
    call jeveuo(numgen//'.NEQU', 'L', llnequ)
    neqgen = zi(llnequ)
    call jelibe(numgen//'.NEQU')
!
! --- RECUPERATION NUMERO DE LA SOUS-STRUCTURE
!
    call jenonu(jexnom(modgen//'      .MODG.SSNO', nomsst), nusst)
    if (nusst .eq. 0) then
        valk (1) = modgen
        valk (2) = nomsst
        call u2mesg('F', 'ALGORITH14_25', 2, valk, 0,&
                    0, 0, 0.d0)
    endif
!
!-- ON TESTE SI ON A EU RECOURS A L'ELIMINATION
!
    seliai=numgen(1:14)//'.ELIM.BASE'
    sizlia=numgen(1:14)//'.ELIM.TAIL'
    sst=   numgen(1:14)//'.ELIM.NOMS'
!
    call jeexin(seliai, elim)
!
    if (elim .eq. 0) then
!
        call jenonu(jexnom(numgen//'.LILI', soutr), ibid)
        call jeveuo(jexnum(numgen//'.ORIG', ibid), 'L', llors)
        call jenonu(jexnom(numgen//'.LILI', soutr), ibid)
        call jelira(jexnum(numgen//'.ORIG', ibid), 'LONMAX', nbsst)
!
        nutars=0
        do 20 i = 1, nbsst
            if (zi(llors+i-1) .eq. nusst) nutars=i
20      continue
!
!
! --- NOMBRE DE MODES ET NUMERO DU PREMIER DDL DE LA SOUS-STRUCTURE
        call jenonu(jexnom(numgen//'.LILI', soutr), ibid)
        call jeveuo(jexnum(numgen//'.PRNO', ibid), 'L', llprs)
        nbddg=zi(llprs+(nutars-1)*2+1)
        ieq=zi(llprs+(nutars-1)*2)
!
    else
!
        neqet=0
        ieq=0
        call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst)
        call jeveuo(numgen//'.NEQU', 'L', ibid)
        neqred=zi(ibid)
        call jeveuo(seliai, 'L', lmapro)
        call jeveuo(sizlia, 'L', lsilia)
        call jeveuo(sst, 'L', lsst)
        ibid=1
        do 11 i = 1, nbsst
            neqet=neqet+zi(lsilia+i-1)
11      continue
!
        ieq=0
        do 41 i1 = 1, nusst-1
            ieq=ieq+zi(lsilia+i1-1)
41      continue
!
        call wkvect('&&MODE_ETENDU_REST_ELIM', 'V V C', neqet, lmoet)
    endif
!
! --- RECUPERATION D'INFORMATIONS SUR LA SOUS-STRUCTURE
!
    call mgutdm(modgen, nomsst, ibid, 'NOM_BASE_MODALE', ibid,&
                basmod)
    if (elim .ne. 0) then
        call dismoi('F', 'NB_MODES_TOT', basmod, 'RESULTAT', nbddg,&
                    kbid, ier)
    endif
!
    call jeveuo(basmod//'           .REFD', 'L', llrefe)
! -->AAC-->NORMALEMENT CE .REFD EST INCOHERENT AVEC CELUI DE DYNA_GENE
    lint = zk24(llrefe+4)(1:8)
    call dismoi('F', 'NOM_MAILLA', lint, 'INTERF_DYNA', ibid,&
                mailla, iret)
    call dismoi('F', 'NOM_NUME_DDL', lint, 'INTERF_DYNA', ibid,&
                numddl, iret)
    call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neq,&
                k8bid, iret)
!
    crefe(1) = mailla
    crefe(2) = numddl
!
! --- RECUPERATION DES FREQUENCES
!
    call getvtx(' ', 'CRITERE', 0, iarg, 1,&
                crit, n1)
    call getvr8(' ', 'PRECISION', 0, iarg, 1,&
                epsi, n1)
    call getvtx(' ', 'INTERPOL', 0, iarg, 1,&
                interp, n1)
!
    knume = '&&RETREC.NUM_RANG'
    kfreq = '&&RETREC.FREQ'
    call rstran(interp, harmge, ' ', 1, kfreq,&
                knume, nbfreq, iretou)
    if (iretou .ne. 0) then
        call u2mess('F', 'ALGORITH10_47')
    endif
    call jeexin(kfreq, iret)
    if (iret .gt. 0) then
        call jeveuo(kfreq, 'E', jfreq)
        call jeveuo(knume, 'E', jnume)
    endif
!
! --- ALLOCATION DE LA STRUCTURE DE DONNEES RESULTAT-COMPOSE
!
    call rscrsd('G', nomres, 'DYNA_HARMO', nbfreq)
!
! -------------------------------------
! --- RESTITUTION SUR BASE PHYSIQUE ---
! -------------------------------------
!
    call jeveuo(numgen//'.NUEQ', 'L', llnueq)
!
    iarchi = 0
    if (interp(1:3) .ne. 'NON') then
        call u2mess('F', 'ALGORITH3_86')
!
    else
        call jeexin(harmge//'.ORDR', iret)
!
        do 40 i = 0, nbfreq-1
            iarchi = iarchi + 1
!
            do 42 ich = 1, nbcham
                idresu = itresu(ich)
!
!-- SI ELIMINATION, ON RESTITUE D'ABORD LES MODES GENERALISES
                if (elim .ne. 0) then
                    do 22 i1 = 1, neqet
                        zc(lmoet+i1-1)=dcmplx(0.d0,0.d0)
                        do 33 k1 = 1, neqred
                            zc(lmoet+i1-1)=zc(lmoet+i1-1)+ zr(lmapro+(&
                            k1-1)*neqet+i1-1)* zc(idresu+k1-1+(zi(&
                            jnume+i)-1)*neqred)
33                      continue
22                  continue
                endif
!
                call rsexch(' ', nomres, chmp(ich), iarchi, chamno,&
                            iret)
                if (iret .eq. 0) then
                    call u2mesk('A', 'ALGORITH2_64', 1, chamno)
                else if (iret.eq.100) then
                    call vtcrea(chamno, crefe, 'G', 'C', neq)
                else
                    ASSERT(.false.)
                endif
                chamno(20:24) = '.VALE'
                call jeveuo(chamno, 'E', ldnew)
!
! --- BOUCLE SUR LES MODES PROPRES DE LA BASE
!
                do 70 j = 1, nbddg
                    call dcapno(basmod, 'DEPL', j, chamba)
                    call jeveuo(chamba, 'L', llchab)
!
                    if (elim .ne. 0) then
                        iad=lmoet+ieq+j-1
                    else
                        iad=idresu+(zi(jnume+i)-1)*neqgen+ zi(llnueq+&
                        ieq+j-2)-1
                    endif
!
! --- BOUCLE SUR LES EQUATIONS PHYSIQUES
!
                    do 80 k = 1, neq
                        zc(ldnew+k-1)=zc(ldnew+k-1)+zr(llchab+k-1)*zc(&
                        iad)
80                  continue
                    call jelibe(chamba)
70              continue
                call jelibe(chamno)
                call rsnoch(nomres, chmp(ich), iarchi)
42          continue
            call rsadpa(nomres, 'E', 1, 'FREQ', iarchi,&
                        0, lfreq, k8b)
            zr(lfreq) = zr(jfreq+i)
40      continue
!
    endif
!
    call wkvect(nomres//'           .REFD', 'G V K24', 7, lrefe)
! --> AAC-->NORMALEMENT CE .REFD EST INCOHERENT AVEC CELUI DE DYNA_GENE
    zk24(lrefe ) = zk24(llrefe)
    zk24(lrefe+1) = zk24(llrefe+1)
    zk24(lrefe+2) = zk24(llrefe+2)
    zk24(lrefe+3) = zk24(llrefe+3)
    zk24(lrefe+4) = zk24(llrefe+4)
    zk24(lrefe+5) = zk24(llrefe+5)
    zk24(lrefe+6) = zk24(llrefe+6)
    call jelibe(nomres//'           .REFD')
    call jelibe(basmod//'           .REFD')
!
    call jelibe(numgen//'.NUEQ')
    call jedetr('&&RETREC.NUM_RANG')
    call jedetr('&&RETREC.FREQ')
!
    goto 9999
!
9999  continue
    call jedema()
end subroutine
