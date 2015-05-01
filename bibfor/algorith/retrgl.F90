subroutine retrgl(nomres, resgen, mailsk, profno)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
    implicit none
!  C. VARE     DATE 16/11/94
!-----------------------------------------------------------------------
!
!  BUT : < RESTITUTION TRANSITOIRE GLOBALE >
!
!        RESTITUER EN BASE PHYSIQUE SUR UN MAILLAGE SQUELETTE LES
!        RESULTATS ISSUS D'UN CALCUL TRANSITOIRE PAR SOUS-STRUCTURATION
!        CLASSIQUE
!
!        LE CONCEPT RESULTAT EST UN RESULTAT COMPOSE "DYNA_TRANS"
!
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM K8 DU CONCEPT DYNA_TRANS RESULTAT
! RESGEN /I/ : NOM K8 DU RESULTAT GENERALISE AMONT (TRAN_GENE)
! MAILSK /I/ : NOM K8 DU MAILLAGE SQUELETTE SUPPORT
! PROFNO /I/ : NOM K19 DU PROF_CHNO DU SQUELETTE
!
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dcapno.h"
#include "asterfort/dismoi.h"
#include "asterfort/extrac.h"
#include "asterfort/genugl.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/nueq_chck.h"
#include "asterfort/refdcp.h"
#include "asterfort/rotchm.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rstran.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrea.h"
#include "asterfort/wkvect.h"
!
!
!
    real(kind=8) :: epsi
    character(len=6) :: pgc
    character(len=8) :: chmp(3), k8rep, crit, interp, k8bid, nomres, basmod
    character(len=8) :: mailsk, model_gene, resgen
    character(len=14) :: nume_gene
    character(len=19) :: numddl, prof_gene, knume, kinst, trange, profno
    character(len=24) :: crefe(2), chamba, indirf, chamno, seliai, sizlia, sst
    character(len=24) :: valk, nomsst
    integer :: itresu(3), elim, neqet, neqred, lmapro, lsilia, lsst, lmoet
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i1, iad, iar, iarchi, ibid, ich
    integer :: idep, idresu, idvecg, ieq, ier, ire1, i_ligr_ss
    integer :: ire2, ire3, iret, iretou, j, jinst, jnume
    integer :: k, k1, l, ldnew, linst, llchab, llind
    integer :: llinsk, llors, llprs
    integer :: llrot, ltrotx, ltroty, ltrotz, ltvec, n1
    integer :: nbbas, nbcham, nbcmp, nbcou, nbinsg, nbinst, nbnot
    integer :: nbsst, neq, neqgen, neqs, numsst, nutars
    character(len=24), pointer :: refn(:) => null()
    integer, pointer :: nueq(:) => null()
    real(kind=8), pointer :: disc(:) => null()
!-----------------------------------------------------------------------
    data pgc   /'RETRGL'/
!-----------------------------------------------------------------------
!
    call jemarq()
    indirf = '&&'//pgc//'.INDIR.SST'
!
! --- ECRITURE DU TITRE
    call titre()
!
! --- VERIFICATION SQUELETTE
    call jeexin(mailsk//'.INV.SKELETON', iret)
    if (iret .eq. 0) then
        valk = mailsk
        call utmess('F', 'ALGORITH14_27', sk=valk)
    endif
    call jeveuo(mailsk//'.INV.SKELETON', 'L', llinsk)
!
! --- DETERMINATION DES CHAMPS A RESTITUER, PARMI DEPL, VITE ET ACCE
    trange = resgen
    call jeexin(resgen//'           .DEPL', ire1)
    call jeexin(resgen//'           .VITE', ire2)
    call jeexin(resgen//'           .ACCE', ire3)
    if (ire1 .eq. 0 .and. ire2 .eq. 0 .and. ire3 .eq. 0.d0) then
        valk = resgen
        call utmess('F', 'ALGORITH14_35', sk=valk)
    endif
!
    call getvtx(' ', 'TOUT_CHAM', scal=k8rep, nbret=n1)
    if (k8rep(1:3) .eq. 'OUI') then
        if (ire1 .eq. 0) then
            call utmess('F', 'ALGORITH10_44')
        endif
        if (ire2 .eq. 0) then
            call utmess('F', 'ALGORITH10_45')
        endif
        if (ire3 .eq. 0) then
            call utmess('F', 'ALGORITH10_46')
        endif
        nbcham = 3
        chmp(1) = 'DEPL'
        chmp(2) = 'VITE'
        chmp(3) = 'ACCE'
        call jeveuo(trange//'.DEPL', 'L', itresu(1))
        call jeveuo(trange//'.VITE', 'L', itresu(2))
        call jeveuo(trange//'.ACCE', 'L', itresu(3))
    else
        call getvtx(' ', 'NOM_CHAM', scal=k8rep, nbret=n1)
        if (k8rep(1:4) .eq. 'DEPL' .and. ire1 .eq. 0) then
            call utmess('F', 'ALGORITH10_44')
        else if (k8rep(1:4).eq.'DEPL'.and.ire1.ne.0) then
            nbcham = 1
            chmp(1) = 'DEPL'
            call jeveuo(trange//'.DEPL', 'L', itresu(1))
        else if (k8rep(1:4).eq.'VITE'.and.ire2.eq.0) then
            call utmess('F', 'ALGORITH10_45')
        else if (k8rep(1:4).eq.'VITE'.and.ire2.ne.0) then
            nbcham = 1
            chmp(1) = 'VITE'
            call jeveuo(trange//'.VITE', 'L', itresu(1))
        else if (k8rep(1:4).eq.'ACCE'.and.ire3.eq.0) then
            call utmess('F', 'ALGORITH10_46')
        else if (k8rep(1:4).eq.'ACCE'.and.ire3.ne.0) then
            nbcham = 1
            chmp(1) = 'ACCE'
            call jeveuo(trange//'.ACCE', 'L', itresu(1))
        endif
    endif
!
! --- RECUPERATION DE LA NUMEROTATION ET DU MODELE GENERALISE
    call dismoi('NUME_DDL', trange, 'RESU_DYNA', repk=nume_gene)
    prof_gene = nume_gene//'.NUME'
    call jeveuo(prof_gene//'.REFN', 'L', vk24=refn)
    model_gene = refn(1)(1:8)
    call nueq_chck(prof_gene, neqgen)
!
    call jelira(model_gene//'      .MODG.SSNO', 'NOMMAX', nbsst)
    k8bid = '  '
    call mgutdm(model_gene, k8bid, 1, 'NB_CMP_MAX', nbcmp,&
                k8bid)
!
! --- RECUPERATION DES ROTATIONS
    call wkvect('&&'//pgc//'ROTX', 'V V R', nbsst, ltrotx)
    call wkvect('&&'//pgc//'ROTY', 'V V R', nbsst, ltroty)
    call wkvect('&&'//pgc//'ROTZ', 'V V R', nbsst, ltrotz)
    do i = 1, nbsst
        call jeveuo(jexnum(model_gene//'      .MODG.SSOR', i), 'L', llrot)
        zr(ltrotz+i-1) = zr(llrot)
        zr(ltroty+i-1) = zr(llrot+1)
        zr(ltrotx+i-1) = zr(llrot+2)
    end do
!
! --- CREATION DU PROF-CHAMNO
    call genugl(profno, indirf, model_gene, mailsk)
    call nueq_chck(profno, neq)
!
! --- RECUPERATION DU NOMBRE DE NOEUDS
    call dismoi('NB_NO_MAILLA', mailsk, 'MAILLAGE', repi=nbnot)
!
! --- INFORMATIONS POUR CREATION DES CHAMNO A PARTIR DES .REFE
    crefe(1) = mailsk
    crefe(2) = profno
!
! --- RECUPERATION DES INSTANTS
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=n1)
    call getvtx(' ', 'INTERPOL', scal=interp, nbret=n1)
!
    knume = '&&RETREC.NUM_RANG'
    kinst = '&&RETREC.INSTANT'
    call rstran(interp, trange, ' ', 1, kinst,&
                knume, nbinst, iretou)
    if (iretou .ne. 0) then
        call utmess('F', 'ALGORITH10_47')
    endif
    call jeexin(kinst, iret)
    if (iret .gt. 0) then
        call jeveuo(kinst, 'E', jinst)
        call jeveuo(knume, 'E', jnume)
    endif
!
! --- ALLOCATION DE LA STRUCTURE DE DONNEES RESULTAT-COMPOSE
!
    call rscrsd('G', nomres, 'DYNA_TRANS', nbinst)
!
!-- ON TESTE SI ON A EU RECOURS A L'ELIMINATION
!
    seliai=nume_gene(1:14)//'.ELIM.BASE'
    sizlia=nume_gene(1:14)//'.ELIM.TAIL'
    sst=   nume_gene(1:14)//'.ELIM.NOMS'
!
    call jeexin(seliai, elim)
    if (elim .ne. 0) then
        neqet=0
        neqred=neqgen
        nomsst=model_gene//'      .MODG.SSNO'
        call jeveuo(seliai, 'L', lmapro)
        call jeveuo(sizlia, 'L', lsilia)
        call jeveuo(sst, 'L', lsst)
        do i = 1, nbsst
            neqet=neqet+zi(lsilia+i-1)
        end do
        call wkvect('&&MODE_ETENDU_REST_ELIM', 'V V R', neqet, lmoet)
    endif
!
! -------------------------------------
! --- RESTITUTION SUR BASE PHYSIQUE ---
! -------------------------------------
!
    call jeveuo(prof_gene//'.NUEQ', 'L', vi=nueq)
    call jenonu(jexnom(prof_gene//'.LILI', '&SOUSSTR'), i_ligr_ss)
    call jeveuo(jexnum(prof_gene//'.ORIG', i_ligr_ss), 'L', llors)
    call jeveuo(jexnum(prof_gene//'.PRNO', i_ligr_ss), 'L', llprs)
!
    iarchi = 0
!
    if (interp(1:3) .ne. 'NON') then
!
        call jeveuo(trange//'.DISC', 'L', vr=disc)
        call jelira(trange//'.DISC', 'LONMAX', nbinsg)
        if (elim .eq. 0) then
            call wkvect('&&RETREC.VECTGENE', 'V V R', neqgen, idvecg)
        else
            call wkvect('&&RETREC.VECTGENE', 'V V R', neqet, idvecg)
        endif
!
        do i = 0, nbinst-1
            iarchi = iarchi + 1
!
            do ich = 1, nbcham
!
                idresu = itresu(ich)
                call rsexch(' ', nomres, chmp(ich), iarchi, chamno,&
                            iret)
                if (iret .eq. 0) then
                    call utmess('A', 'ALGORITH2_64', sk=chamno)
                else if (iret.eq.100) then
                    call vtcrea(chamno, crefe, 'G', 'R', neq)
                else
                    ASSERT(.false.)
                endif
                chamno(20:24) = '.VALE'
                call jeveuo(chamno, 'E', ldnew)
                call extrac(interp, epsi, crit, nbinsg, disc,&
                            zr( jinst+i), zr(idresu), neqgen, zr(idvecg), ier)
!
!-- SI ELIMINATION, ON RESTITUE D'ABORD LES MODES GENERALISES
                if (elim .ne. 0) then
                    do i1 = 1, neqet
                        zr(lmoet+i1-1)=0.d0
                        do k1 = 1, neqred
                            zr(lmoet+i1-1)=zr(lmoet+i1-1)+ zr(lmapro+(&
                            k1-1)*neqet+i1-1)* zr(idvecg+k1-1)
                        end do
                    end do
                endif
!
! --- BOUCLE SUR LES SOUS-STRUCTURESs
!
                do k = 1, nbsst
                    call jeexin(jexnum(indirf, k), iret)
!
! --- TEST SI LA SST GENERE DES DDL GLOBAUX
!
                    if (iret .ne. 0) then
!
! --- RECUPERATION DU NUMERO TARDIF DE LA SST
!
                        if (elim .ne. 0) then
                            call jenonu(jexnom(nomsst, zk8(lsst+k-1)), numsst)
                            ieq=0
                            do i1 = 1, k-1
                                ieq=ieq+zi(lsilia+i1-1)
                            end do
                        else
                            numsst=k
!  RECUPERATION DU NUMERO TARDIF DE LA SST
                            do j = 1, nbsst
                                if (zi(llors+j-1) .eq. numsst) nutars=j
                            end do
                            ieq=zi(llprs+(nutars-1)*2)
                        endif
!
                        k8bid = '  '
                        call mgutdm(model_gene, k8bid, numsst, 'NOM_BASE_MODALE', ibid,&
                                    basmod)
                        call dismoi('NB_MODES_TOT', basmod, 'RESULTAT', repi=nbbas)
                        k8bid = '  '
                        call mgutdm(model_gene, k8bid, numsst, 'NOM_NUME_DDL', ibid,&
                                    numddl)
                        call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neqs)
                        call wkvect('&&'//pgc//'.TRAV', 'V V R', neqs, ltvec)
!
! --- BOUCLE SUR LES MODES PROPRES DE LA BASE
!
                        do j = 1, nbbas
                            call dcapno(basmod, 'DEPL', j, chamba)
                            call jeveuo(chamba, 'L', llchab)
!
                            if (elim .ne. 0) then
                                iad=lmoet+ieq+j-1
                            else
                                iad=idvecg+nueq(1+ieq+j-2)-1
                            endif
!
! --- BOUCLE SUR LES EQUATIONS PHYSIQUES
!
                            do l = 1, neqs
                                zr(ltvec+l-1)=zr(ltvec+l-1)+zr(llchab+&
                                l-1)*zr(iad)
                            end do
                            call jedetr('&&'//pgc//'.VECTA')
                        end do
!
                        call jeveuo(jexnum(indirf, numsst), 'L', llind)
                        call jelira(jexnum(indirf, numsst), 'LONMAX', nbcou)
                        nbcou = nbcou/2
                        do l = 1, nbcou
                            idep = zi(llind+(l-1)*2)
                            iar = zi(llind+(l-1)*2+1)
                            zr(ldnew+iar-1) = zr(ltvec+idep-1)
                        end do
                        call jedetr('&&'//pgc//'.TRAV')
                    endif
!
                end do
                call rsnoch(nomres, chmp(ich), iarchi)
!
! --- ROTATION DU CHAMP AUX NOEUDS
!
                call rotchm(profno, zr(ldnew), zr(ltrotz), nbsst, zi( llinsk),&
                            nbnot, nbcmp, 3)
                call rotchm(profno, zr(ldnew), zr(ltroty), nbsst, zi( llinsk),&
                            nbnot, nbcmp, 2)
                call rotchm(profno, zr(ldnew), zr(ltrotx), nbsst, zi( llinsk),&
                            nbnot, nbcmp, 1)
!
            end do
            call rsadpa(nomres, 'E', 1, 'INST', iarchi,&
                        0, sjv=linst, styp=k8bid)
            zr(linst) = zr(jinst+i)
        end do
!
    else
!
        call jeexin(trange//'.ORDR', iret)
        if (iret .ne. 0 .and. zi(jnume) .eq. 1) iarchi=-1
!
        do i = 0, nbinst-1
            iarchi = iarchi + 1
!
            do ich = 1, nbcham
                idresu = itresu(ich)
!-- SI ELIMINATION, ON RESTITUE D'ABORD LES MODES GENERALISES
                if (elim .ne. 0) then
                    do i1 = 1, neqet
                        zr(lmoet+i1-1)=0.d0
                        do k1 = 1, neqred
                            zr(lmoet+i1-1)=zr(lmoet+i1-1)+ zr(lmapro+(&
                            k1-1)*neqet+i1-1)* zr(idresu+k1-1+(zi(&
                            jnume+i)-1)*neqred)
                        end do
                    end do
                endif
!
                call rsexch(' ', nomres, chmp(ich), iarchi, chamno,&
                            iret)
                if (iret .eq. 0) then
                    call utmess('A', 'ALGORITH2_64', sk=chamno)
                else if (iret.eq.100) then
                    call vtcrea(chamno, crefe, 'G', 'R', neq)
                else
                    ASSERT(.false.)
                endif
                chamno(20:24) = '.VALE'
                call jeveuo(chamno, 'E', ldnew)
!
! --- BOUCLE SUR LES SOUS-STRUCTURES
!
                do k = 1, nbsst
                    call jeexin(jexnum(indirf, k), iret)
!
! --- TEST SI LA SST GENERE DES DDL GLOBAUX
!
                    if (iret .ne. 0) then
!
! --- RECUPERATION DU NUMERO TARDIF DE LA SST
!
                        if (elim .ne. 0) then
                            call jenonu(jexnom(nomsst, zk8(lsst+k-1)), numsst)
                            ieq=0
                            do i1 = 1, k-1
                                ieq=ieq+zi(lsilia+i1-1)
                            end do
                        else
                            numsst=k
!  RECUPERATION DU NUMERO TARDIF DE LA SST
                            do j = 1, nbsst
                                if (zi(llors+j-1) .eq. numsst) nutars=j
                            end do
                            ieq=zi(llprs+(nutars-1)*2)
                        endif
                        k8bid = '  '
                        call mgutdm(model_gene, k8bid, numsst, 'NOM_BASE_MODALE', ibid,&
                                    basmod)
                        call dismoi('NB_MODES_TOT', basmod, 'RESULTAT', repi=nbbas)
                        k8bid = '  '
                        call mgutdm(model_gene, k8bid, numsst, 'NOM_NUME_DDL', ibid,&
                                    numddl)
                        call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neqs)
                        call wkvect('&&'//pgc//'.TRAV', 'V V R', neqs, ltvec)
!
!
! --- BOUCLE SUR LES MODES PROPRES DE LA BASE
!
                        do j = 1, nbbas
                            call dcapno(basmod, 'DEPL', j, chamba)
                            call jeveuo(chamba, 'L', llchab)
!
                            if (elim .ne. 0) then
                                iad=lmoet+ieq+j-1
                            else
                                iad=idresu+(zi(jnume+i)-1)*neqgen+&
                                nueq(1+ieq+j-2)-1
                            endif
!
! --- BOUCLE SUR LES EQUATIONS PHYSIQUES
!
                            do l = 1, neqs
                                zr(ltvec+l-1)=zr(ltvec+l-1)+zr(llchab+l-1)*zr(iad)
                            end do
                        end do
                        call jeveuo(jexnum(indirf, numsst), 'L', llind)
                        call jelira(jexnum(indirf, numsst), 'LONMAX', nbcou)
                        nbcou = nbcou/2
                        do l = 1, nbcou
                            idep = zi(llind+(l-1)*2)
                            iar = zi(llind+(l-1)*2+1)
                            zr(ldnew+iar-1) = zr(ltvec+idep-1)
                        end do
                        call jedetr('&&'//pgc//'.TRAV')
                    endif
!
                end do
                call rsnoch(nomres, chmp(ich), iarchi)
!
! --- ROTATION DU CHAMP AUX NOEUDS
!
                call rotchm(profno, zr(ldnew), zr(ltrotz), nbsst, zi( llinsk),&
                            nbnot, nbcmp, 3)
                call rotchm(profno, zr(ldnew), zr(ltroty), nbsst, zi( llinsk),&
                            nbnot, nbcmp, 2)
                call rotchm(profno, zr(ldnew), zr(ltrotx), nbsst, zi( llinsk),&
                            nbnot, nbcmp, 1)
!
            end do
            call rsadpa(nomres, 'E', 1, 'INST', iarchi,&
                        0, sjv=linst, styp=k8bid)
            zr(linst) = zr(jinst+i)
        end do
!
    endif
!
    call refdcp(trange, nomres)
!
! --- MENAGE
    call jedetr('&&'//pgc//'ROTX')
    call jedetr('&&'//pgc//'ROTY')
    call jedetr('&&'//pgc//'ROTZ')
    call jedetr('&&'//pgc//'.INDIR.SST')
!
    call jedema()
end subroutine
