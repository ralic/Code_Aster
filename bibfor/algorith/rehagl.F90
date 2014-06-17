subroutine rehagl(nomres, resgen, mailsk, profno)
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
!    T. KERBER     DATE 16/05/93
!-----------------------------------------------------------------------
!  BUT:      < RESTITUTION HARMONIQUE GLOBALE >
    implicit none
!
!      RESTITUER EN BASE PHYSIQUE SUR UN MAILLAGE SQUELETTE
!  LES RESULTATS ISSUS DE LA SOUS-STRUCTURATION GENERALE
!  LE CONCEPT RESULTAT EST UN RESULTAT COMPOSE "DYNA_HARMO"
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM K8 DU CONCEPT DYNA_HARMO RESULTAT
! RESGEN   /I/: NOM K8 DU RESULTAT GENERALISE AMONT
! MAILSK   /I/: NOM K8 DU MAILLAGE SQUELETTE SUPPORT
! PROFNO   /I/: NOM K19 DU PROF_CHNO DU SQUELETTE
!
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dcapno.h"
#include "asterfort/dismoi.h"
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
#include "asterfort/refdcp.h"
#include "asterfort/rotchc.h"
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
    character(len=4) :: champ(8)
    character(len=6) :: pgc
    character(len=8) :: chmp(3), crit, interp, k8b, nomres, basmod, mailsk
    character(len=8) :: modgen, resgen, soutr, k8bid, k8rep
    character(len=19) :: numddl, numgen, knume, kfreq, harmge, profno
    character(len=24) :: crefe(2), chamba, indirf, chamno, seliai, sizlia, sst
    character(len=24) :: valk, nomsst, k24bid
    integer :: itresu(3), elim, neqet, neqred, lmapro, lsilia, lsst, lmoet
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i1, iad, iar, iarchi, ibid, ich
    integer :: idep, idresu, ieq, ire1
    integer :: ire2, ire3, iret, iretou, j, jfreq, jnume
    integer :: k, k1, l, ldnew, lfreq, llchab, llind
    integer :: llinsk,   llors, llprs
    integer :: llrot, ltrotx, ltroty, ltrotz, ltvec, n1
    integer :: nbbas, nbcham, nbcmp, nbcou, nbfreq, nbnot
    integer :: nbsst, neq, neqgen, neqs, numsst, nutars
    integer, pointer :: nueq(:) => null()
    integer, pointer :: nllnequ(:) => null()
    character(len=24), pointer :: refn(:) => null()
!-----------------------------------------------------------------------
    data pgc   /'REHAGL'/
    data soutr /'&SOUSSTR'/
!-----------------------------------------------------------------------
!
    call jemarq()
!
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
!
    harmge = resgen
!
! --- CALCUL DU NOMBRE DE CHAMPS A RESTITUER ET LEURS ADDRESSES
!
    call jeexin(resgen//'           .DEPL', ire1)
    call jeexin(resgen//'           .VITE', ire2)
    call jeexin(resgen//'           .ACCE', ire3)
!
    if (ire1 .eq. 0 .and. ire2 .eq. 0 .and. ire3 .eq. 0) then
        valk = resgen
        call utmess('F', 'ALGORITH14_35', sk=valk)
    endif
!
    call getvtx(' ', 'TOUT_CHAM', nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        call getvtx(' ', 'TOUT_CHAM', scal=k8rep, nbret=n1)
    else
        k8rep=' '
    endif
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
        call jeveuo(harmge//'.DEPL', 'L', itresu(1))
        call jeveuo(harmge//'.VITE', 'L', itresu(2))
        call jeveuo(harmge//'.ACCE', 'L', itresu(3))
    else
! ----  ON RECHERCHE LES CHAMPS QU'IL FAUT RESTITUER
        call getvtx(' ', 'NOM_CHAM', nbval=0, nbret=n1)
        nbcham = -n1
        call getvtx(' ', 'NOM_CHAM', nbval=nbcham, vect=champ, nbret=n1)
! ----   BOUCLE SUR LES CHAMPS DEMANDES
        do i = 1, nbcham
!
            if (champ(i) .eq. 'DEPL') then
                chmp(i) = 'DEPL'
                call jeexin(harmge//'.DEPL', iret)
                if (iret .eq. 0) then
                    call utmess('F', 'ALGORITH10_11')
                else
                    call jeveuo(harmge//'.DEPL', 'L', itresu(i))
                endif
            else if (champ(i).eq.'VITE') then
                chmp(i) = 'VITE'
                call jeexin(harmge//'.VITE', iret)
                if (iret .eq. 0) then
                    call utmess('F', 'ALGORITH10_12')
                else
                    call jeveuo(harmge//'.VITE', 'L', itresu(i))
                endif
            else if (champ(i).eq.'ACCE') then
                chmp(i) = 'ACCE'
                call jeexin(harmge//'.ACCE', iret)
                if (iret .eq. 0) then
                    call utmess('F', 'ALGORITH10_13')
                else
                    call jeveuo(harmge//'.ACCE', 'L', itresu(i))
                endif
            else
! ----        SI LE CHAMP N'EST PAS DEPL,VITE OU ACCE ON PLANTE
                call utmess('F', 'ALGORITH10_16')
            endif
        end do
    endif
!     NOMBRE DE CHAMPS SYMBOLIQUES CALCULES.
!     ON S'ASSURE QUE LEUR NOMBRE EST NON NUL.
!
    if (nbcham .eq. 0) then
        valk = resgen
        call utmess('F', 'ALGORITH14_35', sk=valk)
    endif
!
! --- RECUPERATION DE LA NUMEROTATION ET DU MODELE GENERALISE
    call dismoi('NUME_DDL', harmge, 'RESU_DYNA', repk=k24bid)
    numgen(1:14)=k24bid(1:14)
    numgen(15:19) = '.NUME'
    call jeveuo(numgen//'.REFN', 'L', vk24=refn)
    k24bid=refn(1)
    modgen = k24bid(1:8)
    call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst)
    k8bid = '  '
    call mgutdm(modgen, k8bid, 1, 'NB_CMP_MAX', nbcmp,&
                k8bid)
    call jeveuo(numgen//'.NEQU', 'L', vi=nllnequ)
    neqgen = nllnequ(1)
!
! --- RECUPERATION DES ROTATIONS
    call wkvect('&&'//pgc//'ROTX', 'V V R', nbsst, ltrotx)
    call wkvect('&&'//pgc//'ROTY', 'V V R', nbsst, ltroty)
    call wkvect('&&'//pgc//'ROTZ', 'V V R', nbsst, ltrotz)
    do i = 1, nbsst
        call jeveuo(jexnum(modgen//'      .MODG.SSOR', i), 'L', llrot)
        zr(ltrotz+i-1) = zr(llrot)
        zr(ltroty+i-1) = zr(llrot+1)
        zr(ltrotx+i-1) = zr(llrot+2)
    end do
!
! --- CREATION DU PROF-CHAMNO
    call genugl(profno, indirf, modgen, mailsk)
    call jelira(profno//'.NUEQ', 'LONMAX', neq)
!
! --- RECUPERATION DU NOMBRE DE NOEUDS
    call dismoi('NB_NO_MAILLA', mailsk, 'MAILLAGE', repi=nbnot)
!
! --- INFORMATIONS POUR CREATION DES CHAMNO A PARTIR DES .REFE
    crefe(1) = mailsk
    crefe(2) = profno
!
! --- RECUPERATION DES FREQUENCES
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=n1)
    call getvtx(' ', 'INTERPOL', scal=interp, nbret=n1)
!
    knume = '&&RETREC.NUM_RANG'
    kfreq = '&&RETREC.FREQ'
    call rstran(interp, harmge, ' ', 1, kfreq,&
                knume, nbfreq, iretou)
    if (iretou .ne. 0) then
        call utmess('F', 'ALGORITH10_47')
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
!-- ON TESTE SI ON A EU RECOURS A L'ELIMINATION
!
    seliai=numgen(1:14)//'.ELIM.BASE'
    sizlia=numgen(1:14)//'.ELIM.TAIL'
    sst=   numgen(1:14)//'.ELIM.NOMS'
!
    call jeexin(seliai, elim)
    if (elim .ne. 0) then
        neqet=0
        call jeveuo(numgen//'.NEQU', 'L', ibid)
        neqred=zi(ibid)
        nomsst=modgen//'      .MODG.SSNO'
        call jeveuo(seliai, 'L', lmapro)
        call jeveuo(sizlia, 'L', lsilia)
        call jeveuo(sst, 'L', lsst)
        do i = 1, nbsst
            neqet=neqet+zi(lsilia+i-1)
        end do
        call wkvect('&&MODE_ETENDU_REST_ELIM', 'V V C', neqet, lmoet)
    endif
!
! -------------------------------------
! --- RESTITUTION SUR BASE PHYSIQUE ---
! -------------------------------------
!
    call jeveuo(numgen//'.NUEQ', 'L', vi=nueq)
    call jenonu(jexnom(numgen//'.LILI', soutr), ibid)
    call jeveuo(jexnum(numgen//'.ORIG', ibid), 'L', llors)
    call jenonu(jexnom(numgen//'.LILI', soutr), ibid)
    call jeveuo(jexnum(numgen//'.PRNO', ibid), 'L', llprs)
!
    iarchi = 0
!
    if (interp(1:3) .ne. 'NON') then
        call utmess('F', 'ALGORITH3_86')
    else
!
!        CALL JEEXIN(HARMGE//'.ORDR',IRET)
!        IF (IRET.NE.0.AND.ZI(JNUME).EQ.1) IARCHI=0
!
        do i = 0, nbfreq-1
            iarchi = iarchi + 1
!
            do ich = 1, nbcham
                idresu = itresu(ich)
!-- SI ELIMINATION, ON RESTITUE D'ABORD LES MODES GENERALISES
                if (elim .ne. 0) then
                    do i1 = 1, neqet
                        zc(lmoet+i1-1)=dcmplx(0.d0,0.d0)
                        do k1 = 1, neqred
                            zc(lmoet+i1-1)=zc(lmoet+i1-1)+ zr(lmapro+(&
                            k1-1)*neqet+i1-1)* zc(idresu+k1-1+(zi(&
                            jnume+i)-1)*neqred)
                        end do
                    end do
                endif
                call rsexch(' ', nomres, chmp(ich), iarchi, chamno,&
                            iret)
                if (iret .eq. 0) then
                    call utmess('A', 'ALGORITH2_64', sk=chamno)
                else if (iret.eq.100) then
                    call vtcrea(chamno, crefe, 'G', 'C', neq)
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
                            nutars = 0
                            do j = 1, nbsst
                                if (zi(llors+j-1) .eq. numsst) nutars=j
                            end do
                            ieq=zi(llprs+(nutars-1)*2)
                        endif
                        k8bid = '  '
                        call mgutdm(modgen, k8bid, numsst, 'NOM_BASE_MODALE', ibid,&
                                    basmod)
                        call dismoi('NB_MODES_TOT', basmod, 'RESULTAT', repi=nbbas)
                        k8bid = '  '
                        call mgutdm(modgen, k8bid, numsst, 'NOM_NUME_DDL', ibid,&
                                    numddl)
                        call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neqs)
                        call wkvect('&&'//pgc//'.TRAV', 'V V C', neqs, ltvec)
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
                                zc(ltvec+l-1)=zc(ltvec+l-1)+zr(llchab+&
                                l-1)*zc(iad)
                            end do
                        end do
                        call jeveuo(jexnum(indirf, numsst), 'L', llind)
                        call jelira(jexnum(indirf, numsst), 'LONMAX', nbcou)
                        nbcou = nbcou/2
                        do l = 1, nbcou
                            idep = zi(llind+(l-1)*2)
                            iar = zi(llind+(l-1)*2+1)
                            zc(ldnew+iar-1) = zc(ltvec+idep-1)
                        end do
                        call jedetr('&&'//pgc//'.TRAV')
                    endif
!
                end do
                call rsnoch(nomres, chmp(ich), iarchi)
!
! --- ROTATION DU CHAMP AUX NOEUDS
!
                call rotchc(profno, zc(ldnew), zr(ltrotz), nbsst, zi( llinsk),&
                            nbnot, nbcmp, 3)
                call rotchc(profno, zc(ldnew), zr(ltroty), nbsst, zi( llinsk),&
                            nbnot, nbcmp, 2)
                call rotchc(profno, zc(ldnew), zr(ltrotx), nbsst, zi( llinsk),&
                            nbnot, nbcmp, 1)
!
            end do
            call rsadpa(nomres, 'E', 1, 'FREQ', iarchi,&
                        0, sjv=lfreq, styp=k8b)
            zr(lfreq) = zr(jfreq+i)
        end do
!
    endif
!
    call refdcp(basmod, nomres)
!
! --- MENAGE
    call jedetr('&&'//pgc//'ROTX')
    call jedetr('&&'//pgc//'ROTY')
    call jedetr('&&'//pgc//'ROTZ')
    call jedetr('&&'//pgc//'.INDIR.SST')
!
    goto 999
!
999 continue
!
    call jedema()
end subroutine
