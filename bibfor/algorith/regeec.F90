subroutine regeec(nomres, resgen, nomsst)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/dcapno.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvis.h"
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
#include "asterfort/nueq_chck.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrea.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomres, resgen, nomsst
!-----------------------------------------------------------------------
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
!
!  BUT : < RESTITUTION GENERALISEE ECLATEE >
!
!  RESTITUER EN BASE PHYSIQUE SUR UNE SOUS-STRUCTURE LES RESULTATS
!  ISSUS DE LA SOUS-STRUCTURATION GENERALE
!  LE CONCEPT RESULTAT EST UN RESULTAT COMPOSE "MODE_MECA"
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM K8 DU CONCEPT MODE MECA RESULTAT
! RESGEN /I/ : NOM K8 DU MODE_GENE AMONT
! NOMSST /I/ : NOM K8 DE LA SOUS-STRUCTURE SUR LAQUELLE ON RESTITUE
!
!
!
!
!
    integer :: i, iad, ibid, ieq, ier, iord, j, jbid, k, llchab, i_ligr_ss
    integer :: llchol, llors, llprs, vali(2), nbbas, nbddg, nbmod(1), nbsst
    integer :: neq, nno, numo, nusst, nutars, iadpar(6)
    integer :: elim, neqet, neqred, lmapro, lsilia, lsst, lmoet, i1, k1
    real(kind=8) :: freq, genek, genem, omeg2, rbid
    character(len=8) :: kbid, basmod, mailla, lint, model_gene
    character(len=16) :: depl, nompar(6), typres, quamod
    character(len=19) :: raid, numddl, chamne, prof_gene
    character(len=14) :: nume_gene
    character(len=24) :: crefe(2), chamol, chamba
    character(len=24) :: valk(2), seliai, sizlia, sst
    complex(kind=8) :: cbid
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: nueq(:) => null()
    character(len=24), pointer :: refn(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!
!-----------------------------------------------------------------------
    data depl   /'DEPL            '/
    data nompar /'FREQ','RIGI_GENE','MASS_GENE','OMEGA2','NUME_MODE',&
     &              'TYPE_MODE'/
!-----------------------------------------------------------------------
!
    call jemarq()
    call titre()
!
! --- RECUPERATION DU MODELE GENERALISE
!
    call dismoi('REF_RIGI_PREM', resgen, 'RESU_DYNA', repk=raid)
!
    call jeveuo(raid//'.REFA', 'L', vk24=refa)
    nume_gene = refa(2)(1:14)
    prof_gene = nume_gene(1:14)//'.NUME'
    call nueq_chck(prof_gene, neqred)
    call jelibe(raid//'.REFA')
!
    call jeveuo(prof_gene//'.REFN', 'L', vk24=refn)
    model_gene = refn(1)(1:8)
    call jelibe(prof_gene//'.REFN')
!
! --- RECUPERATION NUMERO DE SOUS-STRUCTURE
!     ET DU NOEUD TARDIF CORRESPONDANT
!
    call jenonu(jexnom(model_gene//'      .MODG.SSNO', nomsst), nusst)
    if (nusst .eq. 0) then
        valk (1) = model_gene
        valk (2) = nomsst
        call utmess('F', 'ALGORITH14_25', nk=2, valk=valk)
    endif
!
!
!
!-- ON TESTE SI ON A EU RECOURS A L'ELIMINATION
!
    seliai=nume_gene(1:14)//'.ELIM.BASE'
    sizlia=nume_gene(1:14)//'.ELIM.TAIL'
    sst=   nume_gene(1:14)//'.ELIM.NOMS'
!
    call jeexin(seliai, elim)
!
    if (elim .eq. 0) then
!
        call jenonu(jexnom(prof_gene//'.LILI', '&SOUSSTR'), i_ligr_ss)
        call jeveuo(jexnum(prof_gene//'.ORIG', i_ligr_ss), 'L', llors)
        call jelira(jexnum(prof_gene//'.ORIG', i_ligr_ss), 'LONMAX', nbsst)
!
        nutars=0
        do i = 1, nbsst
            if (zi(llors+i-1) .eq. nusst) nutars=i
        end do
!
!
        call jeveuo(jexnum(prof_gene//'.PRNO', i_ligr_ss), 'L', llprs)
        nbddg=zi(llprs+(nutars-1)*2+1)
        ieq=zi(llprs+(nutars-1)*2)
!
    else
!
        call jelira(model_gene//'      .MODG.SSNO', 'NOMMAX', nbsst)
        call jeveuo(sst, 'L', ibid)
        do i1 = 1, nbsst
            if (nomsst .eq. zk8(ibid+i1-1)) then
                nusst=i1
            endif
        end do
        neqet=0
        ieq=0
!
        call jeveuo(seliai, 'L', lmapro)
        call jeveuo(sizlia, 'L', lsilia)
        call jeveuo(sst, 'L', lsst)
        ibid=1
        do i = 1, nbsst
            neqet=neqet+zi(lsilia+i-1)
        end do
!
        ieq=0
        do i1 = 1, nusst-1
            ieq=ieq+zi(lsilia+i1-1)
        end do
        call wkvect('&&MODE_ETENDU_REST_ELIM', 'V V R', neqet, lmoet)
!
    endif
!
! --- RECUPERATION DE LA BASE MODALE
!
    call mgutdm(model_gene, nomsst, ibid, 'NOM_BASE_MODALE', ibid,&
                basmod)
!
    call refdcp(basmod, nomres)
!
    call dismoi('NB_MODES_TOT', basmod, 'RESULTAT', repi=nbbas)
!
    if (elim .eq. 0) then
        if (nbbas .ne. nbddg) then
            valk (1) = basmod
            vali (1) = nbbas
            vali (2) = nbddg
            call utmess('F', 'ALGORITH14_26', sk=valk(1), ni=2, vali=vali)
        endif
    endif
!
    call dismoi('REF_INTD_PREM', basmod, 'RESU_DYNA', repk=lint)
!
    call dismoi('NOM_MAILLA', lint, 'INTERF_DYNA', repk=mailla)
    call dismoi('NOM_NUME_DDL', lint, 'INTERF_DYNA', repk=numddl)
    call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
!
    crefe(1)=mailla
    crefe(2)=numddl
!
! --- RECUPERATION NOMBRE DE MODES PROPRES CALCULES
!
    call rsorac(resgen, 'LONUTI', 0, rbid, kbid,&
                cbid, rbid, kbid, nbmod, 1,&
                ibid)
!
! --- ON RESTITUE SUR TOUS LES MODES OU SUR QUELQUES MODES:
!
!
    call getres(kbid, typres, quamod)
    if (quamod .ne. 'CALC_CORR_SSD') then
        call getvis(' ', 'NUME_ORDRE', nbval=0, nbret=nno)
    else
!-- SI ON APPELLE DEPUIS QUAL_MODL, ON RESTITUE TOUS LES MODES
        nno=0
    endif
!
    if (nno .ne. 0) then
        nbmod(1) = -nno
        call wkvect('&&REGEEC.NUME', 'V V I', nbmod(1), jbid)
        call getvis(' ', 'NUME_ORDRE', nbval=nbmod(1), vect=zi(jbid), nbret=nno)
    else
        call wkvect('&&REGEEC.NUME', 'V V I', nbmod(1), jbid)
        do i = 1, nbmod(1)
            zi(jbid+i-1) = i
        end do
    endif
!
! --- ALLOCATION STRUCTURE DE DONNEES RESULTAT
!
    call rscrsd('G', nomres, 'MODE_MECA', nbmod(1))
!
! --- RESTITUTION PROPREMENT DITE
!
    call jeveuo(prof_gene//'.NUEQ', 'L', vi=nueq)
!
! --- BOUCLE SUR LES MODES A RESTITUER
    do i = 1, nbmod(1)
        iord = zi(jbid+i-1)
!
! ----- REQUETTE NOM ET ADRESSE CHAMNO GENERALISE
        call dcapno(resgen, depl, iord, chamol)
        call jeveuo(chamol, 'L', llchol)
!-- SI ELIMINATION, ON RESTITUE D'ABORD LES MODES GENERALISES
        if (elim .ne. 0) then
            do i1 = 1, neqet
                zr(lmoet+i1-1)=0.d0
                do k1 = 1, neqred
                    zr(lmoet+i1-1)=zr(lmoet+i1-1)+ zr(lmapro+(k1-1)*&
                    neqet+i1-1)* zr(llchol+k1-1)
                end do
            end do
            llchol=lmoet
        endif
!
! ----- REQUETTE NOM ET ADRESSE NOUVEAU CHAMNO
        call rsexch(' ', nomres, depl, i, chamne,&
                    ier)
        call vtcrea(chamne, crefe, 'G', 'R', neq)
        call jeveuo(chamne//'.VALE', 'E', vr=vale)
!
        call rsadpa(resgen, 'L', 5, nompar, iord,&
                    0, tjv=iadpar, styp=kbid)
        freq = zr(iadpar(1))
        genek = zr(iadpar(2))
        genem = zr(iadpar(3))
        omeg2 = zr(iadpar(4))
        numo = zi(iadpar(5))
!
! ----- BOUCLE SUR LES MODES PROPRES DE LA BASE
        if (elim .ne. 0) then
            ibid=nbbas
        else
            ibid=nbddg
        endif
        do j = 1, ibid
            call dcapno(basmod, depl, j, chamba)
            call jeveuo(chamba, 'L', llchab)
!
! ------- BOUCLE SUR LES EQUATIONS PHYSIQUES
            do k = 1, neq
                if (elim .ne. 0) then
                    iad=llchol+ieq+j-1
                else
                    iad=llchol+nueq(1+ieq+j-2)-1
                endif
                vale(k) = vale(k) + zr(llchab+k-1)*zr(iad)
            end do
            call jelibe(chamba)
        end do
        call rsnoch(nomres, depl, i)
        call rsadpa(nomres, 'E', 6, nompar, i,&
                    0, tjv=iadpar, styp=kbid)
        zr(iadpar(1)) = freq
        zr(iadpar(2)) = genek
        zr(iadpar(3)) = genem
        zr(iadpar(4)) = omeg2
        zi(iadpar(5)) = numo
        zk16(iadpar(6)) = 'MODE_DYN'
!
        call jelibe(chamol)
    end do
!
    call jelibe(prof_gene//'.NUEQ')
    call jedetr('&&REGEEC.NUME')
!
    call jedema()
end subroutine
