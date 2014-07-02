subroutine rdtmai(noma, nomare, base, corrn, corrm,&
                  bascor, nbmal, lima)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/cargeo.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: noma, nomare
    character(len=*) :: corrn, corrm
    character(len=1) :: base, bascor
    integer :: nbmal, lima(*)
!
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
! person_in_charge: nicolas.sellenet at edf.fr
!
! ======================================================================
!     BUT: REDUIRE UN MAILLAGE SUR UNE LISTE DE MAILLES
!
!  NOMA : IN  : MAILLAGE A REDUIRE
!  NOMARE : OUT : MAILLAGE REDUIT
!  BASE   : IN  : 'G' OU 'V' : BASE POUR LA CREATION DE NOMARE
!  CORRN  : IN/JXOUT : SI != ' ' : NOM DE L'OBJET QUI CONTIENDRA
!           LA CORRESPONDANCE INO_RE -> INO
!  CORRM  : IN/JXOUT : SI != ' ' : NOM DE L'OBJET QUI CONTIENDRA
!           LA CORRESPONDANCE IMA_RE -> IMA
!  BASCOR : IN  : 'G' OU 'V' : BASE POUR LA CREATION DE CORRN ET CORRM
!  NBMAL  : IN   : / =0 => LA LISTE DE MAILLES EST OBTENUE EN SCRUTANT
!                          LE MOT CLE RESTREINT
!                : / >0 => LA LISTE DE MAILLE EST FOURNIE PAR LIMA
!  LIMA   : IN   : SI NBMAL> 0 : LISTE DES NUMEROS DE MAILLES SUR
!                  LESQUELLES IL FAUT REDUIRE LE MAILLAGE.
! ======================================================================
!
    integer :: nbmaou, nbnoin, iret, jnuma, jwk1, jconx2, ima, numa
    integer :: nbno
    integer :: ino, nuno, jdim, itypou, jadin, jadou, ibid
    integer :: jcorou
    integer :: iad, ntgeo, nbnoou, nbnomx, jwk2, nbgma, jgma, igm, nbma, nbmain
    integer :: jwk3, nbgmin, jgmanv, nbgmnv, k, jnmpg, nmpg, nbgno, jmaor
    integer :: nbgnin, jgnonv, jnnpg, nbgnnv, ign, nnpg, numgno
    integer :: jcorrm, imain, imaou, n1
    character(len=4) :: docu
    character(len=8) :: typmcl(2), nomres
    character(len=16) :: motcle(2), nomcmd, typres
    character(len=8) :: nomma, nomno, ttgrma, ttgrno
    character(len=24) :: nommai, nomnoe, grpnoe, cooval, cooref, coodsc
    character(len=24) :: grpmai, connex, typmai, dimin, dimou, nomgma, nomgno
    character(len=24) :: ptngrn, ptngrm, valk(2)
    aster_logical :: lvide
    character(len=24), pointer :: grp_noeu_in(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: vconnex(:) => null()
    integer, pointer :: typmail(:) => null()
    integer, pointer :: num_noeu_in(:) => null()
!
    call jemarq()
!
    ASSERT(noma.ne.nomare)
    ASSERT(base.eq.'V' .or. base.eq.'G')
!
!
! -1- PRELIMINAIRES
!     ============
!
    call getres(nomres, typres, nomcmd)
!
!
! --- CALCUL DE LA LISTE DES MAILLES SUR LESQUELLES IL FAUT REDUIRE :
    if (nbmal .eq. 0) then
        motcle(1)='GROUP_MA'
        motcle(2)='MAILLE'
        typmcl(1)='GROUP_MA'
        typmcl(2)='MAILLE'
        call reliem(' ', noma, 'NU_MAILLE', 'RESTREINT', 1,&
                    2, motcle, typmcl, '&&RDTMAI.NUM_MAIL_IN', nbmaou)
        call jeveuo('&&RDTMAI.NUM_MAIL_IN', 'L', jnuma)
    else
        nbmaou=nbmal
        call wkvect('&&RDTMAI.NUM_MAIL_IN', 'V V I', nbmaou, jnuma)
        do k = 1, nbmaou
            zi(jnuma-1+k)=lima(k)
        end do
    endif
!
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoin)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmain)
!
! --- CREATION DE TABLEAUX DE TRAVAIL:
!     ZI(JWK1) :
!     - DIMENSIONNE AU NOMBRE DE NOEUDS DU MAILLAGE IN
!     - CORRESPONDANCE : NUMEROS DES NOEUDS MAILLAGE IN => MAILLAGE OUT
!     - EX: ZI(JWK1+INO1-1)=INO2
!         -> SI INO2!=0:LE NOEUD INO1 DU MAILLAGE IN CORRESPOND AU NOEUD
!                       INO2 DU MAILLAGE OUT.
!         -> SI INO2=0: LE NOEUD INO1 DU MAILLAGE IN N'EST PAS PRESENT
!                       DANS LE MAILLAGE OUT.
!
    call wkvect('&&RDTMAI_WORK_1', 'V V I', nbnoin, jwk1)
!
!     ZI(JWK2) : (L'INVERSE DE ZI(JWK1))
!     - DIMENSIONNE AU NOMBRE DE NOEUDS DU MAILLAGE IN
!     - CORRESPONDANCE : NUMEROS DES NOEUDS MAILLAGE OUT => MAILLAGE IN
!     - EX: ZI(JWK1+INO1-1)=INO2
!        -> LE NOEUD INO1 DU MAILLAGE OUT CORRESPOND AU NOEUD
!           INO2 DU MAILLAGE IN.
    call wkvect('&&RDTMAI_WORK_2', 'V V I', nbnoin, jwk2)
!
!     ZI(JWK3) :
!     - DIMENSIONNE AU NOMBRE DE MAILLES DU MAILLAGE IN
!     - CORRESPONDANCE : NUMEROS DES MAILLES MAILLAGE IN => MAILLAGE OUT
!     - EX: ZI(JWK3+IMA1-1)=IMA2
!         -> SI IMA2!=0:LA MAILLE IMA1 DU MAILLAGE IN CORRESPOND A
!                       LA MAILLE IMA2 DU MAILLAGE OUT.
!         -> SI IMA2=0: LA MAILLE IMA1 DU MAILLAGE IN N'EST PAS PRESENTE
!                       DANS LE MAILLAGE OUT.
    call wkvect('&&RDTMAI_WORK_3', 'V V I', nbmain, jwk3)
!
!
! ---  REMPLISSAGE DES TABLEAUX DE TRAVAIL
    call jeveuo(noma//'.CONNEX', 'L', vi=vconnex)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    nbnoou=0
    do ima = 1, nbmaou
        numa=zi(jnuma+ima-1)
        zi(jwk3+numa-1)=ima
        nbno=zi(jconx2+numa)-zi(jconx2+numa-1)
        do ino = 1, nbno
            nuno=vconnex(zi(jconx2+numa-1)+ino-1)
            if (zi(jwk1+nuno-1) .eq. 0) then
                nbnoou=nbnoou+1
                zi(jwk1+nuno-1)=nbnoou
                zi(jwk2+nbnoou-1)=nuno
            endif
        end do
    end do
!
!   -- il faut ajouter les noeuds demandes par l'utlisateur :
    call reliem(' ', noma, 'NU_NOEUD', 'RESTREINT', 1,&
                1, ['GROUP_NO'], ['GROUP_NO'], '&&RDTMAI.NUM_NOEU_IN', n1)
    if (n1 .gt. 0) then
        call jeveuo('&&RDTMAI.NUM_NOEU_IN', 'L', vi=num_noeu_in)
        do ino = 1, n1
            nuno=num_noeu_in(ino)
            if (zi(jwk1+nuno-1) .eq. 0) then
                nbnoou=nbnoou+1
                zi(jwk1+nuno-1)=nbnoou
                zi(jwk2+nbnoou-1)=nuno
            endif
        end do
    endif
!
!
!
! -2- CREATION DU NOUVEAU MAILLAGE
!     ============================
!
    nommai=nomare//'.NOMMAI         '
    nomnoe=nomare//'.NOMNOE         '
    grpnoe=nomare//'.GROUPENO'
    grpmai=nomare//'.GROUPEMA'
    ptngrn=nomare//'.PTRNOMNOE'
    ptngrm=nomare//'.PTRNOMMAI'
!
    connex=nomare//'.CONNEX         '
    typmai=nomare//'.TYPMAIL        '
    cooval=nomare//'.COORDO    .VALE'
    coodsc=nomare//'.COORDO    .DESC'
    cooref=nomare//'.COORDO    .REFE'
!
! --- OBJET .DIME
    dimin=noma//'.DIME'
    dimou=nomare//'.DIME'
    call jedupo(dimin, base, dimou, .false._1)
    call jeveuo(dimou, 'E', jdim)
    zi(jdim-1+1)=nbnoou
    zi(jdim-1+3)=nbmaou
!
! --- OBJET .NOMMAI
    call jecreo(nommai, base//' N K8')
    call jeecra(nommai, 'NOMMAX', nbmaou)
    do ima = 1, nbmaou
        call jenuno(jexnum(noma//'.NOMMAI', zi(jnuma+ima-1)), nomma)
        call jecroc(jexnom(nommai, nomma))
    end do
!
! --- OBJET .TYPMAIL
    call wkvect(typmai, base//' V I', nbmaou, itypou)
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
    do ima = 1, nbmaou
        zi(itypou-1+ima)=typmail(zi(jnuma+ima-1))
    end do
!
! --- OBJET .CONNEX
    call jecrec(connex, base//' V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmaou)
    call dismoi('NB_NO_MAX', '&CATA', 'CATALOGUE', repi=nbnomx)
!
    call jeecra(connex, 'LONT', nbnomx*nbmaou, ' ')
    do ima = 1, nbmaou
        call jelira(jexnum(noma//'.CONNEX', zi(jnuma+ima-1)), 'LONMAX', nbno)
        call jeecra(jexnum(connex, ima), 'LONMAX', nbno)
        call jeveuo(jexnum(noma//'.CONNEX', zi(jnuma+ima-1)), 'L', jadin)
        call jeveuo(jexnum(connex, ima), 'E', jadou)
        do ino = 1, nbno
            zi(jadou+ino-1)=zi(jwk1+zi(jadin+ino-1)-1)
        end do
    end do
!
! --- OBJET .NOMNOE
    call jecreo(nomnoe, base//' N K8')
    call jeecra(nomnoe, 'NOMMAX', nbnoou)
    do ino = 1, nbnoou
        call jenuno(jexnum(noma//'.NOMNOE', zi(jwk2+ino-1)), nomno)
        call jecroc(jexnom(nomnoe, nomno))
    end do
!
! --- OBJET .COORDO.VALE
    call jecreo(cooval, base//' V R')
    call jeecra(cooval, 'LONMAX', nbnoou*3)
    call jelira(noma//'.COORDO    .VALE', 'DOCU', cval=docu)
    call jeecra(cooval, 'DOCU', cval=docu)
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
    call jeveuo(cooval, 'E', jcorou)
    do ino = 1, nbnoin
        if (zi(jwk1+ino-1) .ne. 0) then
            zr(jcorou+3*(zi(jwk1+ino-1)-1))=vale(1+3*(ino-1))
            zr(jcorou+3*(zi(jwk1+ino-1)-1)+1)=vale(1+3*(ino-1)+1)
            zr(jcorou+3*(zi(jwk1+ino-1)-1)+2)=vale(1+3*(ino-1)+2)
        endif
    end do
!
!
! --- OBJET COORDO.DESC
    call jecreo(coodsc, base//' V I')
    call jeecra(coodsc, 'LONMAX', 3)
    call jeecra(coodsc, 'DOCU', cval='CHNO')
    call jeveuo(coodsc, 'E', iad)
    call jenonu(jexnom('&CATA.GD.NOMGD', 'GEOM_R'), ntgeo)
    zi(iad)=ntgeo
    zi(iad+1)=-3
    zi(iad+2)=14
!
!
! --- OBJET COORDO.REFE
    call wkvect(cooref, base//' V K24', 4, iad)
    zk24(iad)=nomare
!
!
!     --- OBJET .GROUPEMA
!     --------------------
    if (nbmal .eq. 0) then
        call getvtx('RESTREINT', 'TOUT_GROUP_MA', iocc=1, scal=ttgrma, nbret=iret)
    else
        ttgrma='OUI'
    endif
    if (ttgrma .eq. 'NON') then
!       'TOUT_GROUP_MA'='NON'
        call getvtx('RESTREINT', 'GROUP_MA', iocc=1, nbval=0, nbret=nbgma)
        nbgma=-nbgma
        if (nbgma .eq. 0) goto 141
        call wkvect('&&RDTMAI_GRMA_FOURNIS', 'V V K24', nbgma, jgma)
        call getvtx('RESTREINT', 'GROUP_MA', iocc=1, nbval=nbgma, vect=zk24(jgma),&
                    nbret=iret)
        call jecreo(ptngrm, base//' N K24')
        call jeecra(ptngrm, 'NOMMAX', nbgma)
        call jecrec(grpmai, base//' V I', 'NO '//ptngrm, 'DISPERSE', 'VARIABLE',&
                    nbgma)
        do igm = 1, nbgma
            nomgma=zk24(jgma+igm-1)
            call jecroc(jexnom(grpmai, nomgma))
            call jelira(jexnom(noma//'.GROUPEMA', nomgma), 'LONUTI', nbma)
            call jeecra(jexnom(grpmai, nomgma), 'LONMAX', max(nbma, 1))
            call jeecra(jexnom(grpmai, nomgma), 'LONUTI', nbma)
            call jeveuo(jexnom(noma//'.GROUPEMA', nomgma), 'L', jadin)
            call jeveuo(jexnom(grpmai, nomgma), 'E', jadou)
            do ima = 1, nbma
                zi(jadou+ima-1)=zi(jwk3+zi(jadin+ima-1)-1)
            end do
        end do
    else
!       TOUT_GROUP_MA='OUI'
        call jelira(noma//'.GROUPEMA', 'NOMUTI', nbgmin)
        call wkvect('&&RDTMAI_GRMA_NON_VIDES', 'V V I', nbgmin, jgmanv)
        call wkvect('&&RDTMAI_NB_MA_PAR_GRMA', 'V V I', nbgmin, jnmpg)
        nbgmnv=0
        do igm = 1, nbgmin
            call jeveuo(jexnum(noma//'.GROUPEMA', igm), 'L', jadin)
            call jelira(jexnum(noma//'.GROUPEMA', igm), 'LONUTI', nbma)
            nmpg=0
            lvide=.true.
            do ima = 1, nbma
                if (zi(jwk3+zi(jadin+ima-1)-1) .ne. 0) then
                    if (lvide) then
                        nbgmnv=nbgmnv+1
                        zi(jgmanv+nbgmnv-1)=igm
                        lvide=.false.
                    endif
                    nmpg=nmpg+1
                endif
            end do
            if (.not.lvide) then
                zi(jnmpg+nbgmnv-1)=nmpg
            endif
        end do
        call jecreo(ptngrm, base//' N K24')
        call jeecra(ptngrm, 'NOMMAX', nbgmnv)
        call jecrec(grpmai, base//' V I', 'NO '//ptngrm, 'DISPERSE', 'VARIABLE',&
                    nbgmnv)
        do igm = 1, nbgmnv
            call jenuno(jexnum(noma//'.GROUPEMA', zi(jgmanv+igm-1)), nomgma)
            call jecroc(jexnom(grpmai, nomgma))
            call jelira(jexnom(noma//'.GROUPEMA', nomgma), 'LONUTI', nbma)
            ibid=max(zi(jnmpg+igm-1),1)
            call jeecra(jexnom(grpmai, nomgma), 'LONMAX', ibid)
            call jeecra(jexnom(grpmai, nomgma), 'LONUTI', zi(jnmpg+igm-1))
            call jeveuo(jexnom(noma//'.GROUPEMA', nomgma), 'L', jadin)
            call jeveuo(jexnom(grpmai, nomgma), 'E', jadou)
            k=0
            do ima = 1, nbma
                if (zi(jwk3+zi(jadin+ima-1)-1) .ne. 0) then
                    k=k+1
                    zi(jadou+k-1)=zi(jwk3+zi(jadin+ima-1)-1)
                endif
            end do
        end do
    endif
141 continue
!
!
!
!     --- OBJET .GROUPENO
!     --------------------
    if (nbmal .eq. 0) then
        call getvtx('RESTREINT', 'TOUT_GROUP_NO', iocc=1, scal=ttgrno, nbret=iret)
        call getvtx('RESTREINT', 'GROUP_NO', iocc=1, nbval=0, nbret=nbgno)
    else
        ttgrno='OUI'
        nbgno=0
    endif
!
    if (nbgno .ne. 0) then
        nbgno=-nbgno
        AS_ALLOCATE(vk24=grp_noeu_in, size=nbgno)
        call getvtx('RESTREINT', 'GROUP_NO', iocc=1, nbval=nbgno, vect=grp_noeu_in,&
                    nbret=iret)
    endif
!
!     SI 'TOUT_GROUP_NO'='NON' ET 'GROUP_NO' ABSENT => ON SORT
    if (ttgrno .eq. 'NON' .and. nbgno .eq. 0) goto 210
!
    if (ttgrno .eq. 'NON') then
!       'TOUT_GROUP_MA'='NON' ET 'GROUP_NO' PRESENT
        call wkvect('&&RDTMAI_GRNO_NON_VIDES', 'V V I', nbnoou, jgnonv)
        call wkvect('&&RDTMAI_NB_NO_PAR_GRNO', 'V V I', nbnoou, jnnpg)
        nbgnnv=0
        do ign = 1, nbgno
            call jenonu(jexnom(noma//'.GROUPENO', grp_noeu_in(ign)), numgno)
            if (numgno .eq. 0) then
                valk(1) = grp_noeu_in(ign)
                valk(2) = noma
                call utmess('F', 'CALCULEL6_82', nk=2, valk=valk)
            endif
            call jeveuo(jexnom(noma//'.GROUPENO', grp_noeu_in(ign)), 'L', jadin)
            call jelira(jexnom(noma//'.GROUPENO', grp_noeu_in(ign)), 'LONMAX', nbno)
            nnpg=0
            lvide=.true.
            do ino = 1, nbno
                if (zi(jwk1+zi(jadin+ino-1)-1) .ne. 0) then
                    if (lvide) then
                        nbgnnv=nbgnnv+1
                        zi(jgnonv+nbgnnv-1)=numgno
                        lvide=.false.
                    endif
                    nnpg=nnpg+1
                endif
            end do
            if (.not.lvide) zi(jnnpg+nbgnnv-1)=nnpg
        end do
!
    else
!       TOUT_GROUP_NO='OUI'
        call jelira(noma//'.GROUPENO', 'NOMUTI', nbgnin)
        call wkvect('&&RDTMAI_GRNO_NON_VIDES', 'V V I', nbgnin, jgnonv)
        call wkvect('&&RDTMAI_NB_NO_PAR_GRNO', 'V V I', nbgnin, jnnpg)
        nbgnnv=0
        do ign = 1, nbgnin
            call jeveuo(jexnum(noma//'.GROUPENO', ign), 'L', jadin)
            call jelira(jexnum(noma//'.GROUPENO', ign), 'LONUTI', nbno)
            nnpg=0
            lvide=.true.
            do ino = 1, nbno
                if (zi(jwk1+zi(jadin+ino-1)-1) .ne. 0) then
                    if (lvide) then
                        nbgnnv=nbgnnv+1
                        zi(jgnonv+nbgnnv-1)=ign
                        lvide=.false.
                    endif
                    nnpg=nnpg+1
                endif
            end do
            if (.not.lvide) then
                zi(jnnpg+nbgnnv-1)=nnpg
            endif
        end do
    endif
!
!     SI AUCUN GROUPE DE NOEUD N'EST A CREER, ON SORT
    if (nbgnnv .eq. 0) goto 210
!
    call jecreo(ptngrn, base//' N K24')
    call jeecra(ptngrn, 'NOMMAX', nbgnnv)
    call jecrec(grpnoe, base//' V I', 'NO '//ptngrn, 'DISPERSE', 'VARIABLE',&
                nbgnnv)
    do ign = 1, nbgnnv
        call jenuno(jexnum(noma//'.GROUPENO', zi(jgnonv+ign-1)), nomgno)
        call jecroc(jexnom(grpnoe, nomgno))
        call jelira(jexnom(noma//'.GROUPENO', nomgno), 'LONUTI', nbno)
        ibid=max(zi(jnnpg+ign-1),1)
        call jeecra(jexnom(grpnoe, nomgno), 'LONMAX', ibid)
        call jeecra(jexnom(grpnoe, nomgno), 'LONUTI', zi(jnnpg+ign-1))
        call jeveuo(jexnom(noma//'.GROUPENO', nomgno), 'L', jadin)
        call jeveuo(jexnom(grpnoe, nomgno), 'E', jadou)
        k=0
        do ino = 1, nbno
            if (zi(jwk1+zi(jadin+ino-1)-1) .ne. 0) then
                k=k+1
                zi(jadou+k-1)=zi(jwk1+zi(jadin+ino-1)-1)
            endif
        end do
    end do
210 continue
!
    call cargeo(nomare)
!
!
    if (base .eq. 'G') then
        call wkvect(nomare//'.MAOR', 'G V K8', 1, jmaor)
        zk8(jmaor)=noma
    endif
!     -- SI L'ON SOUHAITE RECUPERER LES TABLEAUX DE CORRESPONDANCE :
    if (corrn .ne. ' ') then
        call juveca('&&RDTMAI_WORK_2', nbnoou)
        call jedupo('&&RDTMAI_WORK_2', bascor, corrn, .false._1)
    endif
    if (corrm .ne. ' ') then
        call wkvect(corrm, bascor//' V I', nbmaou, jcorrm)
        do imain = 1, nbmain
            imaou=zi(jwk3-1+imain)
            if (imaou .ne. 0) then
                zi(jcorrm-1+imaou)=imain
            endif
        end do
    endif
!
    call jedetr('&&RDTMAI_WORK_1')
    call jedetr('&&RDTMAI_WORK_2')
    call jedetr('&&RDTMAI_WORK_3')
!
    call jedetr('&&RDTMAI_GRMA_FOURNIS')
    call jedetr('&&RDTMAI_GRNO_FOURNIS')
    call jedetr('&&RDTMAI_GRMA_NON_VIDES')
    call jedetr('&&RDTMAI_GRNO_NON_VIDES')
    call jedetr('&&RDTMAI_NB_MA_PAR_GRMA')
    call jedetr('&&RDTMAI_NB_NO_PAR_GRNO')
    AS_DEALLOCATE(vk24=grp_noeu_in)
    call jedetr('&&RDTMAI.NUM_MAIL_IN')
    call jedetr('&&RDTMAI.NUM_NOEU_IN')
!
    call jedema()
!
end subroutine
