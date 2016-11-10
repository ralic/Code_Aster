subroutine xlagsp(mesh        , model , crack, algo_lagr, nb_dim,&
                  sdline_crack, l_pilo, tabai, l_ainter)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/r8maem.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/conare.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
#include "asterfort/xlag2c.h"
#include "asterfort/xlagsc.h"
#include "asterfort/xxmmvd.h"
#include "asterfort/xelfis_lists.h"
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
    integer, intent(in) :: nb_dim
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    character(len=8), intent(in)  :: crack
    integer, intent(in) :: algo_lagr
    character(len=14), intent(in) :: sdline_crack
    aster_logical, intent(in) :: l_pilo, l_ainter
    character(len=19) :: tabai
!
! --------------------------------------------------------------------------------------------------
!
! XFEM - Contact definition
!
! Lagrange multiplier space selection for contact
!
! --------------------------------------------------------------------------------------------------
!
! (VOIR BOOK VI 15/07/05) :
!    - DETERMINATION DES NOEUDS
!    - CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
! In  model          : name of model
! In  mesh           : name of mesh
! In  crack          : name of crack 
! In  algo_lagr      : type of Lagrange multiplier space selection
! In  nb_dim         : dimension of space
! In  sdline_crack   : name of datastructure of linear relations for crack
! In  l_pilo         : .true. if creation of linear relations for continuation method (PILOTAGE)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbcmp
    parameter    (nbcmp = 14)
!
    integer :: nb_node_mesh, nbar, nbarto, itypma
    integer :: ar(12, 3), na, nb, nunoa, nb_edge_max
    integer :: nunob, nunom, nunoaa, nunobb
    integer :: ia, iia, ia1, i, k, iret, ima
    integer :: jconx2, jmail
    integer :: npil
    real(kind=8) :: c(3), cc(3)
    character(len=8) :: typma
    integer :: ifm, niv
    character(len=19) :: tabno, tabint, tabcri
    integer :: jtabno, jtabin, jtabcr
    integer :: zxbas, zxain
    real(kind=8) :: lon, dist1, dist2
    aster_logical :: lmulti
    character(len=19) :: chsoe, chslo, chsba, chsai
    integer :: jcesl2, jcesl3, jcesl4, jcesl5
    integer :: jcesd2, jcesd3, jcesd4, jcesd5
    integer :: jcesv5
    integer :: iad2, iad3, iad4, ninter, pint, ifiss
    character(len=24) :: grp(3), gr, elfis_heav, elfis_ctip, elfis_hect
    character(len=6) :: nompro
    parameter (nompro = 'XLAGSP')
    integer :: nmaenr, ienr, jgrp, jxc, ier, jnbpt
    integer :: noeco(2),nuno1,nuno2,jlis,naren
    character(len=8) ::  kbid
    aster_logical :: relpre, enleve
    integer :: nuno_1, nuno_2, ia2, decalage
    integer, pointer :: cesv2(:) => null()
    real(kind=8), pointer :: cesv3(:) => null()
    real(kind=8), pointer :: cesv4(:) => null()
    integer, pointer :: typmail(:) => null()
    integer, pointer :: connex(:) => null()
    aster_logical, pointer :: tab_enl(:) => null()
!
!   tolerances --- absolue et relative --- pour determiner si deux valeurs du critere sont egales
    real(kind=8), parameter :: atol=1.e-12
    real(kind=8), parameter :: rtol=1.e-12
    aster_logical :: near
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
! --- INITIALISATIONS
!
    chsoe = '&&XLAGSP.CHSOE'
    chslo = '&&XLAGSP.CHSLO'
    chsba = '&&XLAGSP.CHSBA'
    chsai = '&&XLAGSP.CHSAI'
    ASSERT(nb_dim.le.3)
!
! --- ON TRANSFORME LE CHAMP TOPOFAC.LO EN CHAMP SIMPLE
!
    call celces(model//'.TOPOFAC.LO', 'V', chslo)
    call jeveuo(chslo//'.CESD', 'L', jcesd2)
    call jeveuo(chslo//'.CESV', 'L', vi=cesv2)
    call jeveuo(chslo//'.CESL', 'L', jcesl2)
!
! --- ON TRANSFORME LE CHAMP TOPOFAC.AI EN CHAMP SIMPLE
!
    zxain = xxmmvd('ZXAIN')
    call celces(model//'.TOPOFAC.AI', 'V', chsai)
    call jeveuo(chsai//'.CESD', 'L', jcesd3)
    call jeveuo(chsai//'.CESV', 'L', vr=cesv3)
    call jeveuo(chsai//'.CESL', 'L', jcesl3)
!
! --- ON TRANSFORME LE CHAMP TOPOFAC.OE EN CHAMP SIMPLE
!
    call celces(model//'.TOPOFAC.OE', 'V', chsoe)
    call jeveuo(chsoe//'.CESD', 'L', jcesd4)
    call jeveuo(chsoe//'.CESV', 'L', vr=cesv4)
    call jeveuo(chsoe//'.CESL', 'L', jcesl4)
!
! --- ON TRANSFORME LE CHAMP TOPOFAC.BA EN CHAMP SIMPLE
!
    zxbas = xxmmvd('ZXBAS')
    call celces(model//'.TOPOFAC.BA', 'V', chsba)
    call jeveuo(chsba//'.CESD', 'L', jcesd5)
    call jeveuo(chsba//'.CESV', 'L', jcesv5)
    call jeveuo(chsba//'.CESL', 'L', jcesl5)
!
! --- SI SDLINE_CRACK EXISTE (PROPAGATION), ON LE LIT
!
    call jeexin(sdline_crack,ier)
    relpre = .false.
    if(ier.ne.0) then
        call jelira(sdline_crack,'LONUTI',naren,kbid)
        naren = naren/2
        if(naren.ne.0) then
           relpre = .true.
           call jeveuo(sdline_crack,'L',jlis)
        endif
    endif
!
! --- RECUPERATION DE DONNEES RELATIVES AU MAILLAGE
!
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_node_mesh)
    call jeveuo(mesh(1:8)//'.TYPMAIL', 'L', vi=typmail)
    call jeveuo(mesh(1:8)//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(mesh(1:8)//'.CONNEX', 'LONCUM'), 'L', jconx2)
! --- RECUPERATION DES MAILLES DU MODELE
    call jeveuo(model//'.MAILLE', 'L', jmail)
! --- LE MULTI-HEAVISIDE EST-IL ACTIF ?
    call jeexin(model//'.FISSNO    .CELD', ier)
    if (ier .ne. 0) then
        lmulti = .true.
    else
        lmulti = .false.
        ifiss = 1
    endif
! --- RECUPERATION DU COMPTAGE DES FISSURES VUES PAR LES MAILLES
    if (lmulti) call jeveuo('&&XCONTA.NBSP', 'E', jnbpt)
!
! --- DIMENSIONNEMENT DU NOMBRE MAXIMUM D'ARETES COUPEES PAR LA FISSURE
! --- PAR LE NOMBRE DE NOEUDS DU MAILLAGE (AUGMENTER SI NECESSAIRE)
!
    nb_edge_max = nb_node_mesh
!
    nbarto = 0
    ASSERT(nbcmp.eq.zxbas)
    tabno = '&&XLAGSP.TABNO'
    tabint = '&&XLAGSP.TABINT'
    tabcri = '&&XLAGSP.TABCRI'
!
!
! --- CREATION OBJETS DE TRAVAIL
! --- TABNO  : COL 1,2     : NOEUDS EXTREMITE
!            : COL 3       : NOEUD MILIEU
! --- TABINT : COL 1,2(,3) : COORDONNEES DU POINT D'INTERSECTION
!
    call wkvect(tabno, 'V V I', 3*nb_edge_max, jtabno)
    call wkvect(tabint, 'V V R', nb_dim*nb_edge_max, jtabin)
    call wkvect(tabcri, 'V V R', nb_edge_max, jtabcr)
!
! --- CREATION DE LA LISTE DES ARETES COUPEES
!
    elfis_heav='&&'//nompro//'.ELEMFISS.HEAV'
    elfis_ctip='&&'//nompro//'.ELEMFISS.CTIP'
    elfis_hect='&&'//nompro//'.ELEMFISS.HECT'
    call xelfis_lists(crack, model, elfis_heav,&
                      elfis_ctip, elfis_hect)
    grp(1)=elfis_heav
    grp(2)=elfis_ctip
    grp(3)=elfis_hect
!
! --- REPERAGE NUM LOCAL DE FISSURE POUR CHAQUE MAILLE
! --- ENRICHIE
!
    do k = 1, 3
        call jeexin(grp(k), iret)
        if (iret .eq. 0) goto 10
        call jeveuo(grp(k), 'L', jgrp)
        call jelira(grp(k), 'LONMAX', nmaenr)
!
! --- BOUCLE SUR LES MAILLES DU GROUPE
!
        do ienr = 1, nmaenr
            ima = zi(jgrp-1+ienr)
            if (lmulti) then
                zi(jnbpt-1+ima) = zi(jnbpt-1+ima)+1
            endif
        end do
 10     continue
    end do
!
!   menage
!
    call jedetr(grp(1))
    call jedetr(grp(2))
    call jedetr(grp(3))
!
! --- RECUP MAILLES DE CONTACT
!
    gr = crack//'.MAILFISS.CONT'
    call jeexin(gr, iret)
    if (iret .eq. 0) goto 99
    call jeveuo(gr, 'L', jgrp)
    call jelira(gr, 'LONMAX', nmaenr)
!
! --- BOUCLE SUR LES MAILLES DE CONTACT
!
    do ienr = 1, nmaenr
        ima = zi(jgrp-1+ienr)
        if (lmulti) ifiss = zi(jnbpt-1+ima)
        itypma = typmail(ima)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
        call jeveuo(model(1:8)//'.XFEM_CONT', 'L', jxc)
!
! --- RECUPERATION DU NOMBRE DE POINT D'INTERSECTIONS
!
        call cesexi('C', jcesd2, jcesl2, ima, 1,&
                    ifiss, 1, iad2)
        ninter = cesv2(iad2)
! --- NINTER DOIT DÉPENDRE DE LA FISS QUI COUPE SI ELEMENT XH2C,3C OU 4C
!          IF (LMULTI) THEN
!            IF (ENR.EQ.'XH2C'.OR.ENR.EQ.'XH3C'.OR.ENR.EQ.'XH4C') THEN
!              CALL CESEXI('S',JCESD2,JCESL2,IMA,1,1,4,IAD2)
!              IF (ZI(JCESV2-1+IAD2).NE.ZI(JNBPT-1+IMA)) NINTER = 0
!            ENDIF
!          ENDIF
!
! --- RECUPERATION DE LA CONNECTIVITE DES ARETES
!
        call conare(typma, ar, nbar)
!
! --- BOUCLE SUR LES POINTS D'INTERSECTIONS
!
        do pint = 1, ninter
!
! --- NUMERO DE L'ARETE INTERSECTEES
!
            call cesexi('S', jcesd3, jcesl3, ima, 1,&
                        ifiss, zxain*(pint-1)+ 1, iad3)
            ASSERT(iad3.gt.0)
            ia=nint(cesv3(iad3))
! - SI PILOTAGE ET NOEUD INTERSECTE, ON L AJOUTE
            if (getexm('PILOTAGE','DIRE_PILO') .eq. 1) then
                call getvtx('PILOTAGE', 'DIRE_PILO', iocc=1, nbval=0, nbret=npil)
                npil=-npil
                if (npil .ge. 1) then
                    if (ia .eq. 0) then
                        call cesexi('S', jcesd3, jcesl3, ima, 1,&
                                    ifiss, zxain*(pint-1)+2, iad3)
                        na=nint(cesv3(iad3))
                        nb=na
                    else
                        na = ar(ia,1)
                        nb = ar(ia,2)
                    endif
                endif
! --- SI CE N'EST PAS UNE ARETE COUPEE, ON SORT
            else
                if (ia .eq. 0) goto 110
                na = ar(ia,1)
                nb = ar(ia,2)
            endif
!
! --- RECUPERATION DES NOEUDS
!
            nunoa = connex(zi(jconx2+ima-1)+na-1)
            nunob = connex(zi(jconx2+ima-1)+nb-1)
            nunom=0
!
! --- EST-CE QUE L'ARETE EST DEJA VUE ?
!
            do i = 1, nbarto
!             ARETE DEJA VUE
                if (nunoa .eq. zi(jtabno-1+3*(i-1)+1) .and. nunob .eq. zi( jtabno-1+3*(i-1)+2)) &
                goto 110
                if (nunoa .eq. zi(jtabno-1+3*(i-1)+2) .and. nunob .eq. zi( jtabno-1+3*(i-1)+1)) &
                goto 110
            end do
!
! --- NOUVELLE ARETE
!
            nbarto = nbarto+1
            ASSERT(nbarto.lt.nb_edge_max)
            zi(jtabno-1+3*(nbarto-1)+1) = nunoa
            zi(jtabno-1+3*(nbarto-1)+2) = nunob
            zi(jtabno-1+3*(nbarto-1)+3) = nunom
            do i = 1, nb_dim
                call cesexi('S', jcesd4, jcesl4, ima, 1,&
                            ifiss, nb_dim*(pint- 1)+i, iad4)
                ASSERT(iad4.gt.0)
                c(i) = cesv4(iad4)
                zr(jtabin-1+nb_dim*(nbarto-1)+i) = c(i)
            end do
!
110         continue
        end do
!
    end do
 99 continue
!
! --- SI NLISEQ EXISTE, ON ENLEVE LES ARETES HYPERSTATIQUES POUR L'ANCIEN ESPACE
!
    if(relpre) then
!
        AS_ALLOCATE(vl=tab_enl,size=nbarto)
!
        do ia = 1,nbarto
!
            enleve = .false.
            nunoa = zi(jtabno-1+3*(ia-1)+1)
            nunob = zi(jtabno-1+3*(ia-1)+2)
            noeco(1) = 0
            noeco(2) = 0
            do i=1,naren
                nuno1 = zi(jlis-1+2*(i-1)+1)
                nuno2 = zi(jlis-1+2*(i-1)+2)
!
!               SI ARETE VITALE PRECEDENTE, ON LA GARDE
                if(nunoa.eq.nuno1.and.nunob.eq.nuno2) goto 882
                if(nunoa.eq.nuno2.and.nunob.eq.nuno1) goto 882
!
!               ON REGARDE LES NOEUDS REPERTORIES DS NLISEQ PRECEDENT
                if(nunoa.eq.nuno1.or.nunoa.eq.nuno2) then
                    if(noeco(1).eq.0) then
                        noeco(1) = nunoa
                    elseif(nunoa.ne.noeco(1)) then
                        noeco(2) = nunoa
                    endif
                endif
!
                if(nunob.eq.nuno1.or.nunob.eq.nuno2) then
                   if(noeco(1).eq.0) then
                       noeco(1) = nunob
                   elseif(nunob.ne.noeco(1)) then
                       noeco(2) = nunob
                   endif
                endif
            end do
            if(noeco(1).ne.0.and.noeco(2).ne.0) enleve = .true.
!           Ajout
!           On regarde si un des noeuds connecte a une ancienne arete
!           vitale qui n est plus intersectee
!           Si oui, on laisse l arete
            if(enleve) then
              do i=1,naren
                 nuno1 = zi(jlis-1+2*(i-1)+1)
                 nuno2 = zi(jlis-1+2*(i-1)+2)
!
!                Ancienne arete vitale connectee
                 if(noeco(1).eq.nuno1.or.noeco(1).eq.nuno2.or.&
                    noeco(2).eq.nuno1.or.noeco(2).eq.nuno2) then
                    do ia2=1,nbarto
                         nuno_1 = zi(jtabno-1+3*(ia2-1)+1)
                         nuno_2 = zi(jtabno-1+3*(ia2-1)+2)
!
                         if((nuno_1.eq.nuno1.and.nuno_2.eq.nuno2).or.&
                            (nuno_1.eq.nuno2.and.nuno_2.eq.nuno1)) goto 881
!
                     end do
!                    Arete vitale perdue pour le nouvel espace
                     enleve = .false.
                     goto 882
881                  continue
                 endif
              end do
            endif
!
!           Si on doit enlever l arete
882        continue
           tab_enl(ia) = enleve
        end do
!
        decalage = 0
        do ia=1, nbarto
            if(tab_enl(ia)) then
               decalage = decalage+1
            else
               zi(jtabno-1+3*(ia-decalage-1)+1) = zi(jtabno-1+3*(ia-1)+1)
               zi(jtabno-1+3*(ia-decalage-1)+2) = zi(jtabno-1+3*(ia-1)+2)
               zi(jtabno-1+3*(ia-decalage-1)+3) = zi(jtabno-1+3*(ia-1)+3)
               do i=1,nb_dim
               zr(jtabin-1+nb_dim*(ia-decalage-1)+i) = zr(jtabin-1+nb_dim*(ia-1)+i)
               end do
            endif
        end do
!
        do ia =nbarto-decalage+1,nbarto
            zi(jtabno-1+3*(ia-1)+1) = 0
            zi(jtabno-1+3*(ia-1)+2) = 0
            zi(jtabno-1+3*(ia-1)+3) = 0
            do i=1,nb_dim
                zr(jtabin-1+nb_dim*(ia-1)+i) = 0.d0
            end do
        end do
!
        nbarto = nbarto-decalage
!
        AS_DEALLOCATE(vl=tab_enl)
    endif
!
! --- CRITERE POUR DEPARTAGER LES ARETES HYPERSTATIQUES:
!     LONGUEUR DE FISSURE CONTROLÏ¿ŒE, I.E.
!     SOMME DES LONGUEURS DES ARETES DES FACETTES
!     DE CONTACT CONNECTEES A CHAQUE ARETE
!
    do ia = 1, nbarto
        nunoa = zi(jtabno-1+3*(ia-1)+1)
        nunob = zi(jtabno-1+3*(ia-1)+2)
        nunom = zi(jtabno-1+3*(ia-1)+3)
        do i = 1, nb_dim
            c(i)=zr(jtabin-1+nb_dim*(ia-1)+i)
        end do
        dist1=r8maem()
        dist2=r8maem()
        ia1=0
        ia2=0
!
        do iia = 1, nbarto
            nunoaa = zi(jtabno-1+3*(iia-1)+1)
            nunobb = zi(jtabno-1+3*(iia-1)+2)
            if ((nunoa.eq.nunoaa.and.nunob.ne.nunobb) .or.&
                ( nunoa.eq.nunobb.and.nunob.ne.nunoaa)) then
!           NUNOA CONNECTE LES DEUX ARETES
                do i = 1, nb_dim
                    cc(i)=zr(jtabin-1+nb_dim*(iia-1)+i)
                end do
                lon=0.d0
                do i = 1, nb_dim
                    lon = lon+(cc(i)-c(i))*(cc(i)-c(i))
                end do
                lon=sqrt(lon)
!
!               lon est-il egal a dist1 ?
                near = abs(lon-dist1) .le. (atol + dist1*rtol)
                if (lon .lt. dist1 .and. .not. near) then
                    dist1=lon
                    ia1=iia
                endif
            endif
            if ((nunoa.ne.nunoaa.and.nunob.eq.nunobb) .or.&
                ( nunoa.ne.nunobb.and.nunob.eq.nunoaa)) then
!           NUNOB CONNECTE LES DEUX ARETES
                do i = 1, nb_dim
                    cc(i)=zr(jtabin-1+nb_dim*(iia-1)+i)
                end do
                lon=0.d0
                do i = 1, nb_dim
                    lon = lon+(cc(i)-c(i))*(cc(i)-c(i))
                end do
                lon=sqrt(lon)
!               lon est-il egal a dist2 ?
                near = abs(lon-dist2) .le. (atol + dist2*rtol)
                if (lon .lt. dist2 .and. .not. near) then
                    dist2=lon
                    ia2=iia
                endif
            endif
        end do
        lon=0.d0
        if (ia2 .ne. 0) then
            lon=lon+dist2
        endif
        if (ia1 .ne. 0) then
            lon=lon+dist1
        endif
!
        zr(jtabcr-1+1*(ia-1)+1)=lon
!
    end do
!
! --- CREATION DES LISTES DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
    call xlagsc(nb_dim, nb_node_mesh, nbarto, nb_edge_max, algo_lagr,&
                jtabno, jtabin      , jtabcr, crack      , sdline_crack,&
                l_pilo, tabai, l_ainter)
!
! --- SI LE MULTI-HEAVISIDE EST ACTIF, ON CREE UNE SD SUPPLEMENTAIRE
! --- CONTENANT LE NUMÉROS DE LAGRANGIEN CORESPONDANT.
!
    if (lmulti) call xlag2c(model, sdline_crack, jnbpt, mesh)
!
! --- DESTRUCTION DES OBJETS TEMPORAIRES
!
    call jedetr(tabno)
    call jedetr(tabint)
    call jedetr(tabcri)
    call detrsd('CHAM_ELEM_S', chsoe)
    call detrsd('CHAM_ELEM_S', chslo)
    call detrsd('CHAM_ELEM_S', chsai)
!
! --- AFFICHAGE LISTE REL. LINEAIRES
!
    if (niv .ge. 2) then
        write(ifm,*) '<XFEM  > LISTE DES RELATIONS LINEAIRES'
        call utimsd(ifm, -1, .true._1, .true._1, sdline_crack,&
                    1, ' ', perm='OUI')
    endif
!
    call jedema()
end subroutine
