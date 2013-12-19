subroutine op0018()
    implicit none
! person_in_charge: jacques.pellet at edf.fr
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!                   AFFE_MODELE
!
!     ------------------------------------------------------------------
!        REMARQUES ET RESTRICTIONS D UTILISATION
!
!       LES SEULES VERIFICATIONS FAITES ( FAUX=EXIT ), PORTENT SUR:
!       - L AFFECTATION D ELEMENTS FINIS A TOUTES LES MAILLES DEMANDEES
!       - L AFFECTATION D ELEMENTS FINIS A TOUS LES NOEUDS DEMANDES
!       - L AFFECTATION D ELEMENTS FINIS SUR UNE MAILLE AU MOINS
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/adalig.h"
#include "asterfort/ajlipa.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/cetucr.h"
#include "asterfort/codent.h"
#include "asterfort/cormgi.h"
#include "asterfort/crevge.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/initel.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/modexi.h"
#include "asterfort/ssafmo.h"
#include "asterfort/taxis.h"
#include "asterfort/utmess.h"
#include "asterfort/w18imp.h"
#include "asterfort/wkvect.h"
!
!
    integer :: vali(4), d1, d2
    character(len=4) :: kioc, cdim
    character(len=8) :: noma, nomu, k8b, verif(2), exivf, bevois
    character(len=8) :: typema
    character(len=16) :: k16bid
    character(len=16) :: concep, cmd, phenom, modeli, lmodel(10)
    character(len=19) :: ligrel
    character(len=24) :: nommai, nomnoe, typmai, grpnoe, grpmai, tmpdef
    character(len=24) :: cptnem, cptnbn, cptlie, cptmai, cptnoe, tmpde2
    character(len=32) :: phemod
    logical :: lmail, lnoeu, laxis
    integer :: i, i2d, i3d, ibid, ico, idim, idim2, ifm, ii, imodel, imodl
    integer :: ioc, j, jdef, jdgm, jdgn, jdli, jdma, jdma2, jdnb, jdef2
    integer :: jlgrf, jdno, jdnw, jdpm, jdtm, jmut, jmut2, jnut
    integer :: lonlie, lonnem, nbgrel, nbgrma, nbgrno, nbmaaf, nbmail
    integer :: nbmpaf, nbmpcf, nbnoaf, nbnoeu, nbnpaf, nbnpcf, nboc
    integer :: nboc2, nbv, ndgm, ndgn, ndma, ndmax, ndmax1, ndmax2, ndno
    integer :: ngm, ngn, niv, nma, nmgrel, nmo, nno, nph, nto, ntypoi, nugrel
    integer :: numail, numnoe, numsup, numvec, nutype, nutypm, idim3
    integer :: iarg
    integer, pointer :: tmdim(:) => null()
!     ------------------------------------------------------------------
!
!
    call jemarq()
!
    lmail=.false.
    lnoeu=.false.
    laxis=.false.
!
!     RECUPERATION DU NIVEAU D'IMPRESSION
!     -----------------------------------
    call infmaj()
    call infniv(ifm, niv)
!
! ---   INITIALISATION DU NB D'ERREUR
!
!
! ---   RECUPERATION DES ARGUMENTS  DE LA COMMANDE
!
    call getres(nomu, concep, cmd)
    ligrel=nomu//'.MODELE'
!
! - MAILLAGE
!
    call getvid(' ', 'MAILLAGE', scal=noma, nbret=nbv)
    if (nbv .eq. 0) then
        call getvid(' ', 'GRILLE', scal=noma, nbret=nbv)
    endif
!
! - VERIF
!
    call getvtx(' ', 'VERIF', nbval=2, vect=verif, nbret=nbv)
!
! - GRANDEURS CARACTERISTIQUES
!
    k16bid='GRANDEUR_CARA'
    call getfac(k16bid, nboc)
    if (nboc .gt. 0) then
        call cetucr(k16bid, nomu)
    endif
!
! - AFFE
!
    ndgm=0
    ndgn=0
    ndma=0
    ndno=0
!
    call getfac('AFFE', nboc)
    call getfac('AFFE_SOUS_STRUC', nboc2)
!
    do ioc = 1, nboc
        call codent(ioc, 'G', kioc)
!
        call getvtx('AFFE', 'TOUT', iocc=ioc, nbval=0, nbret=nto)
        call getvem(noma, 'GROUP_MA', 'AFFE', 'GROUP_MA', ioc,&
                    iarg, 0, k8b, ngm)
        call getvem(noma, 'GROUP_NO', 'AFFE', 'GROUP_NO', ioc,&
                    iarg, 0, k8b, ngn)
        call getvem(noma, 'MAILLE', 'AFFE', 'MAILLE', ioc,&
                    iarg, 0, k8b, nma)
        call getvem(noma, 'NOEUD', 'AFFE', 'NOEUD', ioc,&
                    iarg, 0, k8b, nno)
!
!
        ndgm=max(ndgm,-ngm)
        ndgn=max(ndgn,-ngn)
        ndma=max(ndma,-nma)
        ndno=max(ndno,-nno)
    end do
!
    ndmax1=max(ndgm,ndgn)
    ndmax2=max(ndma,ndno)
    ndmax=max(ndmax1,ndmax2)
    if (ndmax .le. 0) ndmax=1
!
!
!
!
!       -- ON TRAITE CE QUI EST COMMUN AUX MODELES AVEC ELEMENTS
!                           ET AUX MODELES AVEC SOUS-STRUCTURES
!       ---------------------------------------------------------------
    cptnbn=nomu//'.MODELE    .NBNO'
    call wkvect(nomu//'.MODELE    .LGRF', 'G V K8', 2, jlgrf)
    call wkvect(cptnbn, 'G V I', 1, jdnb)
    zk8(jlgrf-1+1)=noma
    zk8(jlgrf-1+2)=nomu
    zi(jdnb)=0
!
!       -- RECHERCHE DU PHENOMENE :
    if (nboc .gt. 0) then
        call getvtx('AFFE', 'PHENOMENE', iocc=1, scal=phenom, nbret=ibid)
    else if (nboc2.gt.0) then
        call getvtx('AFFE_SOUS_STRUC', 'PHENOMENE', iocc=1, scal=phenom, nbret=ibid)
    endif
    call jeecra(nomu//'.MODELE    .LGRF', 'DOCU', cval=phenom(1:4))
!
!       -- S'IL N'Y A PAS D'ELEMENTS ON SAUTE QUELQUES ETAPES:
    if (nboc .eq. 0) goto 190
!
!       MODELE AVEC ELEMENTS:
! ---   RECUPERATION DES NOMS JEVEUX DU CONCEPT MAILLAGE
!
    nommai=noma//'.NOMMAI'
    nomnoe=noma//'.NOMNOE'
    typmai=noma//'.TYPMAIL'
    grpnoe=noma//'.GROUPENO'
    grpmai=noma//'.GROUPEMA'
!
! ---   CONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
!
    tmpdef=nomu//'.DEF'
    call wkvect(tmpdef, 'V V K24', ndmax, jdef)
    tmpde2=nomu//'.DEF2'
    call wkvect(tmpde2, 'V V K8', ndmax, jdef2)
!
    cptmai=nomu//'.MAILLE'
    cptnoe=nomu//'.NOEUD'
    cptlie=nomu//'.MODELE    .LIEL'
    cptnem=nomu//'.MODELE    .NEMA'
!
!     --  CREATION DES VECTEURS TAMPONS MAILLES ET NOEUDS
    call jelira(nommai, 'NOMMAX', nbmail)
    call jelira(nomnoe, 'NOMMAX', nbnoeu)
    call jeveuo(typmai, 'L', jdtm)
!
    call wkvect(cptmai, 'G V I', nbmail, jdma)
    call wkvect(cptnoe, 'G V I', nbnoeu, jdno)
    call wkvect('&&OP0018.MAILLE', 'V V I', nbmail, jmut)
    call wkvect('&&OP0018.MAILLE2', 'V V I', nbmail, jmut2)
    call wkvect('&&OP0018.MAILLE3', 'V V I', nbmail, jdma2)
    call wkvect('&&OP0018.NOEUD', 'V V I', nbnoeu, jnut)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), ntypoi)
    call jeveuo('&CATA.TM.TMDIM', 'L', vi=tmdim)
!
!
!
    do ioc = 1, nboc
        call getvtx('AFFE', 'PHENOMENE', iocc=ioc, scal=phenom, nbret=nph)
        call getvtx('AFFE', 'MODELISATION', iocc=ioc, nbval=10, vect=lmodel,&
                    nbret=nmo)
        ASSERT(nmo.gt.0)
        d2=-99
        call jerazo(tmpdef, ndmax, 1)
        call jerazo('&&OP0018.MAILLE2', nbmail, 1)
        call jerazo('&&OP0018.MAILLE3', nbmail, 1)
!
!       -- RAPPEL : LES MOTS CLES TOUT,GROUP_MA,... S'EXCLUENT
        call getvtx('AFFE', 'TOUT', iocc=ioc, nbval=0, nbret=nto)
        call getvem(noma, 'GROUP_MA', 'AFFE', 'GROUP_MA', ioc,&
                    iarg, ndmax, zk24(jdef), ngm)
        call getvem(noma, 'MAILLE', 'AFFE', 'MAILLE', ioc,&
                    iarg, ndmax, zk8(jdef2), nma)
        call getvem(noma, 'GROUP_NO', 'AFFE', 'GROUP_NO', ioc,&
                    iarg, ndmax, zk24(jdef), ngn)
        call getvem(noma, 'NOEUD', 'AFFE', 'NOEUD', ioc,&
                    iarg, ndmax, zk8(jdef2), nno)
!
        do imodel = 1, nmo
            modeli=lmodel(imodel)
            call jenonu(jexnom('&CATA.'//phenom(1:13)//'.MODL', modeli), imodl)
            call jeveuo(jexnum('&CATA.'//phenom, imodl), 'L', jdpm)
!
            phemod=phenom//modeli
            call dismoi('DIM_TOPO', phemod, 'PHEN_MODE', repi=d1)
            if (d2 .eq. -99) then
                d2=d1
            else
                if (d2 .ne. d1) then
                    call utmess('F', 'MODELISA5_51')
                endif
            endif
!
            if (modeli(1:4) .eq. 'AXIS' .or. modeli .eq. 'COQUE_AXIS') laxis=.true.
!
            if (nto .ne. 0) then
                lmail=.true.
                do numail = 1, nbmail
                    nutypm=zi(jdtm+numail-1)
                    if (zi(jdpm+nutypm-1) .gt. 0) then
                        zi(jdma+numail-1)=zi(jdpm+nutypm-1)
                        zi(jdma2+numail-1)=zi(jdpm+nutypm-1)
                    endif
                    zi(jmut+numail-1)=1
                    if (tmdim(nutypm) .eq. d2) zi(jmut2+numail-1)= 1
                end do
            endif
!
            if (ngm .ne. 0) then
                lmail=.true.
                do i = 1, ngm
                    call jeveuo(jexnom(grpmai, zk24(jdef+i-1)), 'L', jdgm)
                    call jelira(jexnom(grpmai, zk24(jdef+i-1)), 'LONUTI', nbgrma)
                    do j = 1, nbgrma
                        numail=zi(jdgm+j-1)
                        nutypm=zi(jdtm+numail-1)
                        if (zi(jdpm+nutypm-1) .gt. 0) then
                            zi(jdma+numail-1)=zi(jdpm+nutypm-1)
                            zi(jdma2+numail-1)=zi(jdpm+nutypm-1)
                        endif
                        zi(jmut+numail-1)=1
                        if (tmdim(nutypm) .eq. d2) zi(jmut2+numail- 1)=1
                    end do
                end do
            endif
!
            if (nma .ne. 0) then
                lmail=.true.
                do i = 1, nma
                    call jenonu(jexnom(nommai, zk8(jdef2+i-1)), numail)
                    nutypm=zi(jdtm+numail-1)
                    if (zi(jdpm+nutypm-1) .gt. 0) then
                        zi(jdma+numail-1)=zi(jdpm+nutypm-1)
                        zi(jdma2+numail-1)=zi(jdpm+nutypm-1)
                    endif
                    zi(jmut+numail-1)=1
                    if (tmdim(nutypm) .eq. d2) zi(jmut2+numail-1)= 1
                end do
            endif
!
            if (ngn .ne. 0) then
                lnoeu=.true.
                do i = 1, ngn
                    call jeveuo(jexnom(grpnoe, zk24(jdef+i-1)), 'L', jdgn)
                    call jelira(jexnom(grpnoe, zk24(jdef+i-1)), 'LONUTI', nbgrno)
                    do j = 1, nbgrno
                        numnoe=zi(jdgn+j-1)
                        if (zi(jdpm+ntypoi-1) .gt. 0) zi(jdno+numnoe-1)= zi(jdpm+ ntypoi-1)
                        zi(jnut+numnoe-1)=1
                    end do
                end do
            endif
!
            if (nno .ne. 0) then
                lnoeu=.true.
                do i = 1, nno
                    call jenonu(jexnom(nomnoe, zk8(jdef2+i-1)), numnoe)
                    if (zi(jdpm+ntypoi-1) .gt. 0) zi(jdno+numnoe-1)=zi( jdpm+ ntypoi-1)
                    zi(jnut+numnoe-1)=1
                end do
            endif
!
        end do
!
!       -- ON VERIFIE QU'A CHAQUE OCCURENCE DE AFFE, LES MAILLES
!          "PRINCIPALES" ONT BIEN ETE AFFECTEES PAR DES ELEMENTS
!          (PB DES MODELISATIONS A "TROUS") :
!          ------------------------------------------------------
        ico=0
        do numail = 1, nbmail
            if ((zi(jmut2+numail-1).eq.1) .and. zi(jdma2+numail-1) .eq. 0) ico=ico+1
        end do
        if (ico .gt. 0) then
            vali(1)=ioc
            vali(2)=ico
            vali(3)=d2
            call utmess('A', 'MODELISA8_70', ni=3, vali=vali)
        endif
    end do
!
!
! --- VERIFICATION QUE LES MAILLES "UTILISATEUR" ONT ETE AFFECTEES
    nbmpcf=0
    nbmpaf=0
    do i = 1, nbmail
        if (zi(jmut+i-1) .eq. 1) then
            if (zi(jdma+i-1) .eq. 0) then
                nbmpaf=nbmpaf+1
                call jenuno(jexnum(nommai, i), k8b)
                nutypm=zi(jdtm+i-1)
                call jenuno(jexnum('&CATA.TM.NOMTM', nutypm), typema)
                if (niv .eq. 2) then
                    write (ifm,*)'  MAILLE QUE L''ON N''A PAS PU AFFEC',&
     &          'TER: ',k8b,' DE TYPE: ',typema
                endif
            endif
        else
            nbmpcf=nbmpcf+1
        endif
    end do
!
! --- VERIFICATION QUE LES NOEUDS "UTILISATEUR" ONT ETE AFFECTES
    nbnpcf=0
    nbnpaf=0
    do i = 1, nbnoeu
        if (zi(jnut+i-1) .eq. 1) then
            if (zi(jdno+i-1) .eq. 0) then
                nbnpaf=nbnpaf+1
                call jenuno(jexnum(nomnoe, i), k8b)
                if (niv .eq. 2) then
                    write (ifm,*)'  NOEUD QUE L''ON N''A PAS PU AFFEC',&
     &          'TER: ',k8b
                endif
            endif
        else
            nbnpcf=nbnpcf+1
        endif
    end do
!
! ---   DIMENSIONNEMENT DES OBJETS LIEL ET NEMA
    nbmaaf=0
    nbnoaf=0
    nutype=0
    nbgrel=0
!
    do i = 1, nbmail
        if (zi(jdma+i-1) .ne. 0) then
            nbmaaf=nbmaaf+1
            if (zi(jdma+i-1) .ne. nutype) then
                nutype=zi(jdma+i-1)
                nbgrel=nbgrel+1
            endif
        endif
    end do
!
!
    if (lmail) then
        ii=nbmaaf+nbmpaf
        write (ifm,9000)nbmail,noma,ii,nbmaaf
    endif
!
    if (nbmaaf .eq. 0) then
        call utmess('F', 'MODELISA5_52', sk=noma)
    endif
!
    nutype=0
!
    do i = 1, nbnoeu
        if (zi(jdno+i-1) .ne. 0) then
            nbnoaf=nbnoaf+1
            if (zi(jdno+i-1) .ne. nutype) then
                nutype=zi(jdno+i-1)
                nbgrel=nbgrel+1
            endif
        endif
    end do
!
!
    if (lnoeu) then
        ii=nbnoaf+nbnpaf
        write (ifm,9010)nbnoeu,noma,ii,nbnoaf
    endif
!
    lonlie=nbgrel+nbmaaf+nbnoaf
    lonnem=nbnoaf*2
!
!
! ---   CREATION DES OBJETS DU CONCEPT MODELE
!
! -     OBJET LIEL
    call jecrec(cptlie, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbgrel)
    call jeecra(cptlie, 'LONT', lonlie)
    call jeveuo(cptlie, 'E', jdli)
!
! -     OBJET NEMA
    if (nbnoaf .ne. 0) then
        call jecrec(cptnem, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    nbnoaf)
        call jeecra(cptnem, 'LONT', lonnem)
        call jeveuo(cptnem, 'E', jdnw)
    endif
!
! ---   STOCKAGE DES GROUPES ELEMENTS DANS LIEL
    nutype=0
    nugrel=0
    nmgrel=0
    numvec=0
!
    do numail = 1, nbmail
        if (zi(jdma+numail-1) .ne. 0) then
            if (zi(jdma+numail-1) .ne. nutype .and. nutype .ne. 0) then
                nugrel=nugrel+1
                nmgrel=nmgrel+1
                numvec=numvec+1
                zi(jdli+numvec-1)=nutype
                call jecroc(jexnum(cptlie, nugrel))
                call jeecra(jexnum(cptlie, nugrel), 'LONMAX', nmgrel)
                nmgrel=0
            endif
            nmgrel=nmgrel+1
            numvec=numvec+1
            zi(jdli+numvec-1)=numail
            nutype=zi(jdma+numail-1)
        endif
        if (numail .eq. nbmail .and. nmgrel .ne. 0) then
            nugrel=nugrel+1
            nmgrel=nmgrel+1
            numvec=numvec+1
            zi(jdli+numvec-1)=nutype
            call jecroc(jexnum(cptlie, nugrel))
            call jeecra(jexnum(cptlie, nugrel), 'LONMAX', nmgrel)
        endif
    end do
!
    nutype=0
    numsup=0
    nmgrel=0
!
    do numnoe = 1, nbnoeu
        if (zi(jdno+numnoe-1) .ne. 0) then
            if (zi(jdno+numnoe-1) .ne. nutype .and. nutype .ne. 0) then
                nugrel=nugrel+1
                nmgrel=nmgrel+1
                numvec=numvec+1
                zi(jdli+numvec-1)=nutype
                call jecroc(jexnum(cptlie, nugrel))
                call jeecra(jexnum(cptlie, nugrel), 'LONMAX', nmgrel)
                nmgrel=0
            endif
            nmgrel=nmgrel+1
            numvec=numvec+1
            numsup=numsup+1
            zi(jdli+numvec-1)=-numsup
            nutype=zi(jdno+numnoe-1)
        endif
        if (numnoe .eq. nbnoeu .and. nmgrel .ne. 0) then
            nugrel=nugrel+1
            nmgrel=nmgrel+1
            numvec=numvec+1
            zi(jdli+numvec-1)=nutype
            call jecroc(jexnum(cptlie, nugrel))
            call jeecra(jexnum(cptlie, nugrel), 'LONMAX', nmgrel)
        endif
    end do
!
! ---   STOCKAGE DES NOUVELLES MAILLES DANS NEMA
!
    if (nbnoaf .ne. 0) then
        numvec=0
        numsup=0
        do numnoe = 1, nbnoeu
            if (zi(jdno+numnoe-1) .ne. 0) then
                zi(jdnw+numvec)=numnoe
                zi(jdnw+numvec+1)=ntypoi
                numvec=numvec+2
                numsup=numsup+1
                call jecroc(jexnum(cptnem, numsup))
                call jeecra(jexnum(cptnem, numsup), 'LONMAX', 2)
            endif
        end do
    endif
!
190 continue
!
! --- PRISE EN COMPTE DES SOUS-STRUCTURES (MOT CLEF AFFE_SOUS_STRUC):
    call ssafmo(nomu)
!
!
!       ---   ADAPTATION DE LA TAILLE DES GRELS
!       ----------------------------------------
    call adalig(ligrel)
!
!     --- CREATION DE LA CORRESPONDANCE MAILLE --> (IGREL,IM)
!     -------------------------------------------------------
    call cormgi('G', ligrel)
!
!     ---   INITIALISATION DES ELEMENTS POUR CE LIGREL
!     -------------------------------------------------
    call initel(ligrel)
!
!     ---   IMPRESSION DES ELEMENTS FINIS AFFECTES :
!     -------------------------------------------------
    call w18imp(ligrel, noma, nomu)
!
!
!     --- VERIFICATION DE LA DIMENSION DES TYPE_ELEM DU MODELE
!     ----------------------------------------------------------
    call dismoi('DIM_GEOM', nomu, 'MODELE', repi=idim)
    if (idim .gt. 3) then
        idim2=0
        call utmess('A', 'MODELISA4_4')
    else
        idim2=3
        idim3=3
        call dismoi('Z_CST', noma, 'MAILLAGE', repk=cdim)
        if (cdim .eq. 'OUI') then
            idim2=2
            call dismoi('Z_ZERO', noma, 'MAILLAGE', repk=cdim)
            if (cdim .eq. 'OUI') idim3=2
        endif
!
        if ((idim.eq.3) .and. (idim2.eq.2)) then
!         -- LES ELEMENTS DE COQUE PEUVENT EXISTER DAS LE PLAN Z=CSTE :
        else if ((idim.eq.2) .and. (idim2.eq.3)) then
!         -- DANGER : MODELE 2D SUR UN MAILLAGE COOR_3D
            call utmess('A', 'MODELISA5_53')
            elseif ((idim.eq.2) .and. (idim2.eq.2).and. (idim3.eq.3))&
        then
!         -- BIZARRE : MODELE 2D SUR UN MAILLAGE Z=CSTE /= 0.
            call utmess('A', 'MODELISA5_58')
        endif
    endif
!
!
!     --- VERIFICATION DU FAIT QUE POUR UN MAILLAGE 2D ON NE PEUT
!     ---- AVOIR A LA FOIS DES ELEMENTS DISCRETS 2D ET 3D :
!     ---------------------------------------------------
    call modexi(nomu, 'DIS_', i3d)
    call modexi(nomu, '2D_DIS_', i2d)
    if (idim2 .eq. 2 .and. i3d .eq. 1 .and. i2d .eq. 1) then
        call utmess('F', 'MODELISA5_54')
    endif
!
!
!     ---   VERIFICATION DES X > 0 POUR L'AXIS
!     -------------------------------------------------
    if (laxis) then
        call taxis(noma, zi(jdma), nbmail)
    endif
!
!
!     -- AJOUT EVENTUEL DE LA SD_PARTITION  :
!     ---------------------------------------------------
    call ajlipa(nomu, 'G')
!
!
!     -- POUR LES VOLUMES FINIS, CREATION DU VOISINAGE :
!     ---------------------------------------------------
    call dismoi('EXI_VF', ligrel, 'LIGREL', repk=exivf)
!
!     -- SCHEMAS NON VF AYANT BESOIN D'UN VOISINAGE :
!     ---------------------------------------------------
    call dismoi('BESOIN_VOISIN', ligrel, 'LIGREL', repk=bevois)
!
!
!     -- CREATION DE LA SD_VOISINAGE SI NECESSAIRE :
!     ---------------------------------------------------
    if ((bevois.eq.'OUI') .or. (exivf.eq.'OUI')) call crevge(ligrel, 'G')
!
!
!     -- ON VERIFIE QUE LA GEOMETRIE DES MAILLES
!        N'EST PAS TROP CHAHUTEE :
!     ---------------------------------------------------
    call getvtx(' ', 'VERI_JACOBIEN', scal=verif(1), nbret=nbv)
    if (verif(1) .eq. 'OUI') call calcul('C', 'VERI_JACOBIEN', ligrel, 1, noma//'.COORDO',&
                                         'PGEOMER', 1, '&&OP0018.CODRET', 'PCODRET', 'V',&
                                         'OUI')
!
!
!
    call jedema()
!
    9000 format (/,' SUR LES ',i12,' MAILLES DU MAILLAGE ',a8,/,'    ON A',&
     &       ' DEMANDE L''AFFECTATION DE ',i12,/,'    ON A PU EN AFFEC',&
     &       'TER           ',i12)
    9010 format (/,' SUR LES ',i12,' NOEUDS  DU MAILLAGE ',a8,/,'    ON A',&
     &       ' DEMANDE L''AFFECTATION DE ',i12,/,'    ON A PU EN AFFEC',&
     &       'TER           ',i12)
end subroutine
