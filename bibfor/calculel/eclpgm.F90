subroutine eclpgm(ma2, mo, cham1, ligrel, shrink,&
                  lonmin, nch, lisch)
    implicit none
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
!   - TRAITEMENT DU MOT CLE CREA_MAILLAGE/ECLA_PG
!-----------------------------------------------------------------------
! BUT : CREER LE MAILLAGE (MA2) CORRESPONDANT AUX POINTS DE GAUSS
!       DES ELEMENTS DU LIGREL (LIGREL).
!
! ARGUMENTS :
!  IN/JXIN  MO : MODELE DONT ON VEUT "ECLATER" LES POINTS DE GAUSS
!  IN/JXIN  LIGREL : NOM DU LIGREL (D'ELEMENTS DU MODELE) CORRESPONDANT
!           AUX MAILLES QUI INTERESSENT L'UTILISATEUR.
!           SI LIGREL=' ', ON PREND LE LIGREL DU MODELE (TOUT='OUI')
!  IN/JXIN  CHAM1 : NOM DU CHAM_ELEM/ELGA QUE L'ON VEUT ECLATER
!           (OU ' ' SI ON UTILISE NCH ET LISCH)
!  IN       LISCH(*) : LISTE DES NOMS SYMBOLIQUES DES CHAMPS AUXQUELS
!           ON S'INTERESSE. EX: (SIEF_ELGA, VARI_ELGA, ...)
!  IN NCH : DIMENSION DE LISCH  (0 SI CHAM1/=' ')
!  IN/JXOUT MA2 : NOM DU MAILLAGE A CREER (1 MAILLE PAR POINT DE GAUSS)
!           LES MAILLES DE MA2 SONT TOUTES DECONNECTEES LES UNES DES
!           DES AUTRES.
!  IN  SHRINK : COEFFICENT DE "CONTRACTION" DES MAILLES POUR QUE
!       CELLES-CI NE SE CHEVAUCHENT PAS.
!       SHRINK=0.9 DONNE UN BEAU VITRAIL AVEC 10% DE NOIR.
!  IN  LONMIN : LONGUEUR MINIMALE POUR LES ARETES DES MAILLES CREEES.
!      (IGNORE SI LONMIN <=0)
!      CETTE FONCTIONNALITE PERMET D'"EPAISSIR" DES ELEMENTS DE JOINT
!      POUR QU'ON PUISSE LES VOIR.
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/celfpg.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/eclapp.h"
#include "asterfort/eclaty.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/typele.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!
!
    integer :: k, te, tabno(27), iret1, jobj, numa, nch
    integer :: igr, iel,   ilmaco,  illiel
    integer :: dimgeo,  ibid, ino, ino1, ino2
    integer :: nbmail, nbnoeu, nbcoor, iadime, kse
    integer :: nbno2t, ianno2, iatypm, nuno2, nupoi2, cas
    integer :: npg1, nbpi,  iagese, nno2, nuse, nse1
    integer :: ima, nbelgr, nupoin, npoini, iterm, ipoini
    integer :: iret, ich,  nch2
    character(len=8) :: mo, ma1, ma2, nom, elrefa, fapg, nompar
    character(len=8) :: tych
    character(len=16) :: nomte, lisch(nch)
    character(len=19) :: ligrel, ligrmo, cel, cham1, ligre1
    character(len=24) :: nomobj
    character(len=24) :: valk(2)
    real(kind=8) :: x, xc, xm, shrink, lonmin
!
! ----------------------------------------------------------------------
!     VARIABLES NECESSAIRES A L'APPEL DE ECLATY :
!     ON COMPREND LE SENS DE CES VARIABLES EN REGARDANT ECLATY
    integer :: mxnbn2, mxnbpi, mxnbte, mxnbse
!     MXNBN2 : MAX DU NOMBRE DE NOEUDS D'UN SOUS-ELEMENT (HEXA8)
    parameter (mxnbn2=8)
!     MXNBPI : MAX DU NOMBRE DE POINT_I (HEXA A 27 POINTS DE GAUSS)
!     MXNBPI = 4X4X4
    parameter (mxnbpi=64)
!     MXNBTE : MAX DU NOMBRE DE TERMES DE LA C.L. DEFINISSANT 1 POINT_I
!              AU PLUS LES 8 SOMMETS D'UN HEXA8
    parameter (mxnbte=8)
!     MXNBSE : MAX DU NOMBRE DE SOUS-ELEMENTS
    parameter (mxnbse=27)
!
    integer :: nbse, corsel(mxnbse)
    integer :: connx(mxnbn2, mxnbse), nsomm1(mxnbpi, mxnbte)
    integer :: nterm1(mxnbpi), nbno2(mxnbse), tyma(mxnbse)
    real(kind=8) :: csomm1(mxnbpi, mxnbte)
    integer :: ico
    integer :: opt, iadesc, iaoppa, nbin
    integer, pointer :: connexse(:) => null()
    real(kind=8), pointer :: geopoini(:) => null()
    integer, pointer :: liel(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    character(len=24), pointer :: refe(:) => null()
    integer, pointer :: maco(:) => null()
! ----------------------------------------------------------------------
!
!     FONCTIONS FORMULES :
!     NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
#define numail(igr,iel) liel(zi(illiel+igr-1)+iel-1)
!     NBNOMA(IMA)=NOMBRE DE NOEUDS DE LA MAILLE IMA
#define nbnoma(ima) zi(ilmaco-1+ima+1)-zi(ilmaco-1+ima)
!     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
!                     IMA ETANT UNE MAILLE DU MAILLAGE.
#define numglm(ima,ino) maco(zi(ilmaco+ima-1)+ino-1)
!
! DEB ------------------------------------------------------------------
    call jemarq()
    dimgeo = 3
!
!
    call dismoi('NOM_MAILLA', mo, 'MODELE', repk=ma1)
    call jeveuo(ma1//'.COORDO    .VALE', 'L', vr=vale)
    call jeveuo(ma1//'.CONNEX', 'L', vi=maco)
    call jeveuo(jexatr(ma1//'.CONNEX', 'LONCUM'), 'L', ilmaco)
!
!
!     -- ON ACCEPTE 2 CAS DE FIGURE :
    if (cham1 .ne. ' ') then
!       -- CAS "PROJ_CHAMP" :
        call dismoi('TYPE_CHAMP', cham1, 'CHAMP', repk=tych)
        ASSERT(tych.eq.'ELGA')
        ASSERT(nch.eq.0)
        ASSERT(ligrel.ne.' ')
        call dismoi('NOM_LIGREL', cham1, 'CHAM_ELEM', repk=ligre1)
        ASSERT(ligre1.eq.ligrel)
        cas=1
    else
!       -- CAS "CREA_MAILLAGE/ECLA_PG" :
        cas=2
    endif
!
!
!
!    0.1 : ON CHERCHE LE NOM DES FAMILLES DE POINTS DE GAUSS A ECLATER
!          SORTIES : NCH2 [+ JOBJ SI NCH2>0]
!    -----------------------------------------------------------------
    nomobj = '&&ECLPGM.NOMOBJ'
!
    if (cas .eq. 1) then
        nch2=1
        call celfpg(cham1, nomobj, iret)
        ASSERT(iret.eq.0)
        call jeveuo(nomobj, 'L', jobj)
!
    else if (cas.eq.2) then
        cel = '&&ECLPGM.CHAM_ELEM'
        ico=0
        nch2=nch
        call dismoi('NOM_LIGREL', mo, 'MODELE', repk=ligrmo)
        do ich = 1, nch2
            if (lisch(ich)(6:9) .ne. 'ELGA') then
                call utmess('F', 'CALCULEL2_41')
            endif
            call jenonu(jexnom('&CATA.OP.NOMOPT', lisch(ich)), opt)
            if (opt .eq. 0) goto 30
!
            call jeveuo(jexnum('&CATA.OP.DESCOPT', opt), 'L', iadesc)
            call jeveuo(jexnum('&CATA.OP.OPTPARA', opt), 'L', iaoppa)
            nbin = zi(iadesc-1+2)
            nompar = zk8(iaoppa-1+nbin+1)
!
            call alchml(ligrmo, lisch(ich), nompar, 'V', cel,&
                        iret1, ' ')
            if (iret1 .ne. 0) goto 30
!
            ico=ico+1
            call celfpg(cel, nomobj, iret)
            call detrsd('CHAMP', cel)
            if (iret .eq. 1) then
                valk(1) = mo
                valk(2) = lisch(ich)
                call utmess('F', 'CALCULEL2_33', nk=2, valk=valk)
            endif
            call jeveuo(nomobj, 'L', jobj)
 30         continue
        end do
!        -- ON N'A PAS TROUVE DE CHAMP ELGA CORRECT :
        if (ico .eq. 0) then
           valk(1) = lisch(1)
           call utmess('F','CALCULEL2_24',nk=1, valk=valk)
        endif
    endif
!
!
!
!     0.2 : ON SE RESTREINT AUX MAILLES EVENTUELLEMENT SPECIFIEES PAR
!           L'UTILISATEUR :
!     ----------------------------------------------------------------
    if (ligrel .eq. ' ') ligrel=ligrmo
    call jeveuo(ligrel//'.LIEL', 'L', vi=liel)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', illiel)
!
!
!
!     1. ON COMPTE LES FUTURS SOUS-ELEMENTS
!        ET LES POINT_I ET LES NOEUDS DU FUTUR MAILLAGE
!     ---------------------------------------------------------------
    nbse = 0
    nbpi = 0
    nbno2t = 0
    do igr = 1, nbgrel(ligrel)
        te = typele(ligrel,igr)
        nbelgr = nbelem(ligrel,igr)
        if (nbelgr .eq. 0) goto 1
        call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
!
        ASSERT(nch2.gt.0)
        numa = numail(igr,1)
        elrefa = zk16(jobj-1+numa)(1:8)
        fapg = zk16(jobj-1+numa)(9:16)
        if (fapg .eq. ' ') goto 1
!
        call eclaty(nomte, elrefa, fapg, npg1, npoini,&
                    nterm1, nsomm1, csomm1, tyma, nbno2,&
                    connx, mxnbn2, mxnbpi, mxnbte, mxnbse,&
                    nse1, corsel)
        nbse = nbse+nbelgr*nse1
        nbpi = nbpi+nbelgr*npoini
        do kse = 1, nse1
            nbno2t = nbno2t+nbelgr*nbno2(kse)
        end do
  1     continue
    end do
!
!
!     2. ON ALLOUE 4 OBJETS DE TRAVAIL (+ MAILLAGE//'.TYPMAIL') :
!        .GEOPOINI : GEOMETRIE DES POINT_I
!        .CONNEXSE : NUMEROS DES POINT_I DES SOUS-ELEMENTS
!        .GEOSE    : GEOMETRIE DES SOUS-ELEMENTS
!        .NBNO2    : NOMBRE DE NOEUDS DES SOUS-ELEMENTS
!        .TYPMAIL  : TYPE_MAILLE DES SOUS-ELEMENTS
!     ---------------------------------------------------------------
    if (nbpi .eq. 0) then
        call utmess('F', 'CALCULEL2_35')
    endif
    AS_ALLOCATE(vr=geopoini, size=nbpi*dimgeo)
    AS_ALLOCATE(vi=connexse, size=nbno2t)
    call wkvect('&&ECLPGM.GEOSE', 'V V R', nbno2t*dimgeo, iagese)
    call wkvect('&&ECLPGM.NBNO2', 'V V I', nbse, ianno2)
!
    if (nbse .eq. 0) then
        call utmess('F', 'CALCULEL2_36')
    endif
    call wkvect(ma2//'.TYPMAIL', 'G V I', nbse, iatypm)
!
!
!
!     3. ON CALCULE DES COORDONNEES DES SOUS-ELEMENTS
!        ET LEUR CONNECTIVITE
!     ---------------------------------------------------------------
!
    nuse = 0
    nupoin = 0
    nuno2 = 0
    do igr = 1, nbgrel(ligrel)
        te = typele(ligrel,igr)
        nbelgr = nbelem(ligrel,igr)
        if (nbelgr .eq. 0) goto 2
        call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
!
        ASSERT(nch2.gt.0)
        numa = numail(igr,1)
        elrefa = zk16(jobj-1+numa)(1:8)
        fapg = zk16(jobj-1+numa)(9:16)
        if (fapg .eq. ' ') goto 2
!
        call eclaty(nomte, elrefa, fapg, npg1, npoini,&
                    nterm1, nsomm1, csomm1, tyma, nbno2,&
                    connx, mxnbn2, mxnbpi, mxnbte, mxnbse,&
                    nse1, corsel)
        if (nse1 .eq. 0) goto 2
!
        do iel = 1, nbelgr
!          ON RECUPERE LE NUMERO DE LA MAILLE ET LE NUMERO
!          DE SES SOMMETS :
            ima = numail(igr,iel)
            do ino1 = 1, nbnoma(ima)
                tabno(ino1)=numglm(ima,ino1)
            end do
!
!          -- CALCUL DES COORDONNEES DES POINT_I :
            do ipoini = 1, npoini
                nupoin=nupoin+1
                do k = 1, dimgeo
                    x=0.d0
                    do iterm = 1, nterm1(ipoini)
                        x=x+csomm1(ipoini,iterm)* vale(3*(&
                        tabno(nsomm1(ipoini,iterm))-1)+k)
                    end do
                    geopoini((nupoin-1)*dimgeo+k)=x
                end do
            end do
!
!          -- STOCKAGE DES NUMEROS DES POINT_I DES SOUS-ELEMENTS
!             ET DE LEURS COORDONNEES :
            do kse = 1, nse1
                nuse=nuse+1
                zi(iatypm-1+nuse)=tyma(kse)
                zi(ianno2-1+nuse)=nbno2(kse)
                nno2=nbno2(kse)
                do ino2 = 1, nno2
                    nuno2=nuno2+1
                    connexse(nuno2)=connx(ino2,kse)+ nupoin-npoini
                    nupoi2=connexse(nuno2)
                    do k = 1, dimgeo
                        zr(iagese-1+(nuno2-1)*dimgeo+k)= geopoini((&
                        nupoi2-1)*dimgeo+k)
                    end do
                end do
!           DANS LE CAS DU QUADRILATERE ON CONTROLE L'APPLATISSEMENT
                if (nno2 .eq. 4) then
                    call eclapp(dimgeo, nno2, lonmin, zr(iagese+(nuno2- 4)*dimgeo))
                endif
            end do
!
        end do
  2     continue
    end do
!
    call jedetr(nomobj)
!
!     3. CONSTRUCTION DES OBJETS DU MAILLAGE RESULTAT :
!     -------------------------------------------------
    nbmail=nbse
    nbnoeu=nbno2t
    nbcoor=dimgeo
!
!     3.1 CREATION DE L'OBJET .DIME  :
!     ------------------------------------
    call wkvect(ma2//'.DIME', 'G V I', 6, iadime)
    zi(iadime-1+1)= nbnoeu
    zi(iadime-1+3)= nbmail
    zi(iadime-1+6)= nbcoor
!
!
!     3.2 CREATION DES OBJETS .NOMNOE ET .NOMMAI :
!     --------------------------------------------
    call jecreo(ma2//'.NOMNOE', 'G N K8')
    call jeecra(ma2//'.NOMNOE', 'NOMMAX', nbnoeu)
    call jecreo(ma2//'.NOMMAI', 'G N K8')
    call jeecra(ma2//'.NOMMAI', 'NOMMAX', nbmail)
!
    nom(1:1)='N'
    do k = 1, nbnoeu
        call codent(k, 'G', nom(2:8))
        call jecroc(jexnom(ma2//'.NOMNOE', nom))
    end do
    nom(1:1)='M'
    do k = 1, nbmail
        call codent(k, 'G', nom(2:8))
        call jecroc(jexnom(ma2//'.NOMMAI', nom))
    end do
!
!
!     3.3 CREATION DES OBJETS  .CONNEX ET .TYPMAIL
!     ---------------------------------------------
    call jecrec(ma2//'.CONNEX', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmail)
    call jeecra(ma2//'.CONNEX', 'LONT', nbnoeu, ' ')
    call jeveuo(ma2//'.CONNEX', 'E', ibid)
!
    nuno2=0
    do ima = 1, nbmail
        nno2=zi(ianno2-1+ima)
        call jecroc(jexnum(ma2//'.CONNEX', ima))
        call jeecra(jexnum(ma2//'.CONNEX', ima), 'LONMAX', nno2)
        do ino2 = 1, nno2
            nuno2=nuno2+1
            zi(ibid-1+nuno2)=nuno2
        end do
    end do
!
!
!
!     3.4 CREATION DU CHAMP DE GEOMETRIE (.COORDO)
!     ---------------------------------------------
    call copisd('CHAMP_GD', 'G', ma1//'.COORDO', ma2//'.COORDO')
    call jedetr(ma2//'.COORDO    .VALE')
    call wkvect(ma2//'.COORDO    .VALE', 'G V R', 3*nbnoeu, ibid)
    call jeveuo(ma2//'.COORDO    .REFE', 'E', vk24=refe)
    refe(1)=ma2
!
    do k = 1, dimgeo
        nuno2=0
        do ima = 1, nbmail
            nno2=zi(ianno2-1+ima)
!         -- ON FAIT UN PETIT "SHRINK" SUR LES MAILLES :
            xc=0.d0
            do ino = 1, nno2
                nuno2=nuno2+1
                xc= xc+zr(iagese-1+(nuno2-1)*dimgeo+k)/dble(nno2)
            end do
            nuno2=nuno2-nno2
!
            do ino = 1, nno2
                nuno2=nuno2+1
                xm= zr(iagese-1+(nuno2-1)*dimgeo+k)
                xm=xc+shrink*(xm-xc)
                zr(ibid-1+(nuno2-1)*3+k)=xm
            end do
        end do
    end do
!
!
!
    call jedetr('&&ECLPGM.NOMOBJ')
    AS_DEALLOCATE(vr=geopoini)
    AS_DEALLOCATE(vi=connexse)
    call jedetr('&&ECLPGM.GEOSE')
    call jedetr('&&ECLPGM.NBBO2')
    call jedema()
!
! FIN ------------------------------------------------------------------
end subroutine
