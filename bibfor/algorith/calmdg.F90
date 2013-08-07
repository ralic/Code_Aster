subroutine calmdg(model, modgen, nugene, num, nu,&
                  ma, mate, moint, moflui, ndble,&
                  itxsto, itysto, itzsto, iprsto, nbmo,&
                  iadirg)
! aslint: disable=
    implicit none
#include "jeveux.h"
!
#include "asterc/getvtx.h"
#include "asterfort/codent.h"
#include "asterfort/delat.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexnum.h"
#include "asterfort/rangen.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/tabcor.h"
#include "asterfort/trprot.h"
#include "asterfort/wkvect.h"
    character(len=*) :: mate
!
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
! AUTEUR : G. ROUSSEAU
! ROUTINE CALCULANT :
!     A PARTIR D UN MODELE GENERALISE : CHAMPS DE
!     TEMPERATURE CORRESPONDANT A TOUS LES MODES
!     DE CHACUNE DES SOUS-STRUCTURES, ET LES CHAMPS DE PRESSION
!     ASSOCIES
!     IN: K2 : MODEL : CHARACTER TRADUISANT LA DIMENSION DU FLUIDE
!     IN: K8 : MODGEN : MODELE GENERALISE
!     IN : K14 : NUMEGE : NUMEROTATION DES DDLS GENERALISEES
!     IN : K14 : NUM :NUMEROTATION ASSOCIEE AU MODELE D INTERFACE
!     IN : K14 : NU :NUMEROTATION ASSOCIEE AU MODELE FLUIDE
!     IN : K8 : MA : MATRICE DE RAIDEUR DU FLUIDE
!     IN : K8 : MOINT,MOFLUI : MODELE INTERFACE, MODELE FLUIDE
!     IN : I : NDBLE :INDICATEUR DE RECHERCHE DE NOEUDS DOUBLES
!     OUT : INTEGER : ITXSTO,ITYSTO,ITZSTO,IPRSTO :
!           ADRESSE DU CHAMP DE TEMP
!     CORRESPONDANT A TOUS LES MODES
!     DE CHAQUE SSTRUCTURE,RESP. DECOMPOSE
!     SUIVANT DX PUIS DY PUIS DZ - CHAMP DE PRESSION ASSOCIE A UN MODE
!     OUT : NOMBRE DE MODES TOTAL (SOMME DES MODES CONTRAINTS ET
!     DYNAMIQUES DE CHACUNE DES SOUS-STRUCTURES )
!     OUT : INTEGER : IADIRG : ADRESSE DU TABLEAU CONTENANT
!           LE RANG DES DDLS GENERALISES HORS LAGRANGES
!
!---------------------------------------------------------------------
    integer :: ibid, nbid, isst, iadrp
    integer :: i, j, itxsto, itysto, iprsto
    integer :: icor(2), ndble
    real(kind=8) :: tgeom(6)
    real(kind=8) :: norm1, norm2, reste(3), deuxpi
    character(len=2) :: model
    character(len=6) :: chaine
    character(len=8) :: repon, moflui, moint, ma, k8bid
    character(len=8) :: modgen, bamo, macel, mailla, maflui
    character(len=14) :: nu, num
    character(len=14) :: nugene
    character(len=24) :: nomcha
    complex(kind=8) :: cbid
    integer :: iarg
! -----------------------------------------------------------------
!---------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: iadirg, iadrx, iadry, iadrz, iadx, iady, iadz
    integer :: ibamo, icompt, idelat, ierd, igeo, ilires, ilmax
    integer :: imacl, imodg, ind, ior, iprs, irang, iret
    integer :: irot, itabl, itzsto, k, nbmo, nbmod, nbmodg
    integer :: nbsst, nn
    real(kind=8) :: bid, ebid
!-----------------------------------------------------------------------
    call jemarq()
!
!=====================================================================
!---------------------------------------------------------------------
!                 CALCUL SUR MODELE GENERALISE
!---------------------------------------------------------------------
!=====================================================================
!
    call jelira(modgen//'      .MODG.SSNO        ', 'NOMMAX', nbsst)
    call getvtx(' ', 'AVEC_MODE_STAT', 0, iarg, 1,&
                repon, nn)
    if (repon(1:3) .eq. 'NON') then
        call delat(modgen, nbsst, nbmo)
        call jeveuo('&&DELAT.INDIC', 'L', idelat)
    endif
!
! CREATION DE TABLEAUX D ADRESSES PAR SOUS-STRUCTURES POUR LES
! NOMS DES CHAMNO DE DEPL_R PAR COMPOSANTES ET LA PRESSION
!
    call wkvect('&&CALMDG.TABL_MODE', 'V V I', nbsst, itabl)
    call wkvect('&&CALMDG.TABL_ADRX', 'V V I', nbsst, iadrx)
    call wkvect('&&CALMDG.TABL_ADRY', 'V V I', nbsst, iadry)
    if (model .eq. '3D') call wkvect('&&CALMDG.TABL_ADRZ', 'V V I', nbsst, iadrz)
    call wkvect('&&CALMDG.TABL_ADRP', 'V V I', nbsst, iadrp)
    call wkvect('&&CALMDG.TABL_LONMAX', 'V V I', nbsst, ilmax)
!
!
    ilires=0
    icompt=0
    do 1 isst = 1, nbsst
        k=0
        call jeveuo(jexnum(modgen//'      .MODG.SSME', isst), 'L', imacl)
        macel=zk8(imacl)
!
        call jeveuo(macel//'.MAEL_REFE', 'L', ibamo)
        bamo = zk24(ibamo)(1:8)
!
! TEST POUR DETERMINER SI FLUIDE ET STRUCTURE S APPUIENT SUR
! DES MAILLAGES COMMUNS
!
        call rsexch('F', bamo, 'DEPL', 1, nomcha,&
                    iret)
        call dismoi('F', 'NOM_MAILLA', nomcha(1:19), 'CHAM_NO', ibid,&
                    mailla, ierd)
        call dismoi('F', 'NOM_MAILLA', moint, 'MODELE', ibid,&
                    maflui, ierd)
        if (maflui .ne. mailla) then
            call tabcor(model, mate, mailla, maflui, moint,&
                        num, ndble, icor)
        endif
!
! RECUPERATION DES CMPS DE TRANSLATION
!
        call jeveuo(jexnum(modgen//'      .MODG.SSTR', isst), 'L', igeo)
!
        tgeom(1)=zr(igeo)
        tgeom(2)=zr(igeo+1)
        tgeom(3)=zr(igeo+2)
!
! RECUPERATION DES CMPS DE ROTATION
!
        call jeveuo(jexnum(modgen//'      .MODG.SSOR', isst), 'L', irot)
!
        tgeom(4)=zr(irot)
        tgeom(5)=zr(irot+1)
        tgeom(6)=zr(irot+2)
!
        norm1 = sqrt(tgeom(1)**2+tgeom(2)**2+tgeom(3)**2)
!
        deuxpi = 4.0d0 * acos ( 0.0d0 )
!
        do 3 ior = 1, 3
!
            reste(ior)=mod(tgeom(ior+3),deuxpi)
!
 3      continue
!
        if ((reste(1).eq.0.0d0) .and. (reste(2).eq.0.0d0) .and. (reste(3) .eq.0.0d0)) then
            norm2 = 0.0d0
        else
            norm2 = 1.0d0
        endif
!
!
!
! ON RECUPERE LE NOMBRE DE MODES DANS LA BASE MODALE DU MACRO-ELEMENT
! DEFINI
!
        call rsorac(bamo, 'LONUTI', ibid, bid, k8bid,&
                    cbid, ebid, 'ABSOLU', nbmodg, 1,&
                    nbid)
!
        zi(itabl+isst-1)=nbmodg
!
! CREATION DE VECTEURS CONTENANT LES NOMS DES VECTEURS DE CHAMP AUX
! NOEUDS DE DEPLACEMENTS SUIVANT OX  OY  OZ AINSI QUE LE CHAMP DE
! PRESSION ASSOCIE A CHAQUE MODE PROPRE OU CONTRAINT DE CHAQUE SST
! ON STOCKE LES ADRESSES DES VECTEURS DANS LES TABLEAUX D ADRESSES
! PRECEDEMMENT CREES
!
        chaine = 'CBIDON'
!
        call codent(isst, 'D0', chaine(1:6))
        call jecreo('&&CALMDG.TXSTO'//chaine, 'V V K24')
        call jeecra('&&CALMDG.TXSTO'//chaine, 'LONMAX', nbmodg)
        call jeecra('&&CALMDG.TXSTO'//chaine, 'LONUTI', nbmodg)
        call jeveut('&&CALMDG.TXSTO'//chaine, 'E', iadx)
        zi(iadrx+isst-1)=iadx
        call jecreo('&&CALMDG.TYSTO'//chaine, 'V V K24')
        call jeecra('&&CALMDG.TYSTO'//chaine, 'LONMAX', nbmodg)
        call jeecra('&&CALMDG.TYSTO'//chaine, 'LONUTI', nbmodg)
        call jeveut('&&CALMDG.TYSTO'//chaine, 'E', iady)
        zi(iadry+isst-1)=iady
        if (model .eq. '3D') then
            call jecreo('&&CALMDG.TZSTO'//chaine, 'V V K24')
            call jeecra('&&CALMDG.TZSTO'//chaine, 'LONMAX', nbmodg)
            call jeecra('&&CALMDG.TZSTO'//chaine, 'LONUTI', nbmodg)
            call jeveut('&&CALMDG.TZSTO'//chaine, 'E', iadz)
            zi(iadrz+isst-1)=iadz
        endif
        call jecreo('&&CALMDG.PRES'//chaine, 'V V K24')
        call jeecra('&&CALMDG.PRES'//chaine, 'LONMAX', nbmodg)
        call jeecra('&&CALMDG.PRES'//chaine, 'LONUTI', nbmodg)
        call jeveut('&&CALMDG.PRES'//chaine, 'E', iprs)
        zi(iadrp+isst-1)=iprs
!
! RECUPERATION DES MODES PROPRES ET CONTRAINTS AUQUELS ON
! FAIT SUBIR ROTATION ET TRANSLATION DEFINIES DANS LE MODELE
! GENERALISE. CETTE ROUTINE PERMET AUSSI LE TRANSPORT
! D UNE SOUS STRUCTURE A UNE AUTRE DES CHAMPS AUX NOEUDS
! CALCULES SUR UNE SEULE SOUS STRUCTURE
!
        do 2, imodg=1,nbmodg
        icompt=icompt+1
!
        if (repon(1:3) .eq. 'NON') then
            if (zi(idelat+icompt-1) .ne. 1) goto 2
        endif
!
        call trprot(model, bamo, tgeom, imodg, iadx,&
                    iady, iadz, isst, iadrp, norm1,&
                    norm2, ndble, num, nu, ma,&
                    mate, moint, ilires, k, icor)
!
!
 2      continue
!
! DESTRUCTION DU TABLEAU DES CORRESPONDANCES
!
        call jedetr('&&TABCOR.CORRE1')
!
        if (ndble .eq. 1) call jedetr('&&TABCOR.CORRE2')
!
 1  continue
!
!----------------------------------------------------------------
! CALCUL DU NOMBRE DE MODES TOTAL
!
    nbmo=0
    do 4 isst = 1, nbsst
        nbmo=nbmo+zi(itabl+isst-1)
 4  continue
!
! CREATION D UN TABLEAU DE VECTEURS CONTENANT LES NOMS DE TOUS
! LES VECTEURS DE DEPLACEMENTS ET DE PRESSION DE L ENSEMBLE
! DES SOUS STRUCTURES
!
    call jecreo('&&TPXSTO', 'V V K24')
    call jeecra('&&TPXSTO', 'LONMAX', nbmo)
    call jeecra('&&TPXSTO', 'LONUTI', nbmo)
    call jeveut('&&TPXSTO', 'E', itxsto)
    call jecreo('&&TPYSTO', 'V V K24')
    call jeecra('&&TPYSTO', 'LONMAX', nbmo)
    call jeecra('&&TPYSTO', 'LONUTI', nbmo)
    call jeveut('&&TPYSTO', 'E', itysto)
    if (model .eq. '3D') then
        call jecreo('&&TPZSTO', 'V V K24')
        call jeecra('&&TPZSTO', 'LONMAX', nbmo)
        call jeecra('&&TPZSTO', 'LONUTI', nbmo)
        call jeveut('&&TPZSTO', 'E', itzsto)
    endif
    call jecreo('&&VESTOC', 'V V K24')
    call jeecra('&&VESTOC', 'LONMAX', nbmo)
    call jeecra('&&VESTOC', 'LONUTI', nbmo)
    call jeveut('&&VESTOC', 'E', iprsto)
    call jecreo('&&TABIRG', 'V V I')
    call jeecra('&&TABIRG', 'LONMAX', nbmo)
    call jeecra('&&TABIRG', 'LONUTI', nbmo)
    call jeveut('&&TABIRG', 'E', iadirg)
!
    ind=0
    do 5 i = 1, nbsst
!
! NB DE MODES PAR SST
!
        nbmod=zi(itabl+i-1)
!
        do 6 j = 1, nbmod
!
            ind=ind+1
            zk24(itxsto+ind-1)=zk24(zi(iadrx+i-1)+j-1)
            zk24(itysto+ind-1)=zk24(zi(iadry+i-1)+j-1)
            if (model .eq. '3D') zk24(itzsto+ind-1)=zk24(zi(iadrz+i-1)+j- 1)
            zk24(iprsto+ind-1)=zk24(zi(iadrp+i-1)+j-1)
            call rangen(nugene//'.NUME', i, j, irang)
            zi(iadirg+ind-1)=irang
!
 6      continue
 5  continue
!
!
! --- MENAGE
!
    chaine = 'CBIDON'
    do 7 isst = 1, nbsst
!
        call codent(isst, 'D0', chaine(1:6))
        call jedetr('&&CALMDG.TXSTO'//chaine)
        call jedetr('&&CALMDG.TYSTO'//chaine)
        call jedetr('&&CALMDG.TZSTO'//chaine)
        call jedetr('&&CALMDG.PRES'//chaine)
!
 7  continue
!
!
    call jedetr('&&CALMDG.TABL_MODE')
    call jedetr('&&CALMDG.TABL_ADRX')
    call jedetr('&&CALMDG.TABL_ADRY')
    call jedetr('&&CALMDG.TABL_ADRZ')
    call jedetr('&&CALMDG.TABL_ADRP')
    call jedetr('&&CALMDG.TABL_LONMAX')
!
    call jedema()
end subroutine
