subroutine pmfd00()
!
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
!
! --------------------------------------------------------------------------------------------------
!
!                    COMMANDE AFFE_CARA_ELEM
!
!     TRAITEMENT DES MOTS CLES :
!           COQUE  /COQUE_NCOU
!           GRILLE /COQUE_NCOU
!           POUTRE /TUYAU_NCOU
!           POUTRE /TUYAU_NSEC
!
!     CONSTRUCTION DU CHAM_ELEM (CONSTANT PAR MAILLE) CONTENANT
!     LES INFORMATIONS DE "DECOUPAGE" DES ELEMENTS DE STRUCTURE :
!     NBRE DE COUCHES (COQUE), DE SECTEURS (TUYAU), "FIBRES" (PMF),..
!
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/indik8.h"
#include "asterc/r8prem.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/imprsd.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/pmfd01.h"
#include "asterfort/pmfd02.h"
#include "asterfort/pmfitg.h"
#include "asterfort/reliem.h"
#include "asterfort/rgcmpg.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbocc0, nbocc1, nbocc2, nbocc3, nbocc4
    integer :: iret, ibid, ifm, niv, iasbon, iasedi, iasmax, ncarfi, ncarfimax
    integer :: nbvm, nmailp, numail, nbfib, icode, igrand, ima, inomcp
    integer :: ii, jj, ioc, ipos, izone, nbcmp, nbec
    integer :: ira1, iriy1, iriz1, irva1, irviy1, irviz1
    integer :: jdnm, jmailfib
    integer :: jnbfig, nbgf, jngf, jcafig, jpofig, jsdfig, jtyfig, ipoint, ngf, ig, ng, ig1
    integer :: nummai, nbmaza,  ilima,  nbfig,tyfib
!
!   Nb de groupes max par élément. Ce nombre doit etre en accord avec les catalogues
!       grandeur_simple__.cata   NBSP_I ==> NUG[ngmxel]
!       gener_mepmf1.cata        NBSP_I ==> NUG[ngmxel]
    integer, parameter ::ngmxel=10
    integer :: nugrp(ngmxel)
!
    real(kind=8) :: casect(6), carg(6)
    real(kind=8) :: airpou, moinoy, moinoz, erre, precai
!
    character(len=8) :: carele, nomo, noma, modele, sdgf, ngrand
    character(len=16) :: concep, cmd, ltymcl(3)
    character(len=19) :: cesdec, ligrmo, celbid
    character(len=24) :: modnom, mommai, vpofig, vmailfib, vcafig, vnbfig, vnmfig, vsdfig, vtyfig
    character(len=24) :: k24bid
!
    integer :: valmi
    real(kind=8) :: valmr(4)
    character(len=80) :: valmk(2)
!
    integer, pointer :: maillsep(:) => null()
    integer, pointer :: desc(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!
    data ltymcl/'MAILLE','GROUP_MA','TOUT'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
    call getvid(' ', 'MODELE', scal=nomo, nbret=nbvm)
    call getres(carele, concep, cmd)
    vmailfib = carele//'.PMFNF'
! --------------------------------------------------------------------------------------------------
    call getfac('MULTIFIBRE', nbocc0)
    if (nbocc0 .ne. 0) then
        call getvid(' ', 'GEOM_FIBRE', scal=sdgf, nbret=ibid)
        vnbfig = sdgf//'.NB_FIBRE_GROUPE'
        vtyfig = sdgf//'.TYPE_GROUPE'
        vpofig = sdgf//'.POINTEUR'
        vcafig = sdgf//'.CARFI'
        vnmfig = sdgf//'.NOMS_GROUPES'
        vsdfig = sdgf//'.CARACSD'
!       nombre de groupes total = dimension de vnbfig
        call jelira(vnbfig, 'LONMAX', nbgf)
        call jeveuo(vnbfig, 'L', jnbfig)
        call jeveuo(vtyfig, 'L', jtyfig)
        call jeveuo(vcafig, 'L', jcafig)
        call jeveuo(vpofig, 'L', jpofig)
        call jeveuo(vsdfig, 'L', jsdfig)
!       nombre total de fibres sur tous les groupes
        nbfib=0
        do ig = 1, nbgf
            nbfib=nbfib+zi(jnbfig-1+ig)
        enddo
    endif
!
    modnom = nomo//'.MODELE    .LGRF'
    call jeveuo(modnom, 'L', jdnm)
    noma = zk8(jdnm)
    mommai = noma//'.NOMMAI'
    call jelira(mommai, 'NOMMAX', nmailp)
! --------------------------------------------------------------------------------------------------
!   s'il n'y a pas d'elements a sous-points, on saute tout
    call getfac('COQUE', nbocc1)
    call getfac('GRILLE', nbocc2)
    call getfac('POUTRE', nbocc3)
    call getfac('MEMBRANE', nbocc4)
    if ((nbocc0+nbocc1+nbocc2+nbocc3+nbocc4) .eq. 0) goto 999
!   2eme chance
    call getvid(' ', 'MODELE', scal=modele, nbret=ibid)
    ligrmo = modele//'.MODELE'
    celbid='&&PMFD00.CELBID'
    call alchml(ligrmo, 'TOU_INI_ELEM', 'PNBSP_I', 'V', celbid, iret, ' ')
    call detrsd('CHAM_ELEM', celbid)
    if (iret .eq. 1) goto 999
!   s'il n'y a pas d'elements pmf, on saute
    if (nbocc0 .eq. 0) goto 200
! --------------------------------------------------------------------------------------------------
!   construction des objets .pmfpt .pmfnf et .pmfcf
    call wkvect('&&PMFD00.NOMS_GROUPES', 'V V K24', nbgf, jngf)
    call wkvect(vmailfib, 'V V I', nmailp*(4+ngmxel), jmailfib)
!   Descripteur de la carte
    call jeveuo(carele//'.CARGENPO  .DESC', 'L', vi=desc)
!   Numero de la grandeur dans le catalogue de grandeur
    igrand = desc(1)
!   nombre de zone max dans la carte
    iasmax = desc(2)
!   nombre de zone affectee dans la carte
    iasedi = desc(3)
!   nom de la grandeur
    call jenuno(jexnum('&CATA.GD.NOMGD', igrand), ngrand)
!   nombre et nom des composantes dans la grandeur
    call jelira(jexnum('&CATA.GD.NOMCMP', igrand), 'LONMAX', nbcmp)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', igrand), 'L', inomcp)
!   rang des composantes a1, iy1, iz1 dans la carte
    ira1  = indik8( zk8(inomcp), 'A1' , 1, nbcmp )
    iriy1 = indik8( zk8(inomcp), 'IY1', 1, nbcmp )
    iriz1 = indik8( zk8(inomcp), 'IZ1', 1, nbcmp )
    ASSERT(ira1 .ne. 0 .and. iriy1 .ne. 0 .and. iriz1 .ne. 0)
!   nombre d'entier code dans la carte
    call dismoi('NB_EC', ngrand, 'GRANDEUR', repi=nbec)
!   valeurs
    call jeveuo(carele//'.CARGENPO  .VALE', 'L', vr=vale)
!
    valmi = 0
    do ioc = 1, nbocc0
        nugrp(:)=0
        call reliem(nomo, noma, 'NU_MAILLE', 'MULTIFIBRE', ioc,&
                    2, ltymcl, ltymcl, '&&PMFD00.MAILLSEP', nmailp)
        call jeveuo('&&PMFD00.MAILLSEP', 'L', vi=maillsep)
!       nombre de groupes a affecter
        call getvtx('MULTIFIBRE', 'GROUP_FIBRE', iocc=ioc, nbval=0, nbret=ngf)
        ngf=-ngf
        if (ngf .gt. ngmxel) then
            call utmess('F', 'MODELISA8_7', si=ngmxel)
        endif
!       noms des groupes a affecter
        call getvtx('MULTIFIBRE', 'GROUP_FIBRE', iocc=ioc, nbval=ngf, vect=zk24(jngf), nbret=ibid)
!       Nombre de fibres de l'ensemble des groupes.
!           On note les numéros de groupes
!           On vérifie que le type des groupes est le même
        nbfib = 0
        tyfib = 0
        do ig = 1, ngf
            call jenonu(jexnom(vnmfig, zk24(jngf+ig-1)), ng)
            if (ng .eq. 0) then
                call utmess('F', 'MODELISA6_18', sk=zk24(jngf+ig-1))
            endif
            nbfib = nbfib+zi(jnbfig+ng-1)
            if (ig.eq.1) then
                tyfib  = zi(jtyfig+ng-1)
            else
                if ( tyfib.ne.zi(jtyfig+ng-1) ) then
                    valmk(1) = zk24(jngf)
                    valmk(2) = zk24(jngf+ig-1)
                    call utmess('F', 'MODELISA6_17', nk=2, valk=valmk)
                endif
            endif
            nugrp(ig) = ng
        enddo
        ASSERT( (tyfib.ne.1).or.(tyfib.ne.2) )
        ncarfi = zi(jsdfig+tyfib)
        ncarfimax = max(zi(jsdfig+1),zi(jsdfig+2))
!       on affecte les éléments poutres concernés par cette occurence pour chaque EL :
!           nb de fibre, nb de groupes de fibres, numéro des groupes
!           dans la SD on ne mémorise que ncarfimax, c'est le dimensionnement de la carte.
        do jj = 1, nmailp
            numail = maillsep(jj)
            ipos = jmailfib+(numail-1)*(4+ngmxel)
            zi(ipos)   = nbfib
            zi(ipos+1) = ngf
            zi(ipos+2) = tyfib
            zi(ipos+3) = ncarfimax
            do ig = 1, ngmxel
                zi(ipos+3+ig) = nugrp(ig)
            enddo
        enddo
!       intégration pour tous les groupes de cette occurence
        casect(:)=0.0d+0
        do ig = 1, ngf
            ig1    = nugrp(ig)
            nbfig  = zi(jnbfig-1+ig1)
            ipoint = zi(jpofig-1+ig1)
            call pmfitg(tyfib, nbfig, ncarfi, zr(jcafig+ipoint-1), carg)
            do ii = 1, 6
                casect(ii) = casect(ii) + carg(ii)
            enddo
        enddo
!       boucle sur les mailles
        do ima = 1, nmailp
            nummai=maillsep(ima)
!           recherche de la zone comtenant nummai
            iasbon = 0
            do ii = 1, iasedi
                icode = desc(1+3+2*(ii-1))
                izone = desc(1+3+2*(ii-1)+1)
!               si c'est une liste de maille
                if (icode .eq. 3) then
                    k24bid = carele//'.CARGENPO  .LIMA'
                    call jeveuo(jexnum(k24bid, izone), 'L', ilima)
                    call jelira(jexnum(k24bid, izone), 'LONMAX', nbmaza)
!               si c'est un groupe de maille
                else if (icode.eq.2) then
                    k24bid = noma//'.GROUPEMA'
                    call jeveuo(jexnum(k24bid, izone), 'L', ilima)
                    call jelira(jexnum(k24bid, izone), 'LONMAX', nbmaza)
!               si c'est tout le maillage
                else if (icode.eq.1) then
                    iasbon = ii
                    goto 160
                else
                    ASSERT(.false.)
                endif
!               maille dans liste ou groupe de maille de cette zone
                do jj = 1, nbmaza
                    if (nummai .eq. zi(ilima+jj-1)) then
                        iasbon = ii
                        goto 160
                    endif
                enddo
            enddo
            if (iasbon .eq. 0) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                call utmess('F', 'ALGELINE_34', sk=valmk(1))
            endif
160         continue
!           entier code de la zone
            icode = desc(1+3+2*iasmax+nbec*(iasbon-1))
!           rang de la valeur dans l'entier code (0 si n'existe pas)
            irva1  = rgcmpg(icode,ira1)
            irviy1 = rgcmpg(icode,iriy1)
            irviz1 = rgcmpg(icode,iriz1)
            if (irva1 .eq. 0 .or. irviy1 .eq. 0 .or. irviz1 .eq. 0) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                call utmess('F', 'MODELISA8_3', sk=valmk(1))
            endif
!           on recupere les composantes : a1, iy1, iz1 de cette zone
            airpou=vale(1+(iasbon-1)*nbcmp + irva1 - 1)
            moinoy=vale(1+(iasbon-1)*nbcmp + irviy1 - 1)
            moinoz=vale(1+(iasbon-1)*nbcmp + irviz1 - 1)
            if ( airpou .le. r8prem() ) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                call utmess('F', 'MODELISA8_1', sk=valmk(1))
            endif
            if ( moinoy .le. r8prem() ) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                valmk(2) = 'IY'
                call utmess('F', 'MODELISA8_2', nk=2, valk=valmk)
            endif
            if ( moinoz .le. r8prem() ) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                valmk(2) = 'IZ'
                call utmess('F', 'MODELISA8_2', nk=2, valk=valmk)
            endif
!           Test sur la section
!               comparaison sur la somme des aires des fibres
            call getvr8('MULTIFIBRE', 'PREC_AIRE', iocc=ioc, scal=precai, nbret=iret)
            erre=abs(airpou-casect(1))/airpou
            if (erre .gt. precai) then
                valmi = ioc
                call jenuno(jexnum(mommai, nummai), valmk(1))
                valmr(1) = airpou
                valmr(2) = casect(1)
                valmr(3) = erre
                valmr(4) = precai
                call utmess('E', 'MODELISA8_4', sk=valmk(1), si=valmi, nr=4, valr=valmr)
            endif
!           Test sur les inerties
            if ( tyfib .eq. 1 ) then
!               comparaison des moments d'inerties : iy, iz
                call getvr8('MULTIFIBRE', 'PREC_INERTIE', iocc=ioc, scal=precai, nbret=iret)
                erre=abs(moinoy-casect(5))/moinoy
                if (erre .gt. precai) then
                    valmi = ioc
                    call jenuno(jexnum(mommai, nummai), valmk(1))
                    valmk(2) = 'IY'
                    valmr(1) = moinoy
                    valmr(2) = casect(5)
                    valmr(3) = erre
                    valmr(4) = precai
                    call utmess('E', 'MODELISA8_5', nk=2, valk=valmk, si=valmi, nr=4, valr=valmr)
                endif
                erre=abs(moinoz-casect(4))/moinoz
                if (erre .gt. precai) then
                    valmi = ioc
                    call jenuno(jexnum(mommai, nummai), valmk(1))
                    valmk(2) = 'IZ'
                    valmr(1) = moinoz
                    valmr(2) = casect(4)
                    valmr(3) = erre
                    valmr(4) = precai
                    call utmess('E', 'MODELISA8_5', nk=2, valk=valmk, si=valmi, nr=4, valr=valmr)
                endif
            else if ( tyfib .eq. 2 ) then
!               Si tyfib=2 pas de test sur les inerties
            endif
        enddo
    enddo
    if (valmi .ne. 0) then
        call utmess('F', 'MODELISA8_6')
    endif
!
200 continue
! --------------------------------------------------------------------------------------------------
!   traitement des mots clefs COQUE_NCOU,TUYAU_NCOU, ...
    cesdec = '&&PMFD00.CESDEC'
    call pmfd02(noma, cesdec)
! --------------------------------------------------------------------------------------------------
!   construction des CHAM_ELEM '.CANBSP' et '.CAFIBR'
    call pmfd01(noma, carele, vmailfib, sdgf, cesdec, ngmxel)
    call jedetr(vmailfib)
    call detrsd('CHAM_ELEM_S', cesdec)
    if (niv .eq. 2) then
        call imprsd('CHAMP', carele//'.CANBSP', 6, 'INFO=2')
        call imprsd('CHAMP', carele//'.CAFIBR', 6, 'INFO=2')
    endif
999 continue
    call jedema()
end subroutine
