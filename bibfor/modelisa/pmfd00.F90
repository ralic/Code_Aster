subroutine pmfd00()
    implicit  none
#include "jeveux.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
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
! --- ------------------------------------------------------------------
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
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
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    integer :: ncarfi
    parameter  (ncarfi=3)
!
    integer :: nbocc0, nbocc1, nbocc2, nbocc3, nbocc4
    integer :: iret, ibid, ifm, niv, iarg, iasbon, iasedi, iasmax
    integer :: nbvm, nmailp, numail, nbfib, icode, igrand, ima, inomcp
    integer :: ii, jj, ioc, ipos, icmp, ier, izone, nbcmp, nbec
    integer :: ira1, iriy1, iriz1, irva1, irviy1, irviz1
    integer :: jdnm, jnf, jmp
    integer :: jnbfg, nbgf, jngf, jcarfi, jpoint, ipoint, ngf, ig, ng, ig1
    integer :: nummai, nbmaza, idesc, ilima, ivale, nbfig
!
!     NB DE GROUPES MAX PAR ELEMENT
!     CE NOMBRE DOIT ETRE EN ACCORD AVEC LES CATALOGUES
!     GRANDEUR_SIMPLE__.CATA ET GENER_MEPMF1.CATA !
    integer :: ngmxel
    character(len=2) :: kngmx
    parameter     (ngmxel=10,kngmx='10')
    integer :: nugrp(ngmxel)
!
    real(kind=8) :: zero
    real(kind=8) :: casect(6), carg(6)
    real(kind=8) :: airpou, moinoy, moinoz, erre, precai
    parameter  (zero=0.d+0)
!
    character(len=1) :: k1bid
    character(len=8) :: carele, nomo, noma, k8b, modele, sdgf, ngrand
    character(len=16) :: concep, cmd, ltymcl(3)
    character(len=19) :: cesdec, ligrmo, celbid
    character(len=24) :: modnom, mommai, vpoint, vnbfib, vcarfi, vnbfig, rnomgf
    character(len=24) :: k24bid
!
    integer :: valmi
    real(kind=8) :: valmr(4)
    character(len=80) :: valmk(2)
!
    data ltymcl/'MAILLE','GROUP_MA','TOUT'/
! --- ------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
    call getvid(' ', 'MODELE', 1, iarg, 1,&
                nomo, nbvm)
!
    call getres(carele, concep, cmd)
!
    vnbfib = carele//'.PMFNF'
!
! --- -------------------------------------------------------
    call getfac('MULTIFIBRE', nbocc0)
    if (nbocc0 .ne. 0) then
        call getvid(' ', 'GEOM_FIBRE', 1, iarg, 1,&
                    sdgf, ibid)
        vnbfig = sdgf//'.NB_FIBRE_GROUPE'
        vpoint = sdgf//'.POINTEUR'
        vcarfi = sdgf//'.CARFI'
        rnomgf = sdgf//'.NOMS_GROUPES'
!        NOMBRE DE GROUPES TOTAL = DIMENSION DE VNBFIG
        call jelira(vnbfig, 'LONMAX', nbgf, k1bid)
        call jeveuo(vnbfig, 'L', jnbfg)
        call jeveuo(vcarfi, 'L', jcarfi)
        call jeveuo(vpoint, 'L', jpoint)
!        NOMBRE TOTAL DE FIBRES SUR TOUS LES GROUPES
        nbfib=0
        do 10 ig = 1, nbgf
            nbfib=nbfib+zi(jnbfg-1+ig)
10      continue
    endif
!
    modnom = nomo//'.MODELE    .LGRF'
    call jeveuo(modnom, 'L', jdnm)
    noma = zk8(jdnm)
    mommai = noma//'.NOMMAI'
    call jelira(mommai, 'NOMMAX', nmailp, k1bid)
!
! --- -------------------------------------------------------
!     S'IL N'Y A PAS D'ELEMENTS A SOUS-POINTS, ON SAUTE TOUT:
    call getfac('COQUE', nbocc1)
    call getfac('GRILLE', nbocc2)
    call getfac('POUTRE', nbocc3)
    call getfac('MEMBRANE', nbocc4)
    if ((nbocc0+nbocc1+nbocc2+nbocc3+nbocc4) .eq. 0) goto 9999
!     2EME CHANCE :
    call getvid(' ', 'MODELE', 0, iarg, 1,&
                modele, ibid)
    ligrmo = modele//'.MODELE'
    celbid='&&PMFD00.CELBID'
    call alchml(ligrmo, 'TOU_INI_ELEM', 'PNBSP_I', 'V', celbid,&
                iret, ' ')
    call detrsd('CHAM_ELEM', celbid)
    if (iret .eq. 1) goto 9999
!     S'IL N'Y A PAS D'ELEMENTS PMF, ON SAUTE :
    if (nbocc0 .eq. 0) goto 200
!
! --- -------------------------------------------------------
!     CONSTRUCTION DES OBJETS .PMFPT .PMFNF ET .PMFCF
    call wkvect('&&PMFD00.NOMS_GROUPES', 'V V K24', nbgf, jngf)
    call wkvect(vnbfib, 'V V I', nmailp*(2+ngmxel), jnf)
!
!     DESCRIPTEUR DE LA CARTE
    call jeveuo(carele//'.CARGENPO  .DESC', 'L', idesc)
!     NUMERO DE LA GRANDEUR DANS LE CATALOGUE DE GRANDEUR
    igrand = zi(idesc)
!     NOMBRE DE ZONE MAX DANS LA CARTE
    iasmax = zi(idesc+1)
!     NOMBRE DE ZONE AFFECTEE DANS LA CARTE
    iasedi = zi(idesc+2)
!     NOM DE LA GRANDEUR
    call jenuno(jexnum('&CATA.GD.NOMGD', igrand), ngrand)
!     NOMBRE ET NOM DES COMPOSANTES DANS LA GRANDEUR
    call jelira(jexnum('&CATA.GD.NOMCMP', igrand), 'LONMAX', nbcmp, k1bid)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', igrand), 'L', inomcp)
!     NOMBRE D'ENTIER CODE DANS LA CARTE
    call dismoi('F', 'NB_EC', ngrand, 'GRANDEUR', nbec,&
                k8b, ier)
!
    valmi = 0
    do 100 ioc = 1, nbocc0
        do 110 ig = 1, ngmxel
            nugrp(ig)=0
110      continue
        call reliem(nomo, noma, 'NU_MAILLE', 'MULTIFIBRE', ioc,&
                    2, ltymcl, ltymcl, '&&PMFD00.MAILLSEP', nmailp)
        call jeveuo('&&PMFD00.MAILLSEP', 'L', jmp)
! ---    NOMBRE DE GROUPES A AFFECTER
        call getvtx('MULTIFIBRE', 'GROUP_FIBRE', ioc, iarg, 0,&
                    k8b, ngf)
        ngf=-ngf
        if (ngf .gt. ngmxel) then
            call u2mesk('F', 'MODELISA8_7', 1, kngmx)
        endif
! ---    NOMS DES GROUPES A AFFECTER
        call getvtx('MULTIFIBRE', 'GROUP_FIBRE', ioc, iarg, ngf,&
                    zk24( jngf), ibid)
! ---    NOMBRE DE FIBRES DE L'ENSEMBLE DES GROUPES
!        ON NOTE LES NUMEROS DE GROUPES
        nbfib=0
        do 120 ig = 1, ngf
            call jenonu(jexnom(rnomgf, zk24(jngf+ig-1)), ng)
            nbfib = nbfib+zi(jnbfg+ng-1)
            nugrp(ig) = ng
120      continue
!
!        ON AFFECTE LES ELEMENTS POUTRES CONCERNES PAR CETTE OCCURENCE
!        POUR CHAQUE EL : NB DE FIBRE, NB DE GROUPES DE FIBRES
!        ET NUMERO DES GROUPES
        do 130 jj = 1, nmailp
            numail = zi(jmp+jj-1)
            ipos = jnf+(numail-1)*(2+ngmxel)
            zi(ipos) = nbfib
            zi(ipos+1) = ngf
            do 131 ig = 1, ngmxel
                zi(ipos+1+ig) = nugrp(ig)
131          continue
130      continue
!
!        INTEGRATION POUR TOUS LES GROUPES DE CETTE OCCURENCE
        do 135 ii = 1, 6
            casect(ii)=zero
135      continue
        do 140 ig = 1, ngf
            ig1 = nugrp(ig)
            nbfig = zi(jnbfg -1+ig1)
            ipoint = zi(jpoint-1+ig1)
            call pmfitg(nbfig, ncarfi, zr(jcarfi+ipoint-1), carg)
            do 141 ii = 1, 6
                casect(ii) = casect(ii) + carg(ii)
141          continue
140      continue
!
!        BOUCLE SUR LES MAILLES
        do 150 ima = 1, nmailp
            nummai=zi(jmp + ima -1)
!           RECHERCHE DE LA ZONE COMTENANT NUMMAI
            iasbon = 0
            do 155 ii = 1, iasedi
                icode = zi(idesc+3+2*(ii-1))
                izone = zi(idesc+3+2*(ii-1)+1)
!              SI C'EST UNE LISTE DE MAILLE
                if (icode .eq. 3) then
                    k24bid = carele//'.CARGENPO  .LIMA'
                    call jeveuo(jexnum(k24bid, izone), 'L', ilima)
                    call jelira(jexnum(k24bid, izone), 'LONMAX', nbmaza, k1bid)
!              SI C'EST UN GROUPE DE MAILLE
                else if (icode.eq.2) then
                    k24bid = noma//'.GROUPEMA'
                    call jeveuo(jexnum(k24bid, izone), 'L', ilima)
                    call jelira(jexnum(k24bid, izone), 'LONMAX', nbmaza, k1bid)
!              SI C'EST TOUT LE MAILLAGE
                else if (icode.eq.1) then
                    iasbon = ii
                    goto 160
                else
                    call assert(.false.)
                endif
!              MAILLE DANS LISTE OU GROUPE DE MAILLE DE CETTE ZONE
                do 152 jj = 1, nbmaza
                    if (nummai .eq. zi(ilima+jj-1)) then
                        iasbon = ii
                        goto 160
                    endif
152              continue
155          continue
            if (iasbon .eq. 0) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                call u2mesk('F', 'ALGELINE_34', 1, valmk(1))
            endif
160          continue
!           RANG DES COMPOSANTES A1, IY1, IZ1 DANS LA CARTE
            ira1 = indik8( zk8(inomcp), 'A1' , 1, nbcmp )
            iriy1 = indik8( zk8(inomcp), 'IY1', 1, nbcmp )
            iriz1 = indik8( zk8(inomcp), 'IZ1', 1, nbcmp )
            if (ira1 .eq. 0 .or. iriy1 .eq. 0 .or. iriy1 .eq. 0) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                call u2mesk('F', 'MODELISA8_3', 1, valmk)
            endif
!           ENTIER CODE DE LA ZONE
            icode = zi(idesc+3+2*iasmax+nbec*(iasbon-1))
!           RANG DE LA VALEUR DANS L'ENTIER CODE (0 SI N'EXISTE PAS)
            irva1 = rgcmpg(icode,ira1)
            irviy1 = rgcmpg(icode,iriy1)
            irviz1 = rgcmpg(icode,iriz1)
            if (irva1 .eq. 0 .or. irviy1 .eq. 0 .or. irviz1 .eq. 0) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                call u2mesk('F', 'MODELISA8_3', 1, valmk)
            endif
!           ON RECUPERE LES COMPOSANTES : A1, IY1, IZ1 DE CETTE ZONE
            call jeveuo(carele//'.CARGENPO  .VALE', 'L', ivale)
            airpou=zr(ivale+(iasbon-1)*nbcmp + irva1 - 1)
            moinoy=zr(ivale+(iasbon-1)*nbcmp + irviy1 - 1)
            moinoz=zr(ivale+(iasbon-1)*nbcmp + irviz1 - 1)
            if (airpou .eq. zero) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                call u2mesk('F', 'MODELISA8_1', 1, valmk)
            endif
            if (moinoy .eq. zero) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                valmk(2) = 'IY'
                call u2mesk('F', 'MODELISA8_2', 2, valmk)
            endif
            if (moinoz .eq. zero) then
                call jenuno(jexnum(mommai, nummai), valmk(1))
                valmk(2) = 'IZ'
                call u2mesk('F', 'MODELISA8_2', 2, valmk)
            endif
!           COMPARAISON DE LA SOMME DES AIRES DES FIBRES
            call getvr8('MULTIFIBRE', 'PREC_AIRE', ioc, iarg, 1,&
                        precai, iret)
            erre=abs(airpou-casect(1))/airpou
            if (erre .gt. precai) then
                valmi = ioc
                call jenuno(jexnum(mommai, nummai), valmk(1))
                valmr(1) = airpou
                valmr(2) = casect(1)
                valmr(3) = erre
                valmr(4) = precai
                call u2mesg('E', 'MODELISA8_4', 1, valmk, 1,&
                            valmi, 4, valmr)
            endif
!           COMPARAISON DES MOMENTS D'INERTIES : IY, IZ
            call getvr8('MULTIFIBRE', 'PREC_INERTIE', ioc, iarg, 1,&
                        precai, iret)
            erre=abs(moinoy-casect(5))/moinoy
            if (erre .gt. precai) then
                valmi = ioc
                call jenuno(jexnum(mommai, nummai), valmk(1))
                valmk(2) = 'IY'
                valmr(1) = moinoy
                valmr(2) = casect(5)
                valmr(3) = erre
                valmr(4) = precai
                call u2mesg('E', 'MODELISA8_5', 2, valmk, 1,&
                            valmi, 4, valmr)
            endif
            erre=abs(moinoz-casect(4))/moinoz
            if (erre .gt. precai) then
                valmi = ioc
                call jenuno(jexnum(mommai, nummai), valmk(1))
                valmk(2) = 'IY'
                valmr(1) = moinoz
                valmr(2) = casect(4)
                valmr(3) = erre
                valmr(4) = precai
                call u2mesg('E', 'MODELISA8_5', 2, valmk, 1,&
                            valmi, 4, valmr)
            endif
150      continue
100  end do
    if (valmi .ne. 0) then
        call u2mess('F', 'MODELISA8_6')
    endif
!
!
200  continue
!
! --- -------------------------------------------------------
!     TRAITEMENT DES MOTS CLES COQUE_NCOU,TUYAU_NCOU, ...
    cesdec = '&&PMFD00.CESDEC'
    call pmfd02(noma, cesdec)
!
! --- -------------------------------------------------------
!     CONSTRUCTION DES CHAM_ELEM '.CANBSP' ET '.CAFIBR'
    call pmfd01(noma, carele, vnbfib, vpoint, vcarfi,&
                vnbfig, cesdec, ngmxel)
    call jedetr(vnbfib)
    call detrsd('CHAM_ELEM_S', cesdec)
    if (niv .eq. 2) then
        call imprsd('CHAMP', carele//'.CANBSP', 6, 'INFO=2')
        call imprsd('CHAMP', carele//'.CAFIBR', 6, 'INFO=2')
    endif
9999  continue
    call jedema()
end subroutine
