subroutine op0119()
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
!                O P E R A T E U R    DEFI_GEOM_FIBRE
!
! --------------------------------------------------------------------------------------------------
!
! person_in_charge: jean-luc.flejou at edf.fr
!
    implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/dgfassefibres.h"
#include "asterfort/dgffibres.h"
#include "asterfort/dgfsections.h"
#include "asterfort/gcncon.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/maillagefibre.h"
#include "asterfort/pmfsce.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ncarfi1,ncarfi2
    parameter  (ncarfi1=3, ncarfi2=7)
    real(kind=8) :: pi4
    parameter  (pi4=0.7853981633974483d+00)
!
    integer :: nbvfibre, maxfibre1, maxfibre2, nbfibres1, nbfibres2
    integer :: iret, ifm, niv, nboccsec, nboccfib, nboccasf, ii, nbmagr, iidepnoeud, nbocctype1
    integer :: nbmaills, nttri3, ntseg2, ntqua4, ntpoi1, correni(4), ncarma
    integer :: numno, nbfibreass, jj, ng, iptrng
    integer :: ulnbnoeuds, ulnbmailles, maxmailgrp
    integer :: iinbnoeuds, iinbmailles
    integer :: nbv, jdtm, nummai, nutyma
    integer :: nbgf, jpofig, ioc, ido, jdo, ipos, in, nno, no, jcf, jdno, jdco, jnbfig, jtyfig
    integer :: ibid, ipointeur, iinbgf, jmafig, jmaill, jsdfig
!
    real(kind=8) :: x(4), y(4), centre(2), axep(2), surf
!
    character(len=8)  :: sdgf, nomas, ksudi, nommai, nogfma
    character(len=16) :: concep, cmd, limcls(3), ltymcl(3)
    character(len=24) :: mlgtms, mlgcnx, mlgcoo, mlgtma, mlgtno, nomgf
    character(len=24) :: vnbfig, vtyfig, vcafig, vpofig, vnmfig, vmafig, vsdfig
!
    character(len=24) :: valk(3)
!
    integer, pointer ::             vmailgrp(:)     => null()
    integer, pointer ::             vimailles(:)    => null()
    integer, pointer ::             vigroup(:)      => null()
    integer, pointer ::             vinoeud(:)      => null()
    real(kind=8),pointer ::         valfibre(:)     => null()
    real(kind=8),pointer ::         vcoord(:)       => null()
    character(len=24),pointer ::    vngroup(:)      => null()
!
    integer, pointer ::             tousgroupesnbf(:) => null()
    character(len=24),pointer ::    tousgroupesnom(:) => null()
    real(kind=8),pointer ::         coorgrfibreass(:) => null()
    real(kind=8),pointer ::         gxgrfibreass(:)   => null()
    character(len=24),pointer ::    nomgrfibreass(:)  => null()
!
    data limcls/'MAILLE_SECT','GROUP_MA_SECT','TOUT_SECT'/
    data ltymcl/'MAILLE','GROUP_MA','TOUT'/
!
! --------------------------------------------------------------------------------------------------
    call jemarq()
    iret=0
!
!   récupération des arguments de la commande
    call getres(sdgf, concep, cmd)
!
!   nombre des groupes de fibres pour dimensionner les objets
    call getfac('SECTION', nboccsec)
    call getfac('FIBRE',   nboccfib)
    call getfac('ASSEMBLAGE_FIBRE',nboccasf)
    nbgf = nboccsec + nboccfib + nboccasf
    ASSERT( nbgf.gt.0 )
!
! --------------------------------------------------------------------------------------------------
!   SD GEOM_FIBRE
!       NOMS_GROUPES    : noms des groupes de fibres (répertoire de noms)
!       NB_FIBRE_GROUPE : nombre de fibres par groupe
!       TYPE_GROUPE     : type de groupe de fibre (1, 2)
!       CARFI           : caractéristiques de fibres (tout à la suite en 1 seul vecteur)
!       POINTEUR        : pointeur pour les caractéristiques : ipos-1+[1..nb_figre_groupe]
!       GFMA            : nom du maillage global des groupes de fibres
!       CARACSD         : caractéristiques de la SD (nbgf , ncar_type1, ncar_type2)
    vnmfig = sdgf//'.NOMS_GROUPES'
    vnbfig = sdgf//'.NB_FIBRE_GROUPE'
    vtyfig = sdgf//'.TYPE_GROUPE'
    vcafig = sdgf//'.CARFI'
    vpofig = sdgf//'.POINTEUR'
    vmafig = sdgf//'.GFMA'
    vsdfig = sdgf//'.CARACSD'
!
    call jecreo(vnmfig, 'G N K24')
    call jeecra(vnmfig, 'NOMMAX', nbgf, ' ')
!
    call wkvect(vnbfig, 'G V I', nbgf, jnbfig)
    call wkvect(vtyfig, 'G V I', nbgf, jtyfig)
    call wkvect(vpofig, 'G V I', nbgf, jpofig)
    call wkvect(vmafig, 'G V K8',   1, jmafig)
    call wkvect(vsdfig, 'G V I',    3, jsdfig)
!
!   nom du maillage global
    call gcncon('_', nogfma)
    zk8(jmafig) = nogfma
!
!   Récupération du niveau d'impression
    call infmaj()
    call infniv(ifm, niv)
!
!   Récupération des types mailles TRI3, QUAD4, SEG2, POI1
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3'), nttri3)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD4'), ntqua4)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'),  ntseg2)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'),  ntpoi1)
!
!   Pour tous les groupes de fibres : Nom , nombre de fibre
    AS_ALLOCATE( size=nbgf, vk24 = tousgroupesnom )
    AS_ALLOCATE( size=nbgf, vi = tousgroupesnbf )
! --------------------------------------------------------------------------------------------------
!   comptage du nombre de :
!       - fibres ==> nombre de maille : QUA4 + TRI3 + POI1
!       - noeuds
!       - max de maille dans un groupe
    ulnbnoeuds  = 0
    ulnbmailles = 0
    maxmailgrp  = 10
    nbfibres1   = 0
    nbfibres2   = 0
    iinbgf      = 0
! --------------------------------------------------------------------------------------------------
!   Les sections (de type 1)
    call dgfsections(nboccsec, iinbgf, tousgroupesnom, tousgroupesnbf, maxmailgrp, &
                     ulnbnoeuds, ulnbmailles, nbfibres1)
! --------------------------------------------------------------------------------------------------
!   Les fibres ponctuelles (de type 1)
    call dgffibres(nboccfib, iinbgf, tousgroupesnom, tousgroupesnbf, maxmailgrp, &
                   ulnbnoeuds, ulnbmailles, nbfibres1, maxfibre1, ncarfi1)
! --------------------------------------------------------------------------------------------------
!   Les fibres ponctuelles (de type 2)
    nbocctype1 = nboccfib + nboccsec
    call dgfassefibres(nboccasf, iinbgf, tousgroupesnom, tousgroupesnbf, maxmailgrp, &
                       ulnbnoeuds, ulnbmailles, nbfibres2, maxfibre2, ncarfi2, nbocctype1)
!   On libère
    AS_DEALLOCATE( vk24 = tousgroupesnom )
    AS_DEALLOCATE( vi = tousgroupesnbf )
!
!   Si pas de noeuds et pas de mailles ==> <F>
    ASSERT( ulnbnoeuds.gt.0 )
    ASSERT( ulnbmailles.gt.0 )
! --------------------------------------------------------------------------------------------------
!   Dimensionnement des vecteurs pour description du maillage
!   Avec    :
!       nbgf        : Nombre de groupe
!       ulnbnoeuds  : Nombre de noeuds maximum
!       ulnbmailles : Nombre exact de mailles
!       maxmailgrp  : Le maximum de mailles dans un groupe
!   Dimensionnement des vecteurs de travail
!       vcoord      :   Coordonnées des fibres dans la section droite, dimension 2.
!       vngroup     :   Nom des groupes de mailles
!       vmailgrp    :   Nombre de maille par groupe
!       vigroup     :   Liste des mailles des groupes.
!           Pour ième groupe [1..nbgf]
!                jème maille du groupe [1..vmailgrp(i)]
!           vigroup( (i-1)*maxmailgrp + j ) c'est la jème maille du ième groupe
!       vimailles   :   Table de connectivité des mailles
!           Mailles du type POI1 ou QUAD4 ou TRI3 : 4 noeuds ==> ncarma = 4 + 2
!           vimailles( (i-1)*ncarma + 1 )       : Type de la ième maille
!           vimailles( (i-1)*ncarma + 2 )       : Nombre de noeud de la ième maille
!           vimailles( (i-1)*ncarma + 2 + j )   : jème noeud de la ième maille
    AS_ALLOCATE( size=ulnbnoeuds*2, vr = vcoord )
    AS_ALLOCATE( size=nbgf, vk24 = vngroup )
    AS_ALLOCATE( size=nbgf, vi = vmailgrp )
    AS_ALLOCATE( size=nbgf*maxmailgrp, vi = vigroup )
    ncarma = 6
    AS_ALLOCATE( size=ulnbmailles*ncarma,  vi = vimailles)
!   Correspondance entre les noeuds du maillage des fibres et du maillage initial de la section
    AS_ALLOCATE( size=ulnbnoeuds, vi = vinoeud )
!
! --------------------------------------------------------------------------------------------------
!   Vecteur de la SD GEOM_FIBRES (carfi)
    call wkvect(vcafig, 'G V R', nbfibres1*ncarfi1+nbfibres2*ncarfi2, jcf)
    ipointeur   = 1
    iinbgf      = 0
    iinbnoeuds  = 0
    iinbmailles = 0
    iidepnoeud  = 1
! --------------------------------------------------------------------------------------------------
!   Les fibres à partir des mailles TRIA3, QUAD4 (de type 1)
    do ioc = 1, nboccsec
        call getvtx('SECTION', 'GROUP_FIBRE', iocc=ioc, scal=nomgf, nbret=ibid)
        if (niv .eq. 2) write(ifm,800) nomgf
!       On récupère le nom du maillage
        call getvid('SECTION', 'MAILLAGE_SECT', iocc=ioc, scal=nomas, nbret=nbv)
!       Récupération des coordonnées de l'axe de la poutre
        call getvr8('SECTION', 'COOR_AXE_POUTRE', iocc=ioc, nbval=2, vect=axep, nbret=iret)
        if (iret .ne. 2) axep(1:2) = 0.0d0
!       Concept maillage associé
        mlgtms = nomas//'.TYPMAIL'
        mlgcnx = nomas//'.CONNEX'
        mlgcoo = nomas//'.COORDO    .VALE'
        mlgtma = nomas//'.NOMMAI'
        mlgtno = nomas//'.NOMNOE'
!       Récupération des adresses utiles
        call jeveuo(mlgtms, 'L', jdtm)
        call jeveuo(mlgcoo, 'L', jdco)
!       On récupère les mailles, les noeuds de la section associés au groupe
        call reliem(' ', nomas, 'NU_MAILLE', 'SECTION', ioc,&
                    3, limcls, ltymcl, '&&OP0119.GRPMAILL', nbmaills)
        call jeveuo('&&OP0119.GRPMAILL', 'L', jmaill)
!       Nom du groupe de mailles
        call jenonu(jexnom(vnmfig, nomgf), iret)
        if ( iret .ne. 0 ) then
            call utmess('F', 'MODELISA6_19', sk=nomgf)
        endif
        iinbgf = iinbgf + 1
        vngroup( iinbgf ) = nomgf
!
        nbmagr = 0
        do jdo = 1, nbmaills
            nummai = zi(jmaill+jdo-1)
!           Si c'est SEG2 on passe
            nutyma = zi(jdtm+nummai-1)
            if (nutyma.eq.ntseg2) cycle
!           Coordonnées des noeuds de la maille
            call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
            nno = 3
            if (nutyma.eq.ntqua4) nno = 4
            do in  = 1, nno
                no = zi(jdno-1+in)
                x(in) = zr(jdco+(no-1)*3)   - axep(1)
                y(in) = zr(jdco+(no-1)*3+1) - axep(2)
            enddo
!           Recherche de la correspondance dans le vecteur vinoeud
            correni(1:4) = 0
            cin: do in = 1, nno
                no = zi(jdno-1+in)
                do ii = iidepnoeud, iinbnoeuds
                    if ( no.eq.vinoeud(ii) ) then
                        correni(in) = ii
                        cycle cin
                    endif
                enddo
                iinbnoeuds = iinbnoeuds + 1
                vinoeud(iinbnoeuds) = no
                correni(in) = iinbnoeuds
            enddo cin
!           La maille
            nbmagr = nbmagr + 1
            iinbmailles = iinbmailles + 1
            ii = (iinbmailles-1)*ncarma+1
            vimailles( ii    ) = nutyma
            vimailles( ii +1 ) = nno
            vimailles( ii +2 : ii+2+nno ) = correni(1:nno)
            vigroup( (iinbgf-1)*maxmailgrp +nbmagr ) = iinbmailles
!           Pour la fibre : surface et centre
            call pmfsce(nno, x, y, surf, centre)
!           Stockage des caractéristiques de fibres dans la SD : Yf Zf Aire
            ipos = jcf + ipointeur - 1 + ncarfi1*(nbmagr-1)
            zr(ipos)   = centre(1)
            zr(ipos+1) = centre(2)
            zr(ipos+2) = surf
            if (niv .eq. 2) then
                call jenuno(jexnum(mlgtma, nummai), nommai)
                if (nno .eq. 3) then
                    write (ifm,801) iinbmailles,nommai,'TRIA3',centre,surf
                else
                    write (ifm,801) iinbmailles,nommai,'QUAD4',centre,surf
                endif
            endif
        enddo
!       Nombre de maille du groupe
        vmailgrp(iinbgf) = nbmagr
!       Les nouveaux noeuds dans la SD MAILLAGE
        do ii = iidepnoeud , iinbnoeuds
            numno = vinoeud(ii)
            vcoord( (ii-1)*2 + 1 ) = zr(jdco+(numno-1)*3)   - axep(1)
            vcoord( (ii-1)*2 + 2 ) = zr(jdco+(numno-1)*3+1) - axep(2)
        enddo
        iidepnoeud = iinbnoeuds+1
        call jecroc(jexnom(vnmfig, nomgf))
        zi(jnbfig+iinbgf-1) = nbmagr
        zi(jpofig+iinbgf-1)   = ipointeur
        zi(jtyfig+iinbgf-1) = 1
        ipointeur = ipointeur + nbmagr*ncarfi1
    enddo
!
! --------------------------------------------------------------------------------------------------
!   Les fibres à partir des mailles POI1 (de type 1)
    AS_ALLOCATE( size=maxfibre1, vr=valfibre )
    do ioc = 1, nboccfib
        call getvtx('FIBRE', 'GROUP_FIBRE', iocc=ioc, scal=nomgf, nbret=ibid)
        if (niv .eq. 2) write (ifm,820) nomgf
!       Surface ou diametre
        call getvtx('FIBRE', 'CARA', iocc=ioc, scal=ksudi, nbret=iret)
        if (iret .eq. 0) ksudi = 'SURFACE '
        call getvr8('FIBRE', 'VALE', iocc=ioc, nbval=maxfibre1, vect=valfibre, nbret=nbvfibre)
!       Récupération des coordonnées de l'axe de la poutre
        call getvr8('FIBRE', 'COOR_AXE_POUTRE', iocc=ioc, nbval=2, vect=axep, nbret=iret)
        if (iret .ne. 2) axep(1:2) = 0.0d0
!       Nom du groupe de mailles
        call jenonu(jexnom(vnmfig, nomgf), iret)
        if ( iret .ne. 0 ) then
            call utmess('F', 'MODELISA6_19', sk=nomgf)
        endif
        iinbgf = iinbgf + 1
        vngroup(iinbgf) = nomgf
!
!       Si diamètre ==> calcul de la surface
        nbmagr = 0
        do ido = 1, nbvfibre/ncarfi1
            centre(1) = valfibre(ncarfi1*(ido-1)+1) - axep(1)
            centre(2) = valfibre(ncarfi1*(ido-1)+2) - axep(2)
            if (ksudi .eq. 'DIAMETRE') then
                surf = valfibre(ncarfi1*(ido-1)+3)*valfibre(ncarfi1*(ido-1)+3)*pi4
            else
                surf = valfibre(ncarfi1*(ido-1)+3)
            endif
!           La maille
            nbmagr = nbmagr + 1
            iinbmailles = iinbmailles + 1
            nno = 1
            vigroup( (iinbgf-1)*maxmailgrp +nbmagr ) = iinbmailles
            ii = (iinbmailles-1)*ncarma + 1
            vimailles( ii    ) = ntpoi1
            vimailles( ii +1 ) = 1
            vimailles( ii +2 ) = ido+iinbnoeuds
!           Stockage des caractéristiques de fibres dans la SD : Yf Zf Aire
            ipos = jcf + ipointeur - 1 + ncarfi1*(nbmagr-1)
            zr(ipos)   = centre(1)
            zr(ipos+1) = centre(2)
            zr(ipos+2) = surf
            if (niv .eq. 2) then
                write (ifm,821) iinbmailles,centre,surf
            endif
!           Le noeud dans la SD MAILLAGE
            vcoord( (ido+iinbnoeuds-1)*2 + 1 ) = centre(1)
            vcoord( (ido+iinbnoeuds-1)*2 + 2 ) = centre(2)
        enddo
!       Nombre de maille du groupe
        vmailgrp(iinbgf) = nbmagr
        iinbnoeuds       = iinbnoeuds + nbvfibre/ncarfi1
        call jecroc(jexnom(vnmfig, nomgf))
        zi(jnbfig+iinbgf-1) = nbmagr
        zi(jpofig+iinbgf-1) = ipointeur
        zi(jtyfig+iinbgf-1) = 1
        ipointeur = ipointeur + nbmagr*ncarfi1
    enddo
    AS_DEALLOCATE( vr=valfibre )
!
! --------------------------------------------------------------------------------------------------
!   Les fibres à partir des mailles POI1 (de type 2)
    AS_ALLOCATE( size=maxfibre2, vr=valfibre )
    do ioc = 1, nboccasf
        call getvtx('ASSEMBLAGE_FIBRE', 'GROUP_ASSE_FIBRE', iocc=ioc, scal=nomgf, nbret=ibid)
        if (niv .eq. 2) write (ifm,830) nomgf
!       Nom des groupes de fibres composant l'assemblage
        call getvtx('ASSEMBLAGE_FIBRE', 'GROUP_FIBRE', iocc=ioc, nbval=0, nbret=nbfibreass)
        nbfibreass = -nbfibreass
        AS_ALLOCATE( size=nbfibreass, vk24 = nomgrfibreass)
        AS_ALLOCATE( size=nbfibreass*2, vr = coorgrfibreass)
        AS_ALLOCATE( size=nbfibreass,   vr = gxgrfibreass)
        call getvtx('ASSEMBLAGE_FIBRE', 'GROUP_FIBRE',      iocc=ioc, &
                        nbval=nbfibreass, vect=nomgrfibreass)
        call getvr8('ASSEMBLAGE_FIBRE', 'COOR_GROUP_FIBRE', iocc=ioc, &
                        nbval= nbfibreass*2, vect=coorgrfibreass)
        call getvr8('ASSEMBLAGE_FIBRE', 'GX_GROUP_FIBRE',   iocc=ioc, &
                        nbval=nbfibreass, vect=gxgrfibreass)
!       Nom du groupe de mailles, Nombre de maille du groupe
        call jenonu(jexnom(vnmfig, nomgf), iret)
        if ( iret .ne. 0 ) then
            call utmess('F', 'MODELISA6_19', sk=nomgf)
        endif
        iinbgf = iinbgf + 1
        vngroup(iinbgf) = nomgf
!       Remplissage de valfibre Yf Zf Aire Yp Zp GX Num
        nbvfibre = 0
        do ii =1 , nbfibreass
!           On pointe sur le groupe de type 1
            call jenonu(jexnom(vnmfig, nomgrfibreass(ii)), ng)
            if ( ng.eq.0 ) then
                valk(1)=nomgf
                valk(2)='GROUP_FIBRE'
                valk(3)= nomgrfibreass(ii)
                call utmess('F', 'MODELISA6_24', nk=3, valk=valk)
            endif
!           C'est obligatoirement un type 1
            if ( zi(jtyfig+ng-1) .ne. 1 ) then
                valk(1)=nomgf
                valk(2)=nomgrfibreass(ii)
                call utmess('F', 'MODELISA6_25', nk=2, valk=valk)
            endif
!           Données du groupe 'ng' : [ iptrng, iptrng+nbmagr*ncarfi1-1 ]
            iptrng = zi(jpofig+ng-1)
!           coor_group_fibre
            axep(1) = coorgrfibreass(1+(ii-1)*2)
            axep(2) = coorgrfibreass(2+(ii-1)*2)
!           Boucle sur le nombre de fibre du groupe 'ng' : Yf Zf Aire Yp Zp GX Num
            do jj=1, zi(jnbfig+ng-1)
                nbvfibre=nbvfibre+1
                valfibre(ncarfi2*(nbvfibre-1)+1) = zr(jcf+iptrng-1+(jj-1)*ncarfi1  )+axep(1)
                valfibre(ncarfi2*(nbvfibre-1)+2) = zr(jcf+iptrng-1+(jj-1)*ncarfi1+1)+axep(2)
                valfibre(ncarfi2*(nbvfibre-1)+3) = zr(jcf+iptrng-1+(jj-1)*ncarfi1+2)
                valfibre(ncarfi2*(nbvfibre-1)+4) = axep(1)
                valfibre(ncarfi2*(nbvfibre-1)+5) = axep(2)
                valfibre(ncarfi2*(nbvfibre-1)+6) = gxgrfibreass(ii)
                valfibre(ncarfi2*(nbvfibre-1)+7) = ii
            enddo
        enddo
!
        nbmagr = 0
        do ido = 1, nbvfibre
!           La maille
            nbmagr = nbmagr + 1
            iinbmailles = iinbmailles + 1
            nno = 1
            vigroup( (iinbgf-1)*maxmailgrp +nbmagr ) = iinbmailles
            ii = (iinbmailles-1)*ncarma + 1
            vimailles( ii    ) = ntpoi1
            vimailles( ii +1 ) = 1
            vimailles( ii +2 ) = ido+iinbnoeuds
!           Stockage des caractéristiques de fibres dans la SD : Yf Zf Aire Yp Zp GX Num
            ipos = jcf + ipointeur - 1 + ncarfi2*(nbmagr-1)
            zr(ipos)   = valfibre(ncarfi2*(ido-1)+1)
            zr(ipos+1) = valfibre(ncarfi2*(ido-1)+2)
            zr(ipos+2) = valfibre(ncarfi2*(ido-1)+3)
            zr(ipos+3) = valfibre(ncarfi2*(ido-1)+4)
            zr(ipos+4) = valfibre(ncarfi2*(ido-1)+5)
            zr(ipos+5) = valfibre(ncarfi2*(ido-1)+6)
            zr(ipos+6) = valfibre(ncarfi2*(ido-1)+7)
            if (niv .eq. 2) then
                write (ifm,831) iinbmailles,zr(ipos:ipos+5),nint(zr(ipos+6))
            endif
!           Le noeud dans la SD MAILLAGE
            vcoord( (ido+iinbnoeuds-1)*2 + 1 ) = zr(ipos)
            vcoord( (ido+iinbnoeuds-1)*2 + 2 ) = zr(ipos+1)
        enddo
!       Nombre de maille du groupe
        vmailgrp(iinbgf) = nbmagr
        iinbnoeuds       = iinbnoeuds + nbvfibre
        call jecroc(jexnom(vnmfig, nomgf))
        zi(jnbfig+iinbgf-1) = nbmagr
        zi(jpofig+iinbgf-1) = ipointeur
        zi(jtyfig+iinbgf-1) = 2
        ipointeur = ipointeur + nbmagr*ncarfi2
        AS_DEALLOCATE( vk24 = nomgrfibreass )
        AS_DEALLOCATE( vr = coorgrfibreass )
        AS_DEALLOCATE( vr = gxgrfibreass )
    enddo
    AS_DEALLOCATE( vr=valfibre )
!
! --------------------------------------------------------------------------------------------------
    ASSERT( iinbnoeuds.le.ulnbnoeuds )
    ASSERT( ulnbmailles.eq.iinbmailles)
    ASSERT( nbgf.eq.iinbgf )
!
! --------------------------------------------------------------------------------------------------
!   On complète la SD GEOM_FIBRE
    zi(jsdfig     ) = nbgf
    zi(jsdfig + 1 ) = ncarfi1
    zi(jsdfig + 2 ) = ncarfi2
!
!   Création du maillage des fibres
    call maillagefibre(nogfma, ulnbnoeuds, maxmailgrp, nbgf, vcoord, iinbnoeuds, &
                       vigroup, vngroup, vmailgrp, vimailles, ulnbmailles, ncarma)
!
800 format(/,'DETAIL DES FIBRES SURFACIQUES DU GROUPE "',A,'"',&
           /,'NUMF  MAILLE    TYPE    Y             Z             SURF')
801 format(i4,2x,a8,2x,a5,3(2x,1pe12.5))
!
820 format(/,'DETAIL DES FIBRES PONCTUELLES DU GROUPE "',A,'"',&
            /,'NUMF   Y             Z             SURF')
821 format(i4,3(2x,1pe12.5))
!
830 format(/,'DETAIL DES FIBRES PONCTUELLES DU GROUPE "',A,'"',&
           /'NUMF   Y             Z             SURF',&
            '          YP            ZP            GX             NUM')
831 format(i4,6(2x,1pe12.5),2x,i4)
!
! --------------------------------------------------------------------------------------------------
!   Destructions
    call jedetr('&&OP0119.GRPMAILL')
    AS_DEALLOCATE( vr   = vcoord )
    AS_DEALLOCATE( vi   = vinoeud )
    AS_DEALLOCATE( vi   = vmailgrp )
    AS_DEALLOCATE( vi   = vigroup )
    AS_DEALLOCATE( vi   = vimailles )
    AS_DEALLOCATE( vk24 = vngroup )
    call jedema()
end subroutine
