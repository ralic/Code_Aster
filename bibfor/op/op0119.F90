subroutine op0119()
!
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
! --- ------------------------------------------------------------------
!
!                O P E R A T E U R    DEFI_GEOM_FIBRE
!
! --- ------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/gcncon.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedetr.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
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
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: ncarfi
    parameter  (ncarfi=3)
    real(kind=8) :: pi4
    parameter  (pi4=0.7853981633974483d+00)
!
    integer :: nbvfibre, maxfibre
    integer :: iret, ifm, niv, nboccs, nboccp, ii, nbmagr, iidepnoeud
    integer :: nbmaills, nttri3, ntseg2, ntqua4, ntpoi1, correni(4), ncarma
    integer :: numno
    integer :: ulnbnoeuds, ulnbmailles, maxmailgrp
    integer :: iinbnoeuds, iinbmailles
    integer :: nbnoeuds
    integer :: nbv, jdtm, nummai, nutyma
    integer :: nbgf, jptr, ioc, ido, jdo, ipos, in, nno, no, jcf, jdno, jdco, jnfg
    integer :: ibid, ipointeur, iinbgf, jngfma, jmaill, jcarasd
!
    real(kind=8) :: x(4), y(4), centre(2), axep(2), surf
!
    character(len=7)  :: k7bid
    character(len=8)  :: sdgf, nomas, ktyma, ksudi, nommai, nogfma
    character(len=16) :: concep, cmd, limcls(3), ltymcl(3)
    character(len=24) :: mlgtms, mlgcnx, mlgcoo, mlgtma, mlgtno, nomgf
    character(len=24) :: vnbfig, vcafig, vpocfg, rnomgf, gfmagl, carasd, valk(3)
!
    integer, pointer ::             vmailgrp(:)     => null()
    integer, pointer ::             vimailles(:)    => null()
    integer, pointer ::             vigroup(:)      => null()
    integer, pointer ::             vinoeud(:)      => null()
    real(kind=8),pointer ::         valfibre(:)     => null()
    real(kind=8),pointer ::         vcoord(:)       => null()
    character(len=24),pointer ::    vngroup(:)      => null()
!
    data limcls/'MAILLE_SECT','GROUP_MA_SECT','TOUT_SECT'/
    data ltymcl/'MAILLE','GROUP_MA','TOUT'/
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    iret=0
!
!   RECUPERATION DES ARGUMENTS DE LA COMMANDE
    call getres(sdgf, concep, cmd)
!
!   NOMBRE DES GROUPES DE FIBRES POUR DIMENSIONNER LES OBJETS
    call getfac('SECTION', nboccs)
    call getfac('FIBRE', nboccp)
    nbgf = nboccs + nboccp
    ASSERT( nbgf.gt.0 )
!
!   SD GEOM_FIBRE
!       noms des groupes de fibres (répertoire de noms)
!       nombre de fibres par groupe
!       caractéristiques de fibres (tout à la suite en 1 seul vecteur)
!       pointeur pour les caractéristiques pour faciliter les accès
!       nom du maillage global des groupes de fibres
!       caractéristiques de la SD (ncarfi, nbgf)
    rnomgf = sdgf//'.NOMS_GROUPES'
    vnbfig = sdgf//'.NB_FIBRE_GROUPE'
    vcafig = sdgf//'.CARFI'
    vpocfg = sdgf//'.POINTEUR'
    gfmagl = sdgf//'.GFMA'
    carasd = sdgf//'.CARACSD'
!
    call jecreo(rnomgf, 'G N K24')
    call jeecra(rnomgf, 'NOMMAX', nbgf, ' ')
!
    call wkvect(vnbfig, 'G V I', nbgf, jnfg)
    call wkvect(vpocfg, 'G V I', nbgf, jptr)
    call wkvect(gfmagl, 'G V K8', 1, jngfma)
    call wkvect(carasd, 'G V I', 2,  jcarasd)
!
!   nom du maillage global
    call gcncon('_', nogfma)
    zk8(jngfma) = nogfma
!
!   Récupération du niveau d'impression
    call infmaj()
    call infniv(ifm, niv)
!
!   Récuperation des types mailles TRI3, QUAD4, SEG2, POI1
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3'), nttri3)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD4'), ntqua4)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'),  ntseg2)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'),  ntpoi1)

!   comptage du nombre de :
!       - fibres ==> nombre de maille : QUA4 + TRI3 + POI1
!       - noeuds
!       - max de maille dans un groupe
    ulnbnoeuds  = 0
    ulnbmailles = 0
    maxmailgrp  = 10
!   Les sections
    do ioc = 1, nboccs
        call getvid('SECTION', 'MAILLAGE_SECT', iocc=ioc, scal=nomas, nbret=nbv)
!       Type de maille dans le maillage associé
        mlgtms = nomas//'.TYPMAIL'
        call jeveuo(mlgtms, 'L', jdtm)
!       nombre de fibres = nombre de mailles concernées
        call reliem(' ', nomas, 'NU_MAILLE', 'SECTION', ioc,&
                    3, limcls, ltymcl, '&&OP0119.GRPMAILL', nbmaills)
        call reliem(' ', nomas, 'NU_NOEUD',  'SECTION', ioc,&
                    3, limcls, ltymcl, '&&OP0119.GRPNOEUD', nbnoeuds)
        ulnbnoeuds = ulnbnoeuds + nbnoeuds
        call jeveuo('&&OP0119.GRPMAILL', 'L', jmaill)
!       Les mailles : TRIA3 ou QUA4
!           - les SEG2 sont exclus, ils peuvent servir à la construction
!           - arrêt dans les autres cas
        nbmagr = 0
        do jdo = 1, nbmaills
            nummai = zi(jmaill+jdo-1)
            nutyma = zi(jdtm+nummai-1)
            if (nutyma.eq.ntseg2) cycle
            if ((nutyma.eq.nttri3).or.(nutyma.eq.ntqua4)) then
                nbmagr = nbmagr + 1
                ulnbmailles = ulnbmailles +1
            else
                call codent(nummai, 'G', k7bid)
                call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), ktyma)
                valk(1)=nomas
                valk(2)=k7bid
                valk(3)=ktyma
                call utmess('F', 'MODELISA6_27', nk=3, valk=valk)
            endif
        enddo
        maxmailgrp = max(maxmailgrp,nbmagr)
    enddo
!   Les fibres ponctuelles
    maxfibre = 10
    do ioc = 1, nboccp
        call getvr8('FIBRE', 'VALE', iocc=ioc, nbval=0, nbret=nbvfibre)
        nbvfibre = -nbvfibre
        maxfibre = max(maxfibre,nbvfibre)
!       Vérification multiple de 'ncarfi' pour 'vale' dans 'fibre'
        if ( modulo(nbvfibre,ncarfi).ne.0 ) then
            call getvtx('FIBRE', 'GROUP_FIBRE', iocc=ioc, scal=nomgf, nbret=ibid)
            call codent(nbvfibre, 'G', k7bid)
            valk(1)=nomgf
            valk(2)=k7bid
            call utmess('F', 'MODELISA6_26', nk=2, valk=valk)
        endif
        ulnbmailles = ulnbmailles + nbvfibre/ncarfi
        ulnbnoeuds  = ulnbnoeuds  + nbvfibre/ncarfi
        maxmailgrp  = max(maxmailgrp,nbvfibre/ncarfi)
    enddo
!   Si pas de noeuds et pas de mailles ==> <F>
    ASSERT( ulnbnoeuds.gt.0 )
    ASSERT( ulnbmailles.gt.0 )
!
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
!   Vecteur de la SD GEOM_FIBRES (carfi)
    call wkvect(vcafig, 'G V R', ulnbmailles*ncarfi, jcf)
    ipointeur   = 1
    iinbgf      = 0
    iinbnoeuds  = 0
    iinbmailles = 0
    iidepnoeud  = 1
! --------------------------------------------------------------------------------------------------
!   Les fibres à partir des mailles TRIA3, QUAD4
    do ioc = 1, nboccs
        call getvtx('SECTION', 'GROUP_FIBRE', iocc=ioc, scal=nomgf, nbret=ibid)
        if (niv .eq. 2) write(ifm,800) nomgf
!       On récupère le nom du maillage
        call getvid('SECTION', 'MAILLAGE_SECT', iocc=ioc, scal=nomas, nbret=nbv)
!       Récupération des coordonnées de l'axe de la poutre
        call getvr8('SECTION', 'COOR_AXE_POUTRE', iocc=ioc, nbval=2, vect=axep,&
                    nbret=iret)
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
!       Nom du groupe de mailles, Nombre de maille du groupe
        iinbgf = iinbgf + 1
        vngroup( iinbgf ) = nomgf
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
!           Stockage des caractéristiques de fibres dans la SD
            ipos = jcf + ipointeur - 1 + ncarfi*(nbmagr-1)
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
        vmailgrp(iinbgf) = nbmagr
!       Les nouveaux noeuds
        do ii = iidepnoeud , iinbnoeuds
            numno = vinoeud(ii)
            vcoord( (ii-1)*2 + 1 ) = zr(jdco+(numno-1)*3)   - axep(1)
            vcoord( (ii-1)*2 + 2 ) = zr(jdco+(numno-1)*3+1) - axep(2)
        enddo
        iidepnoeud = iinbnoeuds+1
!
        call jecroc(jexnom(rnomgf, nomgf))
        zi(jnfg+iinbgf-1) = nbmagr
        zi(jptr+iinbgf-1) = ipointeur
        ipointeur = ipointeur + nbmagr*ncarfi
    enddo
!
! --------------------------------------------------------------------------------------------------
!   Les fibres à partir des mailles POI1
    AS_ALLOCATE( size=maxfibre, vr = valfibre )
    do ioc = 1, nboccp
        call getvtx('FIBRE', 'GROUP_FIBRE', iocc=ioc, scal=nomgf, nbret=ibid)
        if (niv .eq. 2) write (ifm,820) nomgf
!       Surface ou diametre
        call getvtx('FIBRE', 'CARA', iocc=ioc, scal=ksudi, nbret=iret)
        if (iret .eq. 0) ksudi = 'SURFACE '
        call getvr8('FIBRE', 'VALE', iocc=ioc, nbval=maxfibre, vect=valfibre, &
                    nbret=nbvfibre)
!       Récupération des coordonnées de l'axe de la poutre
        call getvr8('FIBRE', 'COOR_AXE_POUTRE', iocc=ioc, nbval=2, vect=axep,&
                    nbret=iret)
        if (iret .ne. 2) axep(1:2) = 0.0d0
!       Nom du groupe de mailles, Nombre de maille du groupe
        iinbgf = iinbgf + 1
        vngroup(iinbgf) = nomgf
!       Si diamètre ==> calcul de la surface
        nbmagr = 0
        do ido = 1, nbvfibre/ncarfi
            centre(1) = valfibre(ncarfi*(ido-1)+1) - axep(1)
            centre(2) = valfibre(ncarfi*(ido-1)+2) - axep(2)
!           Le noeud
            vcoord( (ido+iinbnoeuds-1)*2 + 1 ) = centre(1)
            vcoord( (ido+iinbnoeuds-1)*2 + 2 ) = centre(2)
            if (ksudi .eq. 'DIAMETRE') then
                surf = valfibre(ncarfi*(ido-1)+3)*valfibre(ncarfi*(ido-1)+3)*pi4
            else
                surf = valfibre(ncarfi*(ido-1)+3)
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
!           Stockage des caractéristiques de fibres dans la SD
            ipos = jcf + ipointeur - 1 + ncarfi*(nbmagr-1)
            zr(ipos)   = centre(1)
            zr(ipos+1) = centre(2)
            zr(ipos+2) = surf
            if (niv .eq. 2) then
                write (ifm,821) iinbmailles,centre,surf
            endif
        enddo
        vmailgrp(iinbgf) = nbmagr
        iinbnoeuds       = iinbnoeuds + nbvfibre/ncarfi
!
        call jecroc(jexnom(rnomgf, nomgf))
        zi(jnfg+iinbgf-1) = nbmagr
        zi(jptr+iinbgf-1) = ipointeur
        ipointeur = ipointeur + nbmagr*ncarfi
    enddo
!
    ASSERT( iinbnoeuds.le.ulnbnoeuds )
    ASSERT( ulnbmailles.eq.iinbmailles)
    ASSERT( nbgf.eq.iinbgf )
!
!   SD GEOM_FIBRE
    zi(jcarasd   ) = ncarfi
    zi(jcarasd +1) = nbgf
!
!   Création du maillage des fibres
    call maillagefibre(nogfma, ulnbnoeuds, maxmailgrp, nbgf, vcoord, iinbnoeuds, &
                       vigroup, vngroup, vmailgrp, vimailles, ulnbmailles, ncarma)

800 format(/,'DETAIL DES FIBRES SURFACIQUES DU GROUPE "',A,'"',&
           /,'NUMF  MAILLE    TYPE        Y        ',&
             '     Z            SURF')
801 format(i4,2x,a8,2x,a5,3(2x,1pe12.5))
!
820 format(/,'DETAIL DES FIBRES PONCTUELLES DU GROUPE "',A,'"',&
            /,'NUMF       Y             Z            SURF')
821 format(i4,3(2x,1pe12.5))
!
!   Destructions
    call jedetr('&&OP0119.GRPMAILL')
!
    AS_DEALLOCATE( vr   = vcoord )
    AS_DEALLOCATE( vi   = vinoeud )
    AS_DEALLOCATE( vi   = vmailgrp )
    AS_DEALLOCATE( vr   = valfibre )
    AS_DEALLOCATE( vi   = vigroup )
    AS_DEALLOCATE( vi   = vimailles )
    AS_DEALLOCATE( vk24 = vngroup )
    call jedema()
end subroutine
