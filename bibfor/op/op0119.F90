subroutine op0119()
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
#include "asterfort/cargeo.h"
#include "asterfort/codent.h"
#include "asterfort/gcncon.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/gfmaco.h"
#include "asterfort/gfmacr.h"
#include "asterfort/gfmafi.h"
#include "asterfort/gfmagr.h"
#include "asterfort/gfmama.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedetr.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/pmfsce.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
    integer :: nbvfibre, ncarfi, itrois,maxfibre
    parameter  (ncarfi=3,itrois=3)
!
    integer :: iret, ifm, niv, nboccs, nboccp, nufib, nbfigr
    integer :: nbfib, nmails, nttri3, ntseg2, ntqua4, nunoeu
    integer :: nbv, jdtm, jms, nummai, nutyma, nbnoeu, nbnoma, nbnomm, nbno
    integer :: nbgf, jpo, ioc, i, j, ipos, in, nno, no, jcf, jdno, jdco, jnfg
    integer :: ibid, ipoint, ig, numf, jngfma
!
    real(kind=8) :: pi4, dtrois, zero
    parameter  (pi4=0.7853981633974483d+0,dtrois=3.d+0,zero=0.d+0)
    real(kind=8) :: x(4), y(4), centre(2), axep(2), surf

    real(kind=8),pointer :: valfibre(:) => null()
!
    character(len=8) :: sdgf, nomas, ktyma, ksudi, nommai, nogfma
    character(len=6) :: knbv, kioc, knumai
    character(len=16) :: concep, cmd, limcls(3), ltymcl(3)
    character(len=24) :: mlgtms, mlgcnx, mlgcoo, mlgtma, mlgtno, nomgf
    character(len=24) :: vnbfig, vcafig, vpocfg, rnomgf, gfmagl, valk(3)
!
    data limcls/'MAILLE_SECT','GROUP_MA_SECT','TOUT_SECT'/
    data ltymcl/'MAILLE','GROUP_MA','TOUT'/
! --- ------------------------------------------------------------------
!
    call jemarq()
    iret=0
! --- ------------------------------------------------------------------
! --- RECUPERATION DES ARGUMENTS  DE LA COMMANDE
    call getres(sdgf, concep, cmd)
!
! --- ------------------------------------------------------------------
! --- NOMBRE DES GROUPES DE FIBRES POUR DIMENSIONNER LES OBJETS
    call getfac('SECTION', nboccs)
    call getfac('FIBRE', nboccp)
    nbgf=nboccs+nboccp
!
! --- ------------------------------------------------------------------
! --- SD GEOM_FIBRE
!        NOMS DES GROUPES DE FIBRES (REPERTOIRE DE NOMS)
!        NOMBRE DE FIBRES PAR GROUPE
!        CARACTERISTIQUES DE FIBRES (TOUT A LA SUITE EN 1 SEUL VECTEUR)
!        POINTEUR POUR LES CARACTERISTIQUES POUR FACILITER LES ACCES
!        NOM DU MAILLAGE GLOBAL DES GROUPES DE FIBRES
!
    rnomgf = sdgf//'.NOMS_GROUPES'
    vnbfig = sdgf//'.NB_FIBRE_GROUPE'
    vcafig = sdgf//'.CARFI'
    vpocfg = sdgf//'.POINTEUR'
    gfmagl = sdgf//'.GFMA'
!
!
    call jecreo(rnomgf, 'G N K24')
    call jeecra(rnomgf, 'NOMMAX', nbgf, ' ')
!
    call wkvect(vnbfig, 'G V I', nbgf, jnfg)
    call wkvect(vpocfg, 'G V I', nbgf, jpo)
    call wkvect(gfmagl, 'G V K8', 1, jngfma)
!
! --- ------------------------------------------------------------------
! --- CREATION DU NOM DU MAILLAGE GLOBAL
    call gcncon('_', nogfma)
    zk8(jngfma) = nogfma
!
! --- ------------------------------------------------------------------
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infmaj()
    call infniv(ifm, niv)
!
! --- ------------------------------------------------------------------
! --- RECUPERATION DES NUMEROS DES TYPES MAILLES TRI3,QUA4
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3'), nttri3)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD4'), ntqua4)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'), ntseg2)
!
! comptage du nombre de fibres total et de noeuds et de mailles
    nbfib = 0
    nbnoeu=0
    nbnoma=0
!
    do ioc = 1, nboccs
        nbfigr=0
! ---    LES SECTIONS MAINTENANT
        call getvid('SECTION', 'MAILLAGE_SECT', iocc=ioc, scal=nomas, nbret=nbv)
        mlgtms = nomas//'.TYPMAIL'
        mlgcnx = nomas//'.CONNEX'
        mlgtno = nomas//'.NOMNOE'
! ---    VERIFICATION MAILLES TRIA3 ET QUAD4 UNIQUEMENT
        call jeveuo(mlgtms, 'L', jdtm)
! ---    NOMBRE DE FIBRES = NOMBRE DE MAILLES CONCERNEES
        call reliem(' ', nomas, 'NU_MAILLE', 'SECTION', ioc,&
                    3, limcls, ltymcl, '&&PMFD00.MAILLSEC', nmails)
        nbfigr=nmails
        call jeveuo('&&PMFD00.MAILLSEC', 'L', jms)
!        RECUPERATION DE LA TAILLE DE NOMAS//'.CONNEX'
        call jelira(mlgcnx, 'LONT', nbnomm)
!        RECUPERATION DU NOMBRE DE NOEUX DU MAILLAGE
        call jelira(mlgtno, 'NOMMAX', nbno)
        nbnoeu=nbnoeu+nbno
!
        do j = 1, nmails
            nummai = zi(jms+j-1)
            nutyma = zi(jdtm+nummai-1)
            if (nutyma .ne. ntseg2) then
                if (nutyma .ne. nttri3 .and. nutyma .ne. ntqua4) then
                    call codent(nummai, 'G', knumai)
                    call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), ktyma)
                    valk(1)=nomas
                    valk(2)=knumai
                    valk(3)=ktyma
                    call utmess('F', 'MODELISA6_27', nk=3, valk=valk)
                endif
            else
! ---          ON DEDUIT LES SEG2 DU NB DE FIBRES
                nbfigr=nbfigr -1
                nbnomm=nbnomm-2
            endif
        enddo
        nbfib = nbfib + nbfigr
        nbnoma=nbnoma+nbnomm
        zi(jnfg-1+ioc)=nbfigr
    enddo
!
    maxfibre = 10
    do ioc = 1, nboccp
!       NOMBRE DE FIBRES PONCTUELLES
        call getvr8('FIBRE', 'VALE', iocc=ioc, nbval=0, nbret=nbvfibre)
        nbvfibre = -nbvfibre
        maxfibre = max(maxfibre,nbvfibre)
!       VERIF MULTIPLE DE 3 POUR 'VALE' DANS 'FIBRE'
        if (dble(nbvfibre)/dtrois .ne. nbvfibre/itrois) then
            call getvtx('FIBRE', 'GROUP_FIBRE', iocc=ioc, scal=nomgf, nbret=ibid)
            call codent(ioc, 'G', kioc)
            call codent(nbvfibre, 'G', knbv)
            valk(1)=nomgf
            valk(2)=knbv
            call utmess('F', 'MODELISA6_26', nk=2, valk=valk)
        else
            nbfib = nbfib + nbvfibre/itrois
            nbnoeu = nbnoeu + nbvfibre/itrois
            nbnoma = nbnoma + nbvfibre/itrois
            zi(jnfg-1+nboccs+ioc)= nbvfibre/itrois
        endif
    enddo
!   CREATION DES ATTIBUTS DE MAILLAGE DE NOGFMA
    call gfmacr(nogfma, nbfib, nbnoeu, nbnoma, nbgf)
!
! --- ------------------------------------------------------------------
! --- VECTEUR DE LA SD GEOM FIBRES (CARFI)
    call wkvect(vcafig, 'G V R', nbfib*ncarfi, jcf)
    ipoint = 1
    ig = 0
    numf = 0
! --- ------------------------------------------------------------------
! --- TRAITEMENT DES SECTIONS
    nbnoeu=0
    nufib = 0
    do ioc = 1, nboccs
        ig=ig+1
        call getvtx('SECTION', 'GROUP_FIBRE', iocc=ioc, scal=nomgf, nbret=ibid)
! ---    CREATION DU GROUPE DE MAILLE
        call gfmagr(nogfma, nomgf, zi(jnfg+ig-1))
        if (niv .eq. 2) write(ifm,800) nomgf
! ---    ON RECUPERE LE MAILLAGE
        call getvid('SECTION', 'MAILLAGE_SECT', iocc=ioc, scal=nomas, nbret=nbv)
! ---    RECUPERATION DES COORDONNEES DE L'AXE DE LA POUTRE
        call getvr8('SECTION', 'COOR_AXE_POUTRE', iocc=ioc, nbval=2, vect=axep,&
                    nbret=iret)
        if (iret .ne. 2) then
            axep(1) = zero
            axep(2) = zero
        endif
! ---    RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MAILLAGE ASSOCIE
        mlgtms = nomas//'.TYPMAIL'
        mlgcnx = nomas//'.CONNEX'
        mlgcoo = nomas//'.COORDO    .VALE'
        mlgtma = nomas//'.NOMMAI'
        mlgtno = nomas//'.NOMNOE'
! ---    RECUPERATION DES ADRESSES JEVEUX UTILES
        call jeveuo(mlgtms, 'L', jdtm)
        call jeveuo(mlgcoo, 'L', jdco)
! ---    ON RECUPERE LES MAILLES DE LA SECTION CONCERNEES
        call reliem(' ', nomas, 'NU_MAILLE', 'SECTION', ioc,&
                    3, limcls, ltymcl, '&&OP0119.MAILLSEC', nmails)
        call jeveuo('&&OP0119.MAILLSEC', 'L', jms)
        call jelira(mlgtno, 'NOMMAX', nbno)
! ---    COPIE DES COORDONNEES DES NOEUDS DU MAILLAGE DU GROUPE
        call gfmaco(nogfma, nbnoeu, nbno, jdco, axep)
        nbfib = 0
        do 70 j = 1, nmails
            nummai = zi(jms+j-1)
! ---       COORDONNEES NOEUDS
            nutyma = zi(jdtm+nummai-1)
            if (nutyma .eq. ntseg2) goto 70
            nbfib = nbfib + 1
            nufib = nufib + 1
            call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
            nno = 3
            if (nutyma .eq. ntqua4) nno = 4
! ---       COPIE DE LA MAILLE
            call gfmama(nogfma, nufib, nutyma, jdno, nttri3,&
                        ntqua4, nbnoeu, nomgf, nbfib)
            do in = 1, nno
                no = zi(jdno-1+in)
                x(in) = zr(jdco+ (no-1)*3) - axep(1)
                y(in) = zr(jdco+ (no-1)*3+1) - axep(2)
            enddo
! ---       SURFACE ET CENTRE
            call pmfsce(nno, x, y, surf, centre)
! ---       STOCKAGE DES CARACTERISTIQUES DE FIBRES DANS
            ipos = jcf + ipoint - 1 + ncarfi* (nbfib-1)
            zr(ipos) = centre(1)
            zr(ipos+1) = centre(2)
            zr(ipos+2) = surf
            if (niv .eq. 2) then
                numf = numf + 1
                call jenuno(jexnum(mlgtma, nummai), nommai)
                if (nno .eq. 3) then
                    write (ifm,801) numf,nommai,'TRIA3',centre,surf
                else
                    write (ifm,801) numf,nommai,'QUAD4',centre,surf
                endif
            endif
70      continue
        call jecroc(jexnom(rnomgf, nomgf))
        zi(jpo+ig-1)=ipoint
        ipoint = ipoint + nbfib*ncarfi
        nbnoeu=nbnoeu+nbno
    enddo
!
! --------------------------------------------------------------------------------------------------
!   traitement des fibres
    call wkvect('&&OP0119.VFIBRE', 'V V R', maxfibre, vr=valfibre)
    do ioc = 1, nboccp
        ig=ig+1
        call getvtx('FIBRE', 'GROUP_FIBRE', iocc=ioc, scal=nomgf, nbret=ibid)
!       CREATION DU GROUPE DE MAILLE
        call gfmagr(nogfma, nomgf, zi(jnfg+ig-1))
        if (niv .eq. 2) write (ifm,820) nomgf
!       SURFACE OU DIAMETRE
        call getvtx('FIBRE', 'CARA', iocc=ioc, scal=ksudi, nbret=iret)
        if (iret .eq. 0) ksudi = 'SURFACE '
        call getvr8('FIBRE', 'VALE', iocc=ioc, nbval=maxfibre, vect=valfibre, &
                    nbret=nbvfibre)
!       RECUPERATION DES COORDONNEES DE L'AXE DE LA POUTRE
        call getvr8('FIBRE', 'COOR_AXE_POUTRE', iocc=ioc, nbval=2, vect=axep,&
                    nbret=iret)
        if (iret .ne. 2) then
            axep(1) = zero
            axep(2) = zero
        endif
! ---   CHANGER DIAMETRE EN SURFACE LE CAS ECHEANT
        nbfib = 0
        do i = 1, nbvfibre/3
            if (ksudi .eq. 'DIAMETRE') then
                surf = valfibre(3*i)*valfibre(3*i)*pi4
            else
                surf = valfibre(3*i)
            endif
            if (iret .eq. 2) then
                centre(1) = valfibre(3*i-2) - axep(1)
                centre(2) = valfibre(3*i-1) - axep(2)
            else
                centre(1) = valfibre(3*i-2)
                centre(2) = valfibre(3*i-1)
            endif
            nbfib = nbfib + 1
            nufib = nufib +1
            nunoeu = nbnoeu+i
            call gfmafi(nogfma, nufib, centre, nunoeu, nomgf,&
                        nbfib)
!
! ---       STOCKAGE DES CARACTERISTIQUES DE FIBRES DANS
            ipos = jcf + ipoint - 1 + ncarfi* (i-1)
            zr(ipos) = centre(1)
            zr(ipos+1) = centre(2)
            zr(ipos+2) = surf
            if (niv .eq. 2) then
                numf = numf + 1
                write (ifm,821) numf,centre,surf
            endif
        enddo
        nbnoeu=nbnoeu+nbvfibre/3
        call jecroc(jexnom(rnomgf, nomgf))
        zi(jnfg+ig-1)=nbfib
        zi(jpo+ig-1)=ipoint
        ipoint = ipoint + nbfib*ncarfi
    enddo
!
!
! --- CARACTERISTIQUES GEOMETRIQUES :
!     -----------------------------
    call cargeo(nogfma)
!
800 format(//,'DETAIL DES FIBRES SURFACIQUES DU GROUPE "',a24,'"',&
           /,'NUMF  MAILLE    TYPE        Y        ',&
             '     Z            SURF')
801 format(i4,2x,a8,2x,a5,3(2x,1pe12.5))
!
820 format(//,'DETAIL DES FIBRES PONCTUELLES DU GROUPE "',a24,'"',&
            /,'NUMF       Y             Z            SURF')
821 format(i4,3(2x,1pe12.5))
!
    call jedetr('&&OP0119.VFIBRE')
    call jedema()
end subroutine
