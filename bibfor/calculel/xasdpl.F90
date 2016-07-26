subroutine xasdpl(celmod, prol0, chou)
    implicit none

    character(len=*) :: chou, celmod, prol0
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

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================

!     BUT : A PARTIR DE D'UN CHAM_ELGA CONTENANT UN CHAMP DE DEPLACEMENT
!           X-FEM DECOMPOSE (DX, HX, E1X, ...), CALCULER UN CHAMP DE
!           DEPLACMEMENT RECOMPOSE (DX, DY, DZ).
!     -----------------------------------------------------------------
!
! CHOU IN/JXOUT K19 : NOM DU CHAMP RESULTAT
! PROL0 IN   K3  :
!      /'OUI' : LE CHAM_ELEM CHOU EST PROLONGE
!       PAR DES VALEURS NULLES LA OU IL N'EST PAS DEFINI.
!      /'NON' : ERREUR <F> SI IL EXISTE DES
!       DES VALEURS DE CHOU QUI NE SONT PAS AFFECTEES DANS CHIN
! CELMOD IN/JXIN  K19 : NOM D'UN CHAM_ELEM "MODELE" SI TYPE='EL..'
!
#include "jeveux.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsces.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/exithm.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/manopg.h"
#include "asterfort/modat2.h"
#include "asterfort/nbelem.h"
#include "asterfort/nucalc.h"
#include "asterfort/teattr.h"
#include "asterfort/typele.h"
#include "asterfort/utmess.h"
#include "asterfort/xnpgxx.h"
#include "asterfort/rcmfmc.h"
!
    integer :: nboumx, nbinmx
    parameter   (nboumx=1, nbinmx=11)
    character(len=8) :: lpaout(nboumx), lpain(nbinmx)
    character(len=19) :: lchout(nboumx), lchin(nbinmx)
!
    integer :: nncp, iret, nbin, nbout
    integer :: iopt, iopt1, iopt2, nute, numc, numc2, igr, nbgrel
    integer :: imolo
    integer :: nel, jliel
    integer :: jcesd, jcesl, jcesddpl, jcesldpl
    integer :: jcesddpx, jcesldpx, jcesdnpg, jceslnpg
    integer :: jmofis, jnfiss
    integer :: nfismo, ndime, ndim, npg, nfiss
    integer :: ima, iadnpg, iaddpx, iaddpl, iad
    integer :: iel, kpg
    integer :: k, ifiss
    character(len=3) :: exixfm
    character(len=4) :: tychi
    character(len=8) :: ma, ma2, nomgd, param, mo
    character(len=8) :: noma, chmat, nomfis
    character(len=8), pointer :: typma(:) => null()
    character(len=16) :: option, nomte, enr, typdis
    character(len=19) :: chin, cns, mnoga, ligrel, cesout, cesdpl, mate
    character(len=19) :: chhno, chxpg, chdpx, chsdpx, chsnpg
    character(len=24) :: valk(4)
    aster_logical :: lxfem, ok, yathm, perman, lfiss
    integer, pointer :: cesvnpg(:) => null()
    real(kind=8), pointer :: cesvdpl(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    real(kind=8), pointer :: cesvdpx(:) => null()
!---------------------------------------------------------------------

    call jemarq()

!   -- recuperation du champ reel
    call getvid(' ', 'CHAM_GD', scal=chin, nbret=iret)

! 1- CALCUL DE:
!      MA    : MAILLAGE ASSOCIE A CHIN
!      TYCHI : TYPE DU CHAMP CHIN (CART/NOEU/ELNO/ELGA/CESE)
!      NOMGD : NOM DE LA GRANDEUR ASSOCIEE A CHIN
! ------------------------------------------------------------------
!
    call dismoi('NOM_MAILLA', chin, 'CHAMP', repk=ma)
    call dismoi('TYPE_CHAMP', chin, 'CHAMP', repk=tychi)
    call dismoi('NOM_GD', chin, 'CHAMP', repk=nomgd)

    if (tychi.ne.'NOEU') call utmess('F', 'XFEM_86')
    if (nomgd.ne.'DEPL_R') call utmess('F', 'XFEM_87')
!
! 2.  --  ON CREE UN CHAM_ELEM_S "MODELE" : CESMOD
!         LIGREL: NOM DU LIGREL ASSOCIE A CHOU
! ---------------------------------------------------------------
    ASSERT(celmod.ne.' ')
    call dismoi('NOM_LIGREL', celmod, 'CHAM_ELEM', repk=ligrel)
    call dismoi('NOM_MODELE', ligrel, 'LIGREL', repk=mo)
    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=ma2)

!   verification de la coherence entre le maillage sur lequel s'appuie
!   le champ d'entree et le maillage sur lequel s'appuie le modele
    if (ma .ne. ma2) then
        valk(1) = chin
        valk(2) = mo
        valk(3) = ma
        valk(4) = ma2
        call utmess('F', 'CALCULEL4_59', nk=4, valk=valk)
    endif
!
!   recuperation de la dimension geometrique du maillage
    call dismoi('DIM_GEOM', ma, 'MAILLAGE', repi=ndim)

!   on verfie que le modele comporte des elements X-FEM
    call dismoi('NB_FISS_XFEM', mo, 'MODELE', repi=nfismo)

    if (nfismo.eq.0) call utmess('F', 'XFEM_88')
!
!   on verifie que le modele n'est pas HM
    call exithm(mo, yathm, perman)

    if (yathm) call utmess('F', 'XFEM_89')
!
!   Le modèle comporte-t-il au moins une fissure et pas seulement des interfaces
    call jeveuo(mo//'.FISS','L',jmofis)
    call jeveuo(mo//'.NFIS','L',jnfiss)
    nfiss = zi(jnfiss)
!
    lfiss=.false.
    do ifiss = 1,nfiss
        nomfis = zk8(jmofis-1+ifiss)
        call dismoi('TYPE_DISCONTINUITE', nomfis, 'FISS_XFEM', repk=typdis)
!       
        if(typdis.eq.'FISSURE') then
            lfiss=.true.
            exit
        endif
    enddo
!
    mate=' '
    if (lfiss) then
       !   -- recuperation du champ de matériau
       call getvid(' ', 'CHAM_MATER', scal=chmat, nbret=iret)
       ! Assertion : si on a une fissure, le champ de matériau est fourni
       if (iret.eq.0) then 
         call utmess('F', 'XFEM_100')
       else
         call rcmfmc(chmat, mate)
       endif
    endif
!
!   creation du CHAM_ELEM_S pour stocker les deplacements recomposes
!   a partir du CHAM_ELEM modele
    cesout = '&&XASDPL.CESOUT'
    call celces(celmod, 'V', cesout)
!
    call dismoi('NOM_OPTION', celmod, 'CHAM_ELEM', repk=option)
    call dismoi('NOM_PARAM', celmod, 'CHAM_ELEM', repk=param)
!
    call jelira(ligrel//'.LIEL', 'NMAXOC', nbgrel)
!
    call jeveuo('&CATA.TE.TYPEMA', 'L', vk8=typma)

    call jenonu(jexnom('&CATA.OP.NOMOPT', 'XFEM_XPG'), iopt1)
    call jenonu(jexnom('&CATA.OP.NOMOPT', 'DEPL_XPG'), iopt2)
    call jenonu(jexnom('&CATA.OP.NOMOPT', option), iopt)

!   ouverture du CHAM_ELEM_S ou stocker les champs de deplacement recomposes
    call jeveuo(cesout//'.CESD', 'L', jcesd)
    call jeveuo(cesout//'.CESL', 'E', jcesl)
    call jeveuo(cesout//'.CESV', 'E', vr=cesv)

!   conversion du champ de deplacement passe en entree en CHAM_ELEM_S : NOEU -> ELGA
    cns = '&&XASDPL.CNS'
    cesdpl = '&&XASDPL.DEPL'
!
    call cnocns(chin, 'V', cns)
!   interpolation du champ de deplacement aux points de Gauss
    mnoga = '&&XASDPL.MNOGA'
    call manopg(ligrel, option, param, mnoga)
    call cnsces(cns, 'ELGA', cesout, mnoga, 'V',&
                cesdpl)

    call detrsd('CHAM_NO_S', cns)
    call detrsd('CHAM_ELEM_S', mnoga)
    
!   ouverture du CHAM_ELEM_S ou lire les champs de deplacement passes en entree
    call jeveuo(cesdpl//'.CESD', 'L', jcesddpl)
    call jeveuo(cesdpl//'.CESL', 'L', jcesldpl)
    call jeveuo(cesdpl//'.CESV', 'L', vr=cesvdpl)

!   calcul du nombre de point de Gauss dans la famille XFEM,
!   pour chaque element XFEM 
    chsnpg='&&XASDPL.CHSNPG'
    call xnpgxx(ligrel, option, param, chsnpg, exixfm)
!   
!   verification paranoiaque
    ASSERT(exixfm.eq.'OUI')
!
    call jeveuo(chsnpg//'.CESD', 'L', jcesdnpg)
    call jeveuo(chsnpg//'.CESL', 'L', jceslnpg)
    call jeveuo(chsnpg//'.CESV', 'L', vi=cesvnpg)

!     3. calcul des coordonnees des points de Gauss de la famille XFEM dans
!        l'element de reference :
!     ------------------------------------------------------------------
    chxpg='&&XASDPL.CHXPG'
    lpain(1) = 'PGEOMER'
    lchin(1) = ma//'.COORDO'
    lpain(2) = 'PPINTTO'
    lchin(2) = mo//'.TOPOSE.PIN'
    lpain(3) = 'PCNSETO'
    lchin(3) = mo//'.TOPOSE.CNS'
    lpain(4) = 'PHEAVTO'
    lchin(4) = mo//'.TOPOSE.HEA'
    lpain(5) = 'PLONCHA'
    lchin(5) = mo//'.TOPOSE.LON'
    lpain(6) = 'PPMILTO'
    lchin(6) = mo//'.TOPOSE.PMI'
    nbin=6
    lpaout(1) = 'PXFGEOM'
    lchout(1) = chxpg
    nbout=1
!
    call calcul('S', 'XFEM_XPG', ligrel, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')

!     4. calcul du champ de deplacement recompose aux points de Gauss des
!        sous-elements pour les elements X-FEM
!     ------------------------------------------------------------------
    chdpx='&&XASDPL.CHDPX'
    lpain(1) = 'PDEPLNO'
    lchin(1) = chin
    lpain(2) = 'PXFGEOM'
    lchin(2) = chxpg
    lpain(3) = 'PHEAVTO'
    lchin(3) = mo//'.TOPOSE.HEA'
    lpain(4) = 'PLONCHA'
    lchin(4) = mo//'.TOPOSE.LON'
    lpain(5) = 'PBASLOR'
    lchin(5) = mo//'.BASLOC'
    lpain(6) = 'PLSN'
    lchin(6) = mo//'.LNNO'
    lpain(7) = 'PLST'
    lchin(7) = mo//'.LTNO'
    nbin=7
!   si le champ TOPOSE.HNO existe dans le modele, on l'ajoute a la liste
!   des champs IN
    chhno=mo//'.TOPONO.HNO'
    call exisd('CHAM_ELEM', chhno, iret)
    if (iret.eq.1) then
       lpain(8) = 'PHEA_NO'
       lchin(8) = chhno
       nbin=8
    endif
    lpain(9) = 'PSTANO'
    lchin(9) = mo//'.STNO'
    lpain(10) = 'PMATERC'
    lchin(10) = mate
    lpain(11) = 'PGEOMER'
    lchin(11) = ma//'.COORDO'
    nbin=11
    lpaout(1) = 'PDEPLPG'
    lchout(1) = chdpx
    nbout=1
!
    call calcul('S', 'DEPL_XPG', ligrel, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
!
    chsdpx='&&XASDPL.CHSDGX'
    call celces(chdpx, 'V', chsdpx)
!
    call jeveuo(chsdpx//'.CESD', 'L', jcesddpx)
    call jeveuo(chsdpx//'.CESL', 'L', jcesldpx)
    call jeveuo(chsdpx//'.CESV', 'L', vr=cesvdpx)
!
!     5. stockage du champ de depalcement en chaque point de Gauss :
!           - le champ recompose pour les elements X-FEM
!           - le champ interpole pour les elements FEM
!     ------------------------------------------------------------------

!   boucle sur les groupes d'elements
    do igr = 1, nbgrel
        nel = nbelem(ligrel,igr)
        call jeveuo(jexnum(ligrel//'.LIEL', igr), 'L', jliel)
        nute = typele(ligrel,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', nute), nomte)
!
        lxfem=.false.
!
!       type de maille associe au type d'element
        noma=typma(nute)
!       recuperationn de la dimension topologique de la maille
        call dismoi('DIM_TOPO', noma, 'TYPE_MAILLE', repi=ndime)
!
!       si l'element est un element de bord, on passe au groupe d'element
!       suivant
        if (ndime.lt.ndim) cycle
!
!       calcul de l'identifiant du mode local associe au parametre
!       PDEPL_R, pour le calcul de l'option TOU_INI_ELGA (cf. la
!       construction du cham_elem modele dans op0195)
        imolo = modat2(iopt,nute,param)
!
!       Assertion: l'element sait calculer PDEPL_R, avec l'option
!                  TOU_INI_ELGA
        ASSERT(imolo.ne.0)
!
!       l'element est-il XFEM ?
        call teattr('C', 'XFEM', enr, iret, nomte)
        lxfem=iret.eq.0
!
!       L'ELEMENT SAIT-IL CALCULER XFEM_XPG ?
        numc = nucalc(iopt1,nute,1)
!       L'ELEMENT SAIT-IL CALCULER DEPL_XPG ?
        numc2 = nucalc(iopt2,nute,1)
!
!       assertion : un element xfem --- qui n'est pas un element de bord ---
!       sait calculer les options XFEM_XPG et DEPL_XPG
        ok=.not.lxfem.or.(numc.gt.0.and.numc2.gt.0)
        ASSERT(ok)
!
!       si l'element est X-FEM (et sait calculer XFEM_XPG et DEPL_XPG)
        if (lxfem) then
!          boucle sur les elements du groupe
           do iel = 1, nel
               ima=zi(jliel-1+iel)
               if (ima .lt. 0) cycle
!              
!              recuperation du nombre de point de Gauss de l'element 
               call cesexi('C', jcesdnpg, jceslnpg, ima, 1, 1, 1, iadnpg)
               ASSERT(iadnpg.gt.0)
               npg=cesvnpg(iadnpg)
!
!              boucle sur les points de Gauss
               do kpg=1, npg
!                 recopie du champ de deplacement recompose au point de Gauss courant
!                 dans le champ de sortie
                  do k=1, ndim
!                    recuperation de la composante courante, au point de Gauss courant,
!                    du champ de deplacement recompose
                     call cesexi('C', jcesddpx, jcesldpx, ima, kpg, 1, k, iaddpx)
                     ASSERT(iaddpx.gt.0)
!                    stockage dans le champ de sortie de la composante courante, au
!                    point de Gauss courant, du champ de deplacement recompose
                     call cesexi('C', jcesd, jcesl, ima, kpg, 1, k, iad)
                     iad=abs(iad)
                     zl(jcesl-1+iad)=.true.
                     cesv(iad)=cesvdpx(iaddpx)
                  enddo
               enddo
           enddo
!       si l'element est FEM
        else
!          recopie du champ de deplacement de l'element dans le champ de sortie
           do iel = 1, nel
               ima=zi(jliel-1+iel)
               if (ima .lt. 0) cycle
!
!              recuperation du nombre de point de Gauss de l'element (comme le nombre de point
!              sur lesquels le champ ELGA est defini)
               npg=zi(jcesddpl-1+5+4*(ima-1)+1)
!
!              boucle sur les points de Gauss de l'element            
               do kpg=1, npg
!                 recopie du champ de deplacement recompose au point de Gauss courant
!                 dans le champ de sortie
                  do k=1, ndim
!                    recuperation de la composante courante, au point de Gauss courant,
!                    du champ de deplacement interpole aux points de Gauss
                     call cesexi('C', jcesddpl, jcesldpl, ima, kpg, 1, k, iaddpl)
                     ASSERT(iaddpl.gt.0)
!                    stockage dans le champ de sortie de la composante courante, au
!                    point de Gauss courant, du champ de deplacement interpole
                     call cesexi('C', jcesd, jcesl, ima, kpg, 1, k, iad)
                     iad=abs(iad)
                     zl(jcesl-1+iad)=.true.
                     cesv(iad)=cesvdpl(iaddpl)
                  enddo
               enddo
           enddo
        endif
    enddo
!
!   -- cpnversion du CHAM_ELEM_S recompose en CHAM_ELEM
    call cescel(cesout, ligrel, option, param, prol0,&
                nncp, 'G', chou, 'F', iret)
!
!     -- MENAGE :
!     ------------
    call detrsd('CHAMP', chxpg)
    call detrsd('CHAMP', chdpx)
!
    call detrsd('CHAM_ELEM_S', cesout)
    call detrsd('CHAM_ELEM_S', cesdpl)
    call detrsd('CHAM_ELEM_S', chsnpg)
    call detrsd('CHAM_ELEM_S', chsdpx)
!
    call jedema()

end subroutine
