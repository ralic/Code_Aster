subroutine xnpgxx(ligrel, option, param, chsnpg, exixfm)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: patrick.massin at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/indk32.h"
#include "asterfort/ismali.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/modat2.h"
#include "asterfort/nbelem.h"
#include "asterfort/nucalc.h"
#include "asterfort/typele.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: ligrel, chsnpg
    character(len=16) :: option
    character(len=8) :: param
    character(len=3) :: exixfm
! ------------------------------------------------------------------
! BUT: REGARDER DANS LE LIGREL S'IL Y A DES ELEMENTS XFEM
!      ET SI LE CHAMP ELGA (OPTION/PARAM) UTILISE UNE FAMILLE XFEM..
!      SI FAMILLE XFEM :
!        * CALCULER LE CHAMP SIMPLE CHSNPG CONTENANT LE NOMBRE DE
!          POINTS DE GAUSS DES FAMILLES XFEM...
! ------------------------------------------------------------------
!     ARGUMENTS:
!     ----------
! LIGREL  IN/JXIN  K19 : LIGREL
! OPTION,PARAM  IN  K* : OPTION ET PARAMETRE PERMETTANT DE DETERMINER
!                        LA FAMILLE DE PG UTILISEE.
! EXIXFM  OUT K3 : 'OUI' : IL EXISTE DES GRELS AVEC FAMILLE XFEM...
!                  'NON' SINON
! CHSNPG  IN/JXOUT K19 : CHAM_ELEM_S (NEUT_R) DE TYPE 'ELEM', CONTENANT
!                        LE NOMBRE DE POINTS DE GAUSS DE LA FAMILLE 'XFEM'
!
! REMARQUE :
!   L'OBJET CHSGEO N'EST CREE QUE SI EXIXFM='OUI'
! ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!
    integer :: nbflmx
    parameter (nbflmx=20)
    character(len=8) :: lifapg(nbflmx)
!
    integer :: iopt, iopt1, nute, numc, igr, nbgrel
    integer :: imolo, jmolo, nec, kfpg, kfam
    integer :: igd,  nblfpg,  nbfam, nel, jliel, jfpgl, jcesdlon, jcesllon, jcesd, jcesl
    integer :: nfiss, ndime, irese, nspg, nse, npg
    integer :: ima, iadlon, iad, iel
    integer :: k, nuflpg, nufgpg
    character(len=8) :: nomgd, elrese(6), elrefe, ma, mo, famil, noma
    character(len=8), pointer :: typma(:) => null()
    character(len=16) :: nofpg, nomte
    character(len=19) :: chslon
    character(len=24) :: chlong
    character(len=32) :: noflpg
    character(len=32), pointer :: pnlocfpg(:) => null()
    integer, pointer :: nolocfpg(:) => null()
    integer, pointer :: tmfpg(:) => null()
    integer, pointer :: cesvlon(:) => null()
    integer, pointer :: cesv(:) => null()
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','T10'/
!     ------------------------------------------------------------------
    call jemarq()
!
    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=ma)
    call dismoi('NOM_MODELE', ligrel, 'LIGREL', repk=mo)

    exixfm='NON'
!   le modele comporte-t-il des elements X-FEM ?
    call dismoi('NB_FISS_XFEM', mo, 'MODELE', repi=nfiss)
!   si le modele ne comporte pas d'elements X-FEM, on a fini
    if (nfiss.eq.0) then
        goto 999
    endif

    call jelira(ligrel//'.LIEL', 'NMAXOC', nbgrel)
!
    call jeveuo('&CATA.TE.PNLOCFPG', 'L', vk32=pnlocfpg)
    call jelira('&CATA.TE.NOLOCFPG', 'LONMAX', nblfpg)
    call jeveuo('&CATA.TE.NOLOCFPG', 'L', vi=nolocfpg)
!
    call jeveuo('&CATA.TM.TMFPG', 'L', vi=tmfpg)
    call jeveuo('&CATA.TE.TYPEMA', 'L', vk8=typma)
!
! -- construction d'un champ simple pour parcourir PLONCHA
    chlong=mo//'.TOPOSE.LON'
    chslon='&&XNPGSE.CHSLON'
    call celces(chlong, 'V', chslon)
!
    call jeveuo(chslon//'.CESD', 'L', jcesdlon)
    call jeveuo(chslon//'.CESL', 'L', jcesllon)
    call jeveuo(chslon//'.CESV', 'L', vi=cesvlon)
!
! -- allocation du CHAM_ELEM_S chsnpg
    call cescre('V', chsnpg, 'ELEM', ma, 'NEUT_I',&
                0, ' ', [-1], [-1], [-1])
!
    call jeveuo(chsnpg//'.CESD', 'L', jcesd)
    call jeveuo(chsnpg//'.CESL', 'E', jcesl)
    call jeveuo(chsnpg//'.CESV', 'E', vi=cesv)
!
!     1. CALCUL DE EXIXFM :
!     ------------------------------------------------------------------
    call jenonu(jexnom('&CATA.OP.NOMOPT', 'XFEM_XPG'), iopt1)
    call jenonu(jexnom('&CATA.OP.NOMOPT', option), iopt)
    do igr = 1, nbgrel
        nel = nbelem(ligrel,igr)
        call jeveuo(jexnum(ligrel//'.LIEL', igr), 'L', jliel)
        nute = typele(ligrel,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', nute), nomte)
!
!       L'ELEMENT SAIT-IL CALCULER XFEM_XPG ?
        numc = nucalc(iopt1,nute,1)
        if (numc .lt. 0) cycle
!
        imolo = modat2(iopt,nute,param)
        if (imolo .eq. 0) cycle
!
        call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
        igd = zi(jmolo-1+2)
        call jenuno(jexnum('&CATA.GD.NOMGD', igd), nomgd)
        call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
        kfpg = zi(jmolo-1+4+nec+1)
!
!       -- FAMILLE "LISTE"
        if (kfpg .lt. 0) then
!          FAMILLE "LISTE" :
            call jelira(jexnum('&CATA.TE.FPG_LISTE', -kfpg), 'LONMAX', nbfam)
            nbfam=nbfam-1
            call jeveuo(jexnum('&CATA.TE.FPG_LISTE', -kfpg), 'L', jfpgl)
            elrefe=zk8(jfpgl-1+nbfam+1)
            do k = 1, nbfam
                noflpg = nomte//elrefe//zk8(jfpgl-1+k)
                nuflpg = indk32(pnlocfpg,noflpg,1,nblfpg)
                nufgpg = nolocfpg(nuflpg)
                call jenuno(jexnum('&CATA.TM.NOFPG', nufgpg), nofpg)
                lifapg(k)=nofpg(9:16)
            end do
!
!       -- FAMILLE "ORDINAIRE"
        else
            nbfam=1
            call jenuno(jexnum('&CATA.TM.NOFPG', kfpg), nofpg)
            lifapg(1)=nofpg(9:16)
        endif
!
!       --BOUCLE SUR LA/LES FAMILLE(S) :
        do kfam = 1, nbfam
            famil=lifapg(kfam)
!
            if (famil(1:4) .eq. 'XFEM') then
                exixfm='OUI'
!
!               RECHERCHE DE LA FAMILLE XINT
!
!               type de maille associe au type d'element
                noma=typma(nute)
!               recuperationn de la dimension topologique de la maille
                call dismoi('DIM_TOPO', noma, 'TYPE_MAILLE', repi=ndime)
!               calcul du decalage a appliquer si la maille est quadratique
                if (.not.ismali(noma)) then
                   irese=3
                else
                   irese=0
                endif
!
!               construction du nom de la famille de points de Gauss
!               pour le sous-element
                noflpg = nomte//elrese(ndime+irese)//'XINT'
!
!               recherche de cette famille dans la liste des familles
!               de points de Gauss
                nuflpg = indk32(pnlocfpg,noflpg,1,nblfpg)
!
!               Assertion : on a trouve la famille
                ASSERT(nuflpg.ne.0)

!               recuperation du nombre de point Gauss de la famille XINT,
!               i.e. du nombre de point de Gauss par sous-element
                nufgpg = nolocfpg(nuflpg)
                nspg=tmfpg(nufgpg)
!
!               calcul du nombre de points de Gauss pour chaque element
!               du groupe d'elements
                do iel = 1, nel
                    ima=zi(jliel-1+iel)
                    if (ima .lt. 0) cycle
!
!                   recuperation du nombre de sous-element de l'element
                    call cesexi('C', jcesdlon, jcesllon, ima, 1, 1, 1, iadlon)
                    ASSERT(iadlon.gt.0)
                    nse=cesvlon(iadlon)

!                   calcul du nombre de points de Gauss de l'element
                    npg=nse*nspg

!                   stockage du nombre de points de Gauss de l'element
                    call cesexi('C', jcesd, jcesl, ima, 1, 1, 1, iad)
                    iad=abs(iad)
                    zl(jcesl-1+iad)=.true.
                    cesv(iad)=npg
                enddo
            endif
        end do
    end do
!
    call detrsd('CHAM_ELEM_S', chslon)
!
999 continue
    call jedema()
end subroutine
