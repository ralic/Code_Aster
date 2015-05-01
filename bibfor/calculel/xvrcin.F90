subroutine xvrcin(ligmex, celthx, nomevo, nomsym, celmex, l_xfem)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/alchml.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/exixfe.h"
#include "asterfort/indk32.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/typele.h"
    character(len=8), intent(in) :: nomevo
    character(len=16), intent(in) :: nomsym
    character(len=19), intent(in) :: ligmex, celthx, celmex
    aster_logical, intent(out) :: l_xfem
! ----------------------------------------------------------------------
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
! person_in_charge: sam.cuvilliez at edf.fr
! ----------------------------------------------------------------------
!
!     -> cas particulier du chainage thermo-mecanique avec X-FEM
!
!     gestion dans vrcin1 du champ de température variable de commande
!     qui est ELGA sur les elements enrichis
!
!     but : determiner si on se trouve dans le cas du chainage 
!           thermo-mecanique avec X-FEM, et redefinir le champ de
!           temperature TEMP_ELGA, initialement defini sur le ligrel
!           du modele thermique enrichi, sur le ligrel du modele 
!           mecanique enrichi
!
!     in    ligmex : nom du ligrel '.MODELE' du modele
!     in    celthx : nom du cham_elem defini sur le ligrel thermique
!     in    nomevo : nom du resultat
!     in    nomsym : nom symbolique du champ
!     "out" celmex : nom du cham_elem defini sur le ligrel mecanique
!     out   l_xfem : booleen, vrai si chainage thermo-mecanique xfem
!
! ----------------------------------------------------------------------
    integer :: iret, nbma, ima, i
    integer :: jcelkm, jcelvm, jcelkt, jcelvt
    integer :: igrm, nbgrm, debgrm, nbelm, imolom, lgcatm, alielm, ielm
    integer :: nutem, amolom, lgelm, advelm
    integer :: igrt, nbgrt, debgrt, nbelt, imolot, lgcatt, alielt, ielt
    integer :: nutet, amolot, lgelt, advelt
    integer :: ival, igdm, nec, jpnlfp, jnolfp, kfpgm, nbfamm, adfpgm
    integer :: nuflpg, nufgpg, nblfpg, nbpg_x, nbpg_n
    integer :: nbscal_t, nb_pt_t, nbmcp
    real(kind=8) :: r8nan
    character(len=8) :: nomgdm, elrefm
    character(len=8) :: modevo, noma
    character(len=16) :: ktyelt, ktyelm
    character(len=19) :: ligthx
    character(len=24) :: nomolt, nomolm, nofpglism
    character(len=32) :: noflpg
    integer, pointer :: celdm(:) => null()
    integer, pointer :: celdt(:) => null()
    integer, pointer :: repet(:) => null()
    integer, pointer :: vecma_th(:) => null()
    integer, pointer :: vecma_me(:) => null()
    integer, pointer :: tmfpg(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
!
    l_xfem = .false.
!
    r8nan = r8nnem()
!
! ----------------------------------------------------------------------
! - verifications prealables
! ----------------------------------------------------------------------
!
!   on sort si la sd resultat n'existe pas
    call exisd('RESULTAT', nomevo, iret)
    if (iret .eq. 0) goto 999
!
!   on sort si la sd resultat ne contient pas de sd modele
    call dismoi('NOM_MODELE', nomevo, 'RESULTAT', repk=modevo)
    call exisd('MODELE', modevo, iret)
    if (iret .eq. 0) goto 999
!
!   on sort si ce modele nest pas xfem
    call exixfe(modevo, iret)
    if (iret .eq. 0) goto 999
!
    l_xfem = .true.
    ASSERT(nomsym .eq. 'TEMP_ELGA')
!
! ----------------------------------------------------------------------
! - on redefinit le cham_elem ELGA 'TEMP_ELGA' sur le ligrel ligmex
! - (initialement defini sur le ligrel ligthx)
! ----------------------------------------------------------------------
!
!   recuperation du nom ligthx, du nom du maillage associe 
!   et du nombre de maille
    call dismoi('NOM_LIGREL', celthx, 'CHAM_ELEM', repk=ligthx)
    call dismoi('NOM_MAILLA', ligthx, 'LIGREL', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
!
!   allocation de celmex
    call alchml(ligmex, 'TOU_INI_ELGA', 'PTEMP_R', 'V', celmex,&
                iret, ' ')
    ASSERT(iret .eq. 0)
!
!   recup des objets relatifs a celmex
    call jeveuo(celmex//'.CELK', 'L', jcelkm)
    call jeveuo(celmex//'.CELD', 'L', vi=celdm)
    call jeveuo(celmex//'.CELV', 'E', jcelvm)
!
!   recup des objets relatifs a celthx / ligthx
    call jeveuo(celthx//'.CELK', 'L', jcelkt)
    call jeveuo(celthx//'.CELD', 'L', vi=celdt)
    call jeveuo(celthx//'.CELV', 'L', jcelvt)
    call jeveuo(ligthx//'.REPE', 'L', vi=repet)
!
!   allocation de deux vecteurs de travail de taille nbma
    AS_ALLOCATE(vi=vecma_th, size=nbma)
    AS_ALLOCATE(vi=vecma_me, size=nbma)
!
!   initialisation
    do ima = 1, nbma
        vecma_me(ima) = 0
        vecma_th(ima) = 0
    enddo
!
!   on ecrit 1 dans vecma_me pour les mailles sur lesquelles celmex
!   est defini -> boucle sur les grels de ligmex
    nbgrm = celdm(2)
    do igrm = 1, nbgrm
        debgrm = celdm(4+igrm)
        nbelm  = celdm(debgrm+1)
        imolom = celdm(debgrm+2)
!       si celmex existe sur ce grel
        if (imolom .gt. 0) then
            call jeveuo(jexnum(ligmex//'.LIEL', igrm), 'L', alielm)
!           boucle sur les elements de igrm
            do ielm = 1, nbelm
                ima = zi(alielm-1+ielm)
                vecma_me(ima) = 1
            enddo
!           fin boucle sur les elements de igrm
        endif
    enddo
!   fin boucle sur les grels de ligmex
!
!   on ecrit 1 dans vecma_th pour les mailles sur lesquelles celthx
!   est defini -> boucle sur les grels de ligthx
    nbgrt = celdt(2)
    do igrt = 1, nbgrt
        debgrt = celdt(4+igrt)
        nbelt  = celdt(debgrt+1)
        imolot = celdt(debgrt+2)
!       si celthx existe sur ce grel
        if (imolot .gt. 0) then
            call jeveuo(jexnum(ligthx//'.LIEL', igrt), 'L', alielt)
!           boucle sur les elements de igrt
            do ielt = 1, nbelt
                ima = zi(alielt-1+ielt)
                vecma_th(ima) = 1
            enddo
!           fin boucle sur les elements de igrt
        endif
    enddo
!   fin boucle sur les grels de ligthx
!
!   ces deux cham_elem doivent etre definis sur les memes mailles
     do ima = 1, nbma
        ASSERT(vecma_me(ima) .eq. vecma_th(ima))
    enddo   
!
    AS_DEALLOCATE(vi=vecma_me)
    AS_DEALLOCATE(vi=vecma_th)
!
! ----------------------------------------------------------------------
! - boucle sur les grels de ligmex
! ----------------------------------------------------------------------
!
    call jeveuo('&CATA.TE.PNLOCFPG', 'L', jpnlfp)
    call jeveuo('&CATA.TE.NOLOCFPG', 'L', jnolfp)
    call jelira('&CATA.TE.NOLOCFPG', 'LONMAX', nblfpg)
    call jeveuo('&CATA.TM.TMFPG', 'L', vi=tmfpg)
!
    nbgrm = celdm(2)
    do igrm = 1, nbgrm
!
        debgrm = celdm(4+igrm)
        nbelm  = celdm(debgrm+1)
        imolom = celdm(debgrm+2)
        lgcatm = celdm(debgrm+3)
!
!       si celmex existe sur ce grel
        if (imolom .gt. 0) then
!
!           nom de l'element mecanique
            nutem = typele(ligmex, igrm)
            call jenuno(jexnum('&CATA.TE.NOMTE', nutem), ktyelm)
!
!           recuperation du mode local
            call jeveuo(jexnum('&CATA.TE.MODELOC', imolom), 'L', amolom)
            call jenuno(jexnum('&CATA.TE.NOMMOLOC', imolom), nomolm)
!           code pour le mode local ELGA__ : 3
            ASSERT(zi(amolom-1+1) .eq. 3)
!
!           blindage sur les familles de pg de FPG_LISTE MATER pour
!           l'element mecanique
            igdm = zi(amolom-1+2)
            call jenuno(jexnum('&CATA.GD.NOMGD', igdm), nomgdm)
            call dismoi('NB_EC', nomgdm, 'GRANDEUR', repi=nec)
            kfpgm = zi(amolom-1+4+nec+1)
            ASSERT(kfpgm .lt. 0)
            call jenuno(jexnum('&CATA.TE.NOFPG_LISTE', -kfpgm), nofpglism)
            ASSERT(nofpglism(17:24) .eq. 'MATER')
            call jelira(jexnum('&CATA.TE.FPG_LISTE', -kfpgm), 'LONMAX', nbfamm)
            call jeveuo(jexnum('&CATA.TE.FPG_LISTE', -kfpgm), 'L', adfpgm)
!           il ne peut y avoir que 2 familles dans la liste MATER
            nbfamm=nbfamm-1
            ASSERT(nbfamm .eq. 2)
            elrefm = zk8(adfpgm-1+nbfamm+1)
!
!           la premiere famille doit etre 'XFEM'
!           (l'ordre est important au moment ou l'on recopie les champs)
            ASSERT(zk8(adfpgm-1+1) .eq. 'XFEM')
            noflpg = ktyelm//elrefm//zk8(adfpgm-1+1)
            nuflpg = indk32(zk32(jpnlfp),noflpg,1,nblfpg)
            ASSERT(nuflpg .gt. 0)
            nufgpg = zi(jnolfp-1+nuflpg)
            ASSERT(nufgpg .gt. 0)
!           recup du nombre de pg pour cette famille
            nbpg_x = tmfpg(nufgpg)
!
!           la seconde famille doit etre 'NOEU'
            ASSERT(zk8(adfpgm-1+2) .eq. 'NOEU')
            noflpg = ktyelm//elrefm//zk8(adfpgm-1+2)
            nuflpg = indk32(zk32(jpnlfp),noflpg,1,nblfpg)
            ASSERT(nuflpg .gt. 0)
            nufgpg = zi(jnolfp-1+nuflpg)
            ASSERT(nufgpg .gt. 0)
!           recup du nombre de pg pour cette famille
            nbpg_n = tmfpg(nufgpg)
!
!           adresse de l'objet .LIEL
            call jeveuo(jexnum(ligmex//'.LIEL', igrm), 'L', alielm)
!
! ----------------------------------------------------------------------
! --------- boucle sur les elements de igrm
! ----------------------------------------------------------------------
!
            do ielm = 1, nbelm
!
                ima = zi(alielm-1+ielm)
!
!               position dans ligthx de l'element porte par ima 
                igrt = repet(2*(ima-1)+1)
                ielt = repet(2*(ima-1)+2)
                debgrt = celdt(4+igrt)
!
!               verification de la coherence du mode local mecanique
!               et thermique (code et grandeur)
                imolot = celdt(debgrt+2)
                call jeveuo(jexnum('&CATA.TE.MODELOC', imolot), 'L',&
                            amolot)
                call jenuno(jexnum('&CATA.TE.NOMMOLOC', imolot), nomolt)
                do i = 1,2
                    ASSERT(zi(amolom-1+i) .eq. zi(amolot-1+i))
                enddo
!
!               calcul du nombre de cmp de la grandeur
                nbscal_t = zi(amolot-1+3)
                nb_pt_t = zi(amolot-1+4)
                nbmcp = nbscal_t/nb_pt_t
!               - en 2D, 3 cmp : TEMP, DTX, DTY
!               - en 3D, 4 cmp : TEMP, DTX, DTY, DTZ
                ASSERT((nbmcp .eq. 3) .or. (nbmcp .eq. 4))
!
!               blindage sur le nom des elements
                nutet = typele(ligthx, igrt)
                call jenuno(jexnum('&CATA.TE.NOMTE', nutet), ktyelt)
                ASSERT(ktyelm(5:16) .eq. ktyelt(5:16))
!
!               verification des longueurs locales des champs
                lgelm = celdm(debgrm+4+4*(ielm-1)+3)
                lgelt = celdt(debgrt+4+4*(ielt-1)+3)
                ASSERT( lgelm .eq. nbmcp*(nbpg_x+nbpg_n))
                ASSERT( lgelt .eq. nbmcp*nbpg_x)
!
!               il ne peut pas y avoir de sous-points
                lgcatt = celdt(debgrt+3)
                ASSERT( lgelm .eq. lgcatm )
                ASSERT( lgelt .eq. lgcatt )
!
!               ecriture sur l'element ielm du segment de valeurs lu sur l'element ielt
                advelm = celdm(debgrm+4+4*(ielm-1)+4)
                advelm = jcelvm-1+advelm
                advelt = celdt(debgrt+4+4*(ielt-1)+4)
                advelt = jcelvt-1+advelt
!               -> sur la famille XFEM : recopie des valeurs lues
                zr(advelm : advelm+lgelt) = zr(advelt : advelt+lgelt)
!               -> sur la famille NOEU : on ecrit NaN
                do ival = lgelt+1,lgelm
                    zr(advelm-1+ival) = r8nan
                enddo
!
            enddo
!
! ----------------------------------------------------------------------
! --------- fin boucle sur les elements de igrm
! ----------------------------------------------------------------------
!
        endif
!
    enddo
!
! ----------------------------------------------------------------------
! - fin boucle sur les grels de ligmex
! ----------------------------------------------------------------------
!
999 continue
!
    call jedema()
!
end subroutine


