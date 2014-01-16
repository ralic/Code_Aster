subroutine xcpmo2(modx1, modx2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sam.cuvilliez at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/copich.h"
#include "asterfort/indk16.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/typele.h"
!
    character(len=8) :: modx1, modx2
!
! ----------------------------------------------------------------------
!
! in      modx1 : modele thermique x-fem (mot-cle MODELE_THER)
! in/out  modx2 : modele produit par l'operateur
!
! ----------------------------------------------------------------------
!
! routine xfem : MODI_MODELE_XFEM
!
! --> traitement particulier realise dans le cas ou le mot-cle
!     MODELE_THER est present. Copier les champs out de TOPOSE 
!     et TOPOFA de modx1 dans modx2 :
!
!       '.TOPOSE.PIN'
!       '.TOPOSE.CNS'
!       '.TOPOSE.HEA'
!       '.TOPOSE.LON'
!       '.TOPOSE.PMI'
!
!       '.TOPOFAC.PI'
!       '.TOPOFAC.AI'
!       '.TOPOFAC.CF'
!       '.TOPOFAC.LO'
!       '.TOPOFAC.BA'
!       '.TOPOFAC.OE'
!
! --> il ne suffit pas simplement de copier ces CHAM_ELEM / CHAM_ELNO
!     car les ligrels modx1//'.MODELE' et modx2//'.MODELE' peuvent 
!     etre differents. On procede alors de la maniere suivante :
!
!     - les .CELK et .CELD pour modx2 sont crees et remplis par alchml()
!     - le .CELV pour modx2 est ensuite rempli element par element,
!       en recherchant pour chaque element de modx2//'.MODELE' sa 
!       position dans modx1//'.MODELE'
!
! ----------------------------------------------------------------------
!
    integer :: iret, icopy, ncopy, ima, icode, iopt, jcelk2, jceld2
    integer :: jcelv2, jcelk1, jceld1, jcelv1, nbel2, iel2, iel1
    integer :: lgel2, lgel1, debgr2, debgr1, igr2, igr1, advel2, advel1
    integer :: imolo2, imolo1, amolo2, amolo1, nbgr2, aliel2, jrepe1
    integer :: lgcat2, nute2
!
    character(len=4) :: k4typ2
    character(len=8) :: para, k8blan
    character(len=16) :: option, ktyel2
    character(len=19) :: ligr1, ligr2, chele1, chele2
    character(len=24) :: nomol1, nomol2
!
    integer :: ntopos, ntopof, noptri
    parameter (ntopos = 5)
    parameter (ntopof = 6)
    parameter (noptri = 4)
    character(len=8) :: lpara(ntopos+ntopof+noptri)
    character(len=16) :: lopti(ntopos+ntopof+noptri)
    character(len=19) :: lcham(ntopos+ntopof+noptri)
!   ntopos : nbre de cham_el** a allouer avec option == TOPOSE
!   ntopof : nbre de cham_el** a allouer avec option == TOPOFA
!   noptri : nbre de cham_el** a allouer avec option == FULL_MECA
!
    integer :: nma3d, nma2d, nma1d, nenrch
    parameter ( nma3d  = 4 )
    parameter ( nma2d  = 2 )
    parameter ( nma1d  = 1 )
    parameter ( nenrch = 3 )
!   nma3d : HEXA8, PENTA6, PYRAM5, TETRA4 -> 4
!   nma2d : TRIA3, QUAD4 -> 2
!   nma1d : SEG2 -> 1
!   nenrch : XH, XT, XHT
!
    integer :: nel3dmex, nelplmex, nelaxmex, nelemex
    parameter ( nel3dmex = (  nma3d+nma2d)*nenrch )
    parameter ( nelplmex = (2*nma2d+nma1d)*nenrch )
    parameter ( nelaxmex = (  nma2d+nma1d)*nenrch )
    parameter ( nelemex  = nel3dmex+nelplmex+nelaxmex )
    character(len=16) :: elemex(nelemex)
!
!   elements mecaniques X-FEM
!   -------------------------------------------------------------------
    data elemex/&
!   3D principaux
    &'MECA_XH_HEXA8   ','MECA_XT_HEXA8   ','MECA_XHT_HEXA8  ',&
    &'MECA_XH_PENTA6  ','MECA_XT_PENTA6  ','MECA_XHT_PENTA6 ',&
    &'MECA_XH_PYRAM5  ','MECA_XT_PYRAM5  ','MECA_XHT_PYRAM5 ',&
    &'MECA_XH_TETRA4  ','MECA_XT_TETRA4  ','MECA_XHT_TETRA4 ',&
!   3D de bord
    &'MECA_XH_FACE4   ','MECA_XT_FACE4   ','MECA_XHT_FACE4  ',&
    &'MECA_XH_FACE3   ','MECA_XT_FACE3   ','MECA_XHT_FACE3  ',&
!   C_PLAN/D_PLAN principaux
    &'MECPQU4_XH      ','MECPQU4_XT      ','MECPQU4_XHT     ',&
    &'MECPTR3_XH      ','MECPTR3_XT      ','MECPTR3_XHT     ',&
    &'MEDPQU4_XH      ','MEDPQU4_XT      ','MEDPQU4_XHT     ',&
    &'MEDPTR3_XH      ','MEDPTR3_XT      ','MEDPTR3_XHT     ',&
!   C_PLAN/D_PLAN de bord
    &'MEPLSE2_XH      ','MEPLSE2_XT      ','MEPLSE2_XHT     ',&
!   AXIS principaux
    &'MEAXQU4_XH      ','MEAXQU4_XT      ','MEAXQU4_XHT     ',&
    &'MEAXTR3_XH      ','MEAXTR3_XT      ','MEAXTR3_XHT     ',&
!   AXIS de bord
    &'MEAXSE2_XH      ','MEAXSE2_XT      ','MEAXSE2_XHT     '/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! ----------------------------------------------------------------------
!   initialisations
! ----------------------------------------------------------------------
!
    ligr1 = modx1//'.MODELE'
    ligr2 = modx2//'.MODELE'
!
    do iopt = 1,ntopos
        lopti(iopt) = 'TOPOSE          '
    enddo
    do iopt = 1,ntopof
        lopti(ntopos+iopt) = 'TOPOFA          '
    enddo
    do iopt = 1,noptri
        lopti(ntopos+ntopof+iopt) = 'FULL_MECA       '
    enddo
!
    k8blan = '        '
!
!   pour option == TOPOSE dans alchml
    lcham(1)  = k8blan//'.TOPOSE.PIN'
    lcham(2)  = k8blan//'.TOPOSE.CNS'
    lcham(3)  = k8blan//'.TOPOSE.HEA'
    lcham(4)  = k8blan//'.TOPOSE.LON'
    lcham(5)  = k8blan//'.TOPOSE.PMI'
!   pour option == TOPOFA dans alchml
    lcham(6)  = k8blan//'.TOPOFAC.PI'
    lcham(7)  = k8blan//'.TOPOFAC.AI'
    lcham(8)  = k8blan//'.TOPOFAC.CF'
    lcham(9)  = k8blan//'.TOPOFAC.LO'
    lcham(10) = k8blan//'.TOPOFAC.BA'
    lcham(11) = k8blan//'.TOPOFAC.OE'
!   pour option == FULL_MECA dans alchml
    lcham(12) = k8blan//'.STNO'
    lcham(13) = k8blan//'.LNNO'
    lcham(14) = k8blan//'.LTNO'
    lcham(15) = k8blan//'.BASLOC'

!   pour option == TOPOSE dans alchml
    lpara(1)  = 'PPINTTO'
    lpara(2)  = 'PCNSETO'
    lpara(3)  = 'PHEAVTO'
    lpara(4)  = 'PLONCHA'
    lpara(5)  = 'PPMILTO'
!   pour option == TOPOFA dans alchml
    lpara(6)  = 'PPINTER'
    lpara(7)  = 'PAINTER'
    lpara(8)  = 'PCFACE'
    lpara(9)  = 'PLONCHA'
    lpara(10) = 'PBASECO'
    lpara(11) = 'PGESCLA'
!   pour option == FULL_MECA dans alchml
    lpara(12) = 'PSTANO'
    lpara(13) = 'PLSN'
    lpara(14) = 'PLST'
    lpara(15) = 'PBASLOR'
!
    ncopy = ntopos + ntopof + noptri
!
! ----------------------------------------------------------------------
!   boucle sur les champs a copier
! ----------------------------------------------------------------------
!
    do icopy = 1,ncopy

        chele2 = modx2//lcham(icopy)(9:19)
        para   = lpara(icopy)
        option = lopti(icopy)

!       allocation du CHAM_EL** chele2
        call alchml(ligr2, option, para, 'G', chele2, iret, ' ')
        ASSERT(iret.eq.0)

        call jeveuo(chele2//'.CELK', 'L', jcelk2)
        call jeveuo(chele2//'.CELD', 'L', jceld2)
        call jeveuo(chele2//'.CELV', 'E', jcelv2)

        chele1 = modx1//lcham(icopy)(9:19)
        call jeveuo(chele1//'.CELK', 'L', jcelk1)
        call jeveuo(chele1//'.CELD', 'L', jceld1)
        call jeveuo(chele1//'.CELV', 'L', jcelv1)
        call jeveuo(ligr1//'.REPE', 'L', jrepe1)
        
        call jelira(chele2//'.CELV', 'TYPELONG', cval=k4typ2)
        
! ----------------------------------------------------------------------
!       boucle sur les grels de ligr2
! ----------------------------------------------------------------------
!
        nbgr2 = zi(jceld2-1+2)
!
        do igr2 = 1, nbgr2

            debgr2 = zi(jceld2-1+4+igr2)
            nbel2  = zi(jceld2-1+debgr2+1)
            imolo2 = zi(jceld2-1+debgr2+2)
            lgcat2 = zi(jceld2-1+debgr2+3)

!           si les elts de ce grel savent calculer "option"
            if (imolo2 .gt. 0) then

!               pour s'assurer que les elts qui savent calculer "option"
!               sont uniquement des elts X-FEM
                nute2 = typele(ligr2,igr2)
                call jenuno(jexnum('&CATA.TE.NOMTE', nute2), ktyel2)
                ASSERT( indk16(elemex, ktyel2, 1, nelemex) .gt. 0 )

!               recuperation du mode local
                call jeveuo(jexnum('&CATA.TE.MODELOC', imolo2), 'L', amolo2)
                call jenuno(jexnum('&CATA.TE.NOMMOLOC', imolo2), nomol2)

!               code pour le mode local ELEM__ (1) ou ELNO__ (2)
                ASSERT((zi(amolo2-1+1).eq.1) .or. (zi(amolo2-1+1).eq.2))

                call jeveuo(jexnum(ligr2//'.LIEL', igr2), 'L', aliel2)

! ----------------------------------------------------------------------
!               boucle sur les elts de igr2 qui savent calculer "option"
! ----------------------------------------------------------------------

                do iel2 = 1, nbel2

                    ima = zi(aliel2-1+iel2)

!                   infos sur la position de l'element porte par ima dans ligr1
                    igr1 = zi(jrepe1-1+2*(ima-1)+1)
                    iel1 = zi(jrepe1-1+2*(ima-1)+2)
                    debgr1 = zi(jceld1-1+4+igr1)

!                   verification de coherence sur le mode_local dans chele2 et chele1
                    imolo1 = zi(jceld1-1+debgr1+2)
                    ASSERT( imolo1 .gt. 0 )
                    call jeveuo(jexnum('&CATA.TE.MODELOC', imolo1), 'L', amolo1)
                    call jenuno(jexnum('&CATA.TE.NOMMOLOC', imolo1), nomol1)

                    do icode = 1,4
                        ASSERT(zi(amolo2-1+icode) .eq. zi(amolo1-1+icode))
                    enddo

!                   verification longueur locale du champ dans chele2 et chele1
                    lgel2 = zi(jceld2-1+debgr2+4+4*(iel2-1)+3) 
                    lgel1 = zi(jceld1-1+debgr1+4+4*(iel1-1)+3) 
                    ASSERT( lgel2 .eq. lgel1)

!                   verification permettant d'exclure les sous-points et VARI_R
                    ASSERT( lgel2 .eq. lgcat2 )

!                   copie de chele1 dans chele2 (valeurs de iel1 dans iel2)
                    advel2 = zi(jceld2-1+debgr2+4+4*(iel2-1)+4)
                    advel2 = jcelv2-1+advel2
                    advel1 = zi(jceld1-1+debgr1+4+4*(iel1-1)+4)
                    advel1 = jcelv1-1+advel1
!                   les types autorises sont reel ou entier
                    if     (k4typ2 .eq. 'R') then
                        zr(advel2 : advel2+lgel2) = zr(advel1 : advel1+lgel1)
                    elseif (k4typ2 .eq. 'I') then
                        zi(advel2 : advel2+lgel2) = zi(advel1 : advel1+lgel1)
                    else
                        ASSERT(.false.)
                    endif

                enddo
! ----------------------------------------------------------------------
!               fin boucle sur les elts de igr2
!
            endif
!
        enddo
! ----------------------------------------------------------------------
!       fin boucle sur les grels de ligr2
!
    enddo
! ----------------------------------------------------------------------
!   fin boucle sur les champs a copier
!
! ----------------------------------------------------------------------
!   autres objets (!= cham_elem) independants d'un changement de ligrel
! ----------------------------------------------------------------------
!
    call copich('G', modx1//'.NOXFEM', modx2//'.NOXFEM')
    call jedup1(modx1//'.NFIS', 'G', modx2//'.NFIS')
    call jedup1(modx1//'.FISS', 'G', modx2//'.FISS')
    call jedup1(modx1//'.XFEM_CONT', 'G', modx2//'.XFEM_CONT')
!
    call jedema()
!
end subroutine
