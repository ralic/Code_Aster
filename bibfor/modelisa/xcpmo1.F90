subroutine xcpmo1(modmes, modthx, modmex)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sam.cuvilliez at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/adalig.h"
#include "asterfort/assert.h"
#include "asterfort/cormgi.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/dismte.h"
#include "asterfort/initel.h"
#include "asterfort/indk16.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/utmess.h"
#include "asterfort/xtmafi.h"
!
    character(len=8) :: modmes, modthx, modmex
!
! ----------------------------------------------------------------------
!
! routine xfem : MODI_MODELE_XFEM
!
!     Traitement particulier realise dans le cas ou le mot-cle
!     MODELE_THER est present. Cette routine est appelee par xcpmod
!     une fois que la copie de modmes dans modmex a ete effectuee
!
! --> on modifie dans cette routine certains objets de modmex en fonction
!     de mothx : 
!       - le '.MAILLE' modmex//.MAILLE'
!       - le ligrel    modmex//.MODELE'
!
! ----------------------------------------------------------------------
!
! in     modmes : nom du modele mecanique sain (mot-cle modele_in)
! in     modthx : nom du modele thermique x-fem (mot-cle modele_ther)
! in/out modmex : nom du modele mecanique x-fem produit par l'operateur
!
! ----------------------------------------------------------------------
!
    character(len=24) :: lieltp, mesmai, lismai
    character(len=19) :: ligthx, ligmes, ligmex, ligrtp
    character(len=16) :: ktyelt, ktyelm
    character(len=8) :: noma, valk8(3), nommax
    character(len=1) :: k1bid
    integer :: igr, jeltp, imx
    integer :: ima, iexi
    integer :: nmamex
    integer :: nfiss, nbmx, nutyelt, nutyelm
    integer :: nbelmx, nbel2, cpt
!
    integer, pointer :: tabmx(:) => null()
    character(len=8), pointer :: fiss(:) => null()
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
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!                        THERMIQUE X-FEM
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
    integer :: nel3dthx, nelplthx, nelaxthx
    parameter ( nel3dthx = (nma3d+nma2d)*nenrch )
    parameter ( nelplthx = (nma2d+nma1d)*nenrch )
    parameter ( nelaxthx = (nma2d+nma1d)*nenrch )
    character(len=16) :: ele3dthx(nel3dthx)
    character(len=16) :: eleplthx(nelplthx)
    character(len=16) :: eleaxthx(nelaxthx)
!
!   elements 3D lineaires thermiques X-FEM
!   -------------------------------------------------------------------
    data ele3dthx/&
!   principaux
    &'THER_XH_HEXA8   ','THER_XT_HEXA8   ','THER_XHT_HEXA8  ',&
    &'THER_XH_PENTA6  ','THER_XT_PENTA6  ','THER_XHT_PENTA6 ',&
    &'THER_XH_PYRAM5  ','THER_XT_PYRAM5  ','THER_XHT_PYRAM5 ',&
    &'THER_XH_TETRA4  ','THER_XT_TETRA4  ','THER_XHT_TETRA4 ',&
!   de bord
    &'THER_XH_FACE4   ','THER_XT_FACE4   ','THER_XHT_FACE4  ',&
    &'THER_XH_FACE3   ','THER_XT_FACE3   ','THER_XHT_FACE3  '/
!
!   elements PLAN lineaires thermiques X-FEM
!   -------------------------------------------------------------------
    data eleplthx/&
!   principaux
    &'THPLQU4_XH      ','THPLQU4_XT      ','THPLQU4_XHT     ',&
    &'THPLTR3_XH      ','THPLTR3_XT      ','THPLTR3_XHT     ',&
!   de bord
    &'THPLSE2_XH      ','THPLSE2_XT      ','THPLSE2_XHT     '/
!
!   elements AXIS lineaires thermiques X-FEM
!   -------------------------------------------------------------------
    data eleaxthx/&
!   principaux
    &'THAXQU4_XH      ','THAXQU4_XT      ','THAXQU4_XHT     ',&
    &'THAXTR3_XH      ','THAXTR3_XT      ','THAXTR3_XHT     ',&
!   de bord
    &'THAXSE2_XH      ','THAXSE2_XT      ','THAXSE2_XHT     '/
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!                        MECANIQUE X-FEM
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
    integer :: nel3dmex, nelplmex, nelaxmex
    parameter ( nel3dmex = (  nma3d+nma2d)*nenrch )
    parameter ( nelplmex = (2*nma2d+nma1d)*nenrch )
    parameter ( nelaxmex = (  nma2d+nma1d)*nenrch )
    character(len=16) :: ele3dmex(nel3dmex)
    character(len=16) :: eleplmex(nelplmex)
    character(len=16) :: eleaxmex(nelaxmex)
!
!   elements 3D lineaires mecaniques X-FEM
!   -------------------------------------------------------------------
    data ele3dmex/&
!   principaux
    &'MECA_XH_HEXA8   ','MECA_XT_HEXA8   ','MECA_XHT_HEXA8  ',&
    &'MECA_XH_PENTA6  ','MECA_XT_PENTA6  ','MECA_XHT_PENTA6 ',&
    &'MECA_XH_PYRAM5  ','MECA_XT_PYRAM5  ','MECA_XHT_PYRAM5 ',&
    &'MECA_XH_TETRA4  ','MECA_XT_TETRA4  ','MECA_XHT_TETRA4 ',&
!   de bord
    &'MECA_XH_FACE4   ','MECA_XT_FACE4   ','MECA_XHT_FACE4  ',&
    &'MECA_XH_FACE3   ','MECA_XT_FACE3   ','MECA_XHT_FACE3  '/
!
!   elements C_PLAN/D_PLAN lineaires mecaniques X-FEM
!   -------------------------------------------------------------------
    data eleplmex/&
!   principaux
    &'MECPQU4_XH      ','MECPQU4_XT      ','MECPQU4_XHT     ',&
    &'MECPTR3_XH      ','MECPTR3_XT      ','MECPTR3_XHT     ',&
    &'MEDPQU4_XH      ','MEDPQU4_XT      ','MEDPQU4_XHT     ',&
    &'MEDPTR3_XH      ','MEDPTR3_XT      ','MEDPTR3_XHT     ',&
!   de bord
    &'MEPLSE2_XH      ','MEPLSE2_XT      ','MEPLSE2_XHT     '/
!
!   elements AXIS lineaires mecaniques X-FEM
!   -------------------------------------------------------------------
    data eleaxmex/&
!   principaux
    &'MEAXQU4_XH      ','MEAXQU4_XT      ','MEAXQU4_XHT     ',&
    &'MEAXTR3_XH      ','MEAXTR3_XT      ','MEAXTR3_XHT     ',&
!   de bord
    &'MEAXSE2_XH      ','MEAXSE2_XT      ','MEAXSE2_XHT     '/
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!                        MECANIQUE CLASSIQUE
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
    integer :: nel3dmec, nelplmec, nelaxmec
    parameter ( nel3dmec =   nma3d+nma2d )
    parameter ( nelplmec = 2*nma2d+nma1d )
    parameter ( nelaxmec =   nma2d+nma1d )
    character(len=16) :: ele3dmec(nel3dmec)
    character(len=16) :: eleplmec(nelplmec)
    character(len=16) :: eleaxmec(nelaxmec)
    integer, pointer :: mmes(:) => null()
    integer, pointer :: mmex(:) => null()
    integer, pointer :: mthx(:) => null()
    character(len=8), pointer :: lgrf(:) => null()
!
!   elements 3D lineaires mecaniques classiques
! ---------------------------------------------------------------------
    data ele3dmec/&
!   principaux
    &'MECA_HEXA8      ',&
    &'MECA_PENTA6     ',&
    &'MECA_PYRAM5     ',&
    &'MECA_TETRA4     ',&
!   de bord
    &'MECA_FACE4      ',&
    &'MECA_FACE3      '/
!
!   elements C_PLAN/D_PLAN lineaires mecaniques classiques
! ---------------------------------------------------------------------
    data eleplmec/&
!   principaux
    &'MECPQU4         ',&
    &'MECPTR3         ',&
    &'MEDPQU4         ',&
    &'MEDPTR3         ',&
!   de bord
    &'MEPLSE2         '/
!
!   elements AXIS lineaires mecaniques classiques
! ---------------------------------------------------------------------
    data eleaxmec/&
!   principaux
    &'MEAXQU4         ',&
    &'MEAXTR3         ',&
!   de bord
    &'MEAXSE2         '/
!
! ---------------------------------------------------------------------
! - Debut code
! ---------------------------------------------------------------------
!
    call jemarq()
!
! ---------------------------------------------------------------------
!
    ligmes = modmes//'.MODELE'
    ligthx = modthx//'.MODELE'
    ligmex = modmex//'.MODELE'
!
! - recuperation de la liste de toutes les mailles fissurees
! - (de dimension n et n-1 car appel a xtmafi avec ndim == 0)
!
    lismai = '&&XMOT2M.NUM_MAILLES'
    mesmai = '&&XMOT2M.MES_MAILLES'
!
    call dismoi('NOM_MAILLA', modthx, 'MODELE', repk=noma)
    call dismoi('NB_FISS_XFEM', modthx, 'MODELE', repi=nfiss)
    call jeveuo(modthx//'.FISS', 'L', vk8=fiss)
!
    call xtmafi(0, fiss, nfiss, lismai, mesmai, nbmx, model=modthx)
    call jeveuo(lismai, 'L', vi=tabmx)
!
! - recuperation du '.MAILLE' de modthx et modmes
!
    call jeveuo(modthx//'.MAILLE', 'L', vi=mthx)
    call jeveuo(modmes//'.MAILLE', 'L', vi=mmes)
!
! - on s'assure que toute maille affectee par un element thermique
! - enrichi dans modthx est bien affectee par un element mecanique 
! - sain dans modmes
!
    do ima = 1, nbmx
!
        imx = tabmx(ima)
!
        nutyelt = mthx(imx)
        nutyelm = mmes(imx)
!
        if (nutyelm .eq. 0) then
            call jenuno(jexnum(noma//'.NOMMAI', imx), nommax)
            valk8(1) = nommax
            valk8(2) = modthx
            valk8(3) = modmes
            call utmess('F', 'XFEM_85', nk=3, valk=valk8)
        endif
!
    enddo
!
! - copie integrale du contenu de modmes dans modmex 
!
    call copisd('MODELE', 'G', modmes, modmex)
!
! - on supprime modmex//'.PARTIT' s'il existe car MODI_MODELE_XFEM
! - avec mot-cle FISSURE ne recree pas cet objet s'il existe dans
! - le modele sain renseigne dans MODELE_IN
!
    call jeexin(modmex//'.PARTIT', iexi)
    if (iexi .ne. 0) call jedetr(modmex//'.PARTIT')
!
! - recuperation du '.MAILLE' de modmex
!
    call jeveuo(modmex//'.MAILLE', 'E', vi=mmex)
    call jelira(modmex//'.MAILLE', 'LONMAX', nmamex, k1bid)
!
! - modification du '.MAILLE' de modmex pour les mailles fissurees
!
    do ima = 1, nbmx
!
        imx = tabmx(ima)
!
        nutyelt = mthx(imx)
        call jenuno(jexnum('&CATA.TE.NOMTE', nutyelt), ktyelt)
!
        nutyelm = mmex(imx)
        call jenuno(jexnum('&CATA.TE.NOMTE', nutyelm), ktyelm)
!
!       MECANIQUE 3D
        if (indk16(ele3dmec, ktyelm, 1, nel3dmec) .gt. 0) then
            if (indk16(ele3dthx, ktyelt, 1, nel3dthx) .eq. 0) then
                ASSERT(.false.)
            endif
            ktyelm = ktyelm(1:4)//ktyelt(5:16)
            if (indk16(ele3dmex, ktyelm, 1, nel3dmex) .eq. 0) then
                ASSERT(.false.)
            endif
!
!       MECANIQUE C_PLAN/D_PLAN
        else if (indk16(eleplmec, ktyelm, 1, nelplmec) .gt. 0) then
            if (indk16(eleplthx, ktyelt, 1, nelplthx) .eq. 0) then
                ASSERT(.false.)
            endif
            ktyelm = ktyelm(1:4)//ktyelt(5:16)
            if (indk16(eleplmex, ktyelm, 1, nelplmex) .eq. 0) then
                ASSERT(.false.)
            endif
!
!       MECANIQUE AXIS        
        else if (indk16(eleaxmec, ktyelm, 1, nelaxmec) .gt. 0) then
            if (indk16(eleaxthx, ktyelt, 1, nelaxthx) .eq. 0) then
                ASSERT(.false.)
            endif
            ktyelm = ktyelm(1:4)//ktyelt(5:16)
            if (indk16(eleaxmex, ktyelm, 1, nelaxmex) .eq. 0) then
                ASSERT(.false.)
            endif
!
        else
            ASSERT(.false.)
!
        endif
!
        call jenonu(jexnom('&CATA.TE.NOMTE', ktyelm), nutyelm)
        mmex(imx) = nutyelm
!
    enddo
!
! - ligrel temporaire ligrtp (remplacera celui de modmex : ligmex)
!
    ligrtp = '&&XMOT2M'//'.MODELE'
    lieltp = ligrtp//'.LIEL'
!
    nbelmx = 0
    do igr = 1, nbgrel(ligmex)
        nbelmx = nbelmx + nbelem(ligmex, igr)
    enddo
!
    call jecrec(lieltp, 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbelmx)
    call jeecra(lieltp, 'LONT', 2*nbelmx)
!
    cpt = 0
    do ima = 1, nmamex
        if (mmex(ima) .eq. 0) cycle
        cpt = cpt+1
        call jecroc(jexnum(lieltp, cpt))
        call jeecra(jexnum(lieltp, cpt), 'LONMAX', 2)
        call jeveuo(jexnum(lieltp, cpt), 'E', jeltp)
        zi(jeltp-1+1) = ima
        zi(jeltp-1+2) = mmex(ima)
    end do
!
    call jelira(lieltp, 'NUTIOC', nbel2)
    ASSERT( nbel2 .eq. nbelmx )
!
    call jedupo(ligmex//'.NBNO', 'G', ligrtp//'.NBNO', .false._1)
    call jedupo(ligmex//'.LGRF', 'G', ligrtp//'.LGRF', .false._1)
    call jeveuo(ligrtp//'.LGRF', 'E', vk8=lgrf)
    lgrf(2) = modmex
!
! - on ecrase ligmex avec ligrtp
!
    call adalig(lieltp)
    call cormgi('V', lieltp)
    call initel(lieltp)
!
    call detrsd('LIGREL', ligmex)
    call copisd('LIGREL', 'G', ligrtp, ligmex)
    call detrsd('LIGREL', ligrtp)
!
! - menage final
!
    call jedetr(mesmai)
    call jedetr(lismai)
!
    call jedema()
!
end subroutine
