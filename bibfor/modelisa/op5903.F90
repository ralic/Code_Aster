subroutine op5903(nbocci, sdcomp)
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
!                   DEFI_COMPOR / MULTIFIBRE
!
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
    implicit none
!
    integer, intent(in) :: nbocci
    character(len=8), intent(in) :: sdcomp
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterc/lccree.h"
#include "asterc/lctest.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_rkit.h"
#include "asterfort/comp_meca_vari.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: idbor, imi, jcprk, iocc, irett
    integer :: ibid, nbg, nbgrfib, jnmgrfib, ig, ig1, jnbfig, iaff
    integer :: nbvf, nbv, icp
    character(len=8) :: materi, sdgf, mator
    character(len=16) :: rela_comp, defo_comp, algo1d, rela_comp_py, kit_comp(9)
    character(len=16) :: moclef
    character(len=24) :: vnbfig, vnmfig, kgroup
    aster_logical :: l_kit, l_auto_deborst
!
    integer :: valmi(5)
    character(len=80) :: valmk(5)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
! --------------------------------------------------------------------------------------------------
!   on récupère les renseignements dans la sd_group_fibre
!       noms de tous les groupes, nb maxi de groupes, nb de fibres par groupe
    nbvf=0
    moclef='MULTIFIBRE'
    l_auto_deborst = .false.
!
    call getvid(' ', 'GEOM_FIBRE', scal=sdgf, nbret=ibid)
    vnbfig = sdgf//'.NB_FIBRE_GROUPE'
    vnmfig = sdgf//'.NOMS_GROUPES'
    call jeveuo(vnbfig, 'L', jnbfig)
    call jelira(vnbfig, 'LONMAX', nbgrfib)
!   le +1 c'est pour le matériau de torsion : MATER_SECT fin de routine
    call wkvect(sdcomp//'.CPRK', 'G V K24', 6*nbgrfib+1, jcprk)
    call wkvect('&&OP0059.NOMS_GROUPES', 'V V K24', nbgrfib, jnmgrfib)
    call wkvect('&&OP0059.VERIF_AFFECT', 'V V I', nbgrfib, iaff)
    do ig = 1, nbgrfib
        zi(iaff-1+ig) = 0
    enddo
    idbor = 0
!
    do iocc = 1, nbocci
        call getvtx(moclef, 'GROUP_FIBRE', iocc=iocc, nbval=0, nbret=nbg)
        nbg=-nbg
        if ( nbg.gt.nbgrfib ) then
            valmi(1) = iocc
            valmi(2) = nbg
            valmi(3) = nbgrfib
            valmk(1) = moclef//' / GROUP_FIBRE'
            call utmess('F', 'MODELISA8_19', ni=3 , vali=valmi, nk=1, valk=valmk)
        endif
        call getvtx(moclef, 'GROUP_FIBRE', iocc=iocc, nbval = nbg, vect = zk24(jnmgrfib))
        call getvid(moclef, 'MATER', iocc=iocc, scal = materi)
        call getvtx(moclef, 'RELATION', iocc=iocc, scal = rela_comp)
        defo_comp = 'VIDE'
        algo1d = 'ANALYTIQUE'
!
!       Coding comportment (Python)
        call lccree(1, rela_comp, rela_comp_py)
!       ALGO1D
        call lctest(rela_comp_py, 'MODELISATION', '1D', irett)
        if (irett .eq. 0) then
            l_auto_deborst = .true.
            algo1d = 'DEBORST'
            idbor = idbor+1
        endif
!       Get number of internal variables
        if (rela_comp(1:4) .eq. 'KIT_') then
            ASSERT(rela_comp.eq.'KIT_DDI')
        endif
        l_kit = (rela_comp.eq.'KIT')
        if (l_kit) then
            call comp_meca_rkit(moclef, iocc, rela_comp, kit_comp)
        endif
        call comp_meca_vari(rela_comp, defo_comp, algo1d, nbv)
!
        do ig = 1, nbg
!           Numéro correspondant au nom
            call jenonu(jexnom(vnmfig, zk24(jnmgrfib+ig-1)), ig1)
            if (ig1 .eq. 0) then
                call utmess('F', 'MODELISA8_8', sk=zk24(jnmgrfib+ig-1))
            endif
            icp=jcprk-1+(ig1-1)*6
            zk24(icp+1) = zk24(jnmgrfib+ig-1)
            zk24(icp+2) = materi
            zk24(icp+3) = rela_comp
            zk24(icp+4) = algo1d
            zk24(icp+5) = defo_comp
            write(zk24(icp+6),'(I24)') zi(jnbfig-1+ig1)
            zi(iaff-1+ig1) = 1
        enddo
!       on met à jour le nombre de variables internes maxi
        nbvf=max(nbvf,nbv)
    enddo
!
!   vérification de l'utilisation de COMP_1D
    if (nbocci .gt. 1) then
        if (idbor .ge. 1) then
            call utmess('F', 'COMPOR5_30')
        endif
    endif
!   vérification que tout est affecté au moins une fois. Les groupes non affectes 'VIDE'
    do ig = 1, nbgrfib
        if (zi(iaff-1+ig) .eq. 0) then
            call jenuno(jexnum(vnmfig, ig), kgroup)
            icp=jcprk-1+(ig-1)*6
            zk24(icp+1) = kgroup
            zk24(icp+2) = 'VIDE'
        endif
    enddo
    if (l_auto_deborst) then
        call utmess('I', 'COMPOR5_20')
    endif
!   on récupère le nom du matériau pour la torsion, mis à la fin
    call getvid(' ', 'MATER_SECT', scal=mator, nbret=ibid)
    zk24(jcprk-1+nbgrfib*6+1)=mator
    call wkvect(sdcomp//'.CPRI', 'G V I', 3, imi)
!   type 3 = MULTIFIBRE
    zi(imi) = 3
    zi(imi+1) = nbvf
    zi(imi+2) = nbgrfib
!
    call jedema()
end subroutine
