subroutine op5903(nbocci, sdcomp)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-luc.flejou at edf.fr
! ======================================================================
!
    integer, intent(in) :: nbocci
    character(len=8), intent(in) :: sdcomp
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_COMPOR
!
! MULTIFIBRE
!
! --------------------------------------------------------------------------------------------------
!
    integer :: idbor, imi, imk, iocc, irett
    integer :: ibid, nbg, nbgmax, img, ig, ig1, jnfg, iaff
    integer :: nbvf, nbv, icp
    character(len=8) :: materi, sdgf, mator
    character(len=16) :: rela_comp, defo_comp, algo1d
    character(len=16) :: rela_comp_py
    character(len=16) :: kit_comp(9)
    character(len=16) :: moclef
    character(len=24) :: vnbfig, rnomgf, kgroup
    aster_logical :: l_kit, l_auto_deborst
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
!     ON RECUPERE LES RENSEIGNEMENTS DANS LA SD_GROUP_FIBRE :
!        NOMS DE TOUS LES GROUPES
!        NB MAXI DE GROUPES
!        NB DE FIBRES PAR GROUPE
    nbvf=0
    moclef='MULTIFIBRE'
    l_auto_deborst = .false.
!
    call getvid(' ', 'GEOM_FIBRE', scal=sdgf, nbret=ibid)
    vnbfig = sdgf//'.NB_FIBRE_GROUPE'
    rnomgf = sdgf//'.NOMS_GROUPES'
    call jeveuo(vnbfig, 'L', jnfg)
    call jelira(vnbfig, 'LONMAX', nbgmax)
    call wkvect(sdcomp//'.CPRK', 'G V K24', 6*nbgmax+1, imk)
    call wkvect('&&OP0059.NOMS_GROUPES', 'V V K24', nbgmax, img)
    call wkvect('&&OP0059.VERIF_AFFECT', 'V V I', nbgmax, iaff)
    do ig = 1, nbgmax
        zi(iaff-1+ig) = 0
    end do
    idbor = 0
!
    do iocc = 1, nbocci
        call getvtx(moclef, 'GROUP_FIBRE', iocc=iocc, nbval=0, nbret=nbg)
        nbg=-nbg
        call getvtx(moclef, 'GROUP_FIBRE', iocc=iocc, nbval = nbg, vect = zk24(img))
        call getvid(moclef, 'MATER', iocc=iocc, scal = materi)
        call getvtx(moclef, 'RELATION', iocc=iocc, scal = rela_comp)
        defo_comp = 'VIDE'
        algo1d = 'ANALYTIQUE'
!
! ----- Coding comportment (Python)
!
        call lccree(1, rela_comp, rela_comp_py)
!
! ----- ALGO1D
!
        call lctest(rela_comp_py, 'MODELISATION', '1D', irett)
        if (irett .eq. 0) then
            l_auto_deborst = .true.
            algo1d = 'DEBORST'
            idbor = idbor+1
        endif
!
! ----- Get number of internal variables
!
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
!           NUMERO CORRESPONDANT AU NOM
            call jenonu(jexnom(rnomgf, zk24(img+ig-1)), ig1)
            if (ig1 .eq. 0) then
                call utmess('F', 'MODELISA8_8', sk=zk24(img+ig-1))
            endif
            icp=imk-1+(ig1-1)*6
            zk24(icp+1) = zk24(img+ig-1)
            zk24(icp+2) = materi
            zk24(icp+3) = rela_comp
            zk24(icp+4) = algo1d
            zk24(icp+5) = defo_comp
            write(zk24(icp+6),'(I24)') zi(jnfg-1+ig1)
            zi(iaff-1+ig1) = 1
        end do
!        ON MET À JOUR LE NOMBRE DE VARIABLES INTERNES MAXI
        nbvf=max(nbvf,nbv)
    end do
!
!     VERIFICATION DE L'UTILISATION DE COMP_1D
    if (nbocci .gt. 1) then
        if (idbor .ge. 1) then
            call utmess('F', 'COMPOR5_30')
        endif
    endif
!     VERIF TOUT AFFECTE AU MOINS UNE FOIS
!     ON MARQUE PAR VIDE LES GROUPES NON AFFECTES
    do ig = 1, nbgmax
        if (zi(iaff-1+ig) .eq. 0) then
            call jenuno(jexnum(rnomgf, ig), kgroup)
            icp=imk-1+(ig-1)*6
            zk24(icp+1) = kgroup
            zk24(icp+2) = 'VIDE'
        endif
    end do
    if (l_auto_deborst) then
        call utmess('I', 'COMPOR5_20')
    endif
!
!     ON RECUPERE LE NOM DU MATERIAU POUR LA TORSION, MIS A LA FIN
    call getvid(' ', 'MATER_SECT', scal=mator, nbret=ibid)
    zk24(imk-1+nbgmax*6+1)=mator
    call wkvect(sdcomp//'.CPRI', 'G V I', 3, imi)
!     TYPE 3 = MULTIFIBRE
    zi(imi) = 3
    zi(imi+1) = nbvf
    zi(imi+2) = nbgmax
!
    call jedema()
end subroutine
