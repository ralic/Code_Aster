subroutine nmextr_comp(field     , field_disc, field_type     , meshz    , modelz   ,&
                       cara_elemz, matez     , ds_constitutive, disp_curr, strx_curr,&
                       varc_curr , varc_refe , time           , ligrelz)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/inical.h"
#include "asterfort/megeom.h"
#include "asterfort/mecara.h"
#include "asterfort/meharm.h"
#include "asterfort/mecact.h"
!
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
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: field
    character(len=24), intent(in) :: field_type
    character(len=4), intent(in) :: field_disc
    character(len=*), intent(in) :: modelz
    character(len=*), intent(in) :: meshz
    character(len=*), intent(in) :: cara_elemz
    character(len=*), intent(in) :: matez
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    character(len=*), intent(in) :: disp_curr
    character(len=*), intent(in) :: strx_curr
    character(len=*), intent(in) :: varc_curr
    character(len=*), intent(in) :: varc_refe
    real(kind=8), intent(in) :: time
    character(len=*), optional, intent(in) :: ligrelz
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Compute fields when not a default in nonlinear operator
!
! ONLY EPSI_ELGA !
!
! --------------------------------------------------------------------------------------------------
!
! In  field            : name of field
! In  field_disc       : localization of field (discretization: NOEU or ELGA)
! In  field_type       : type of field (name in results datastructure)
! In  model            : name of model
! In  mesh             : name of mesh
! In  cara_elem        : name of datastructure for elementary parameters (CARTE)
! In  mate             : name of material characteristics (field)
! In  ds_constitutive  : datastructure for constitutive laws management
! In  disp_curr        : current displacements
! In  varc_curr        : command variable for current time
! In  varc_refe        : command variable for reference
! In  time             : current time
! In  strx_curr        : fibers information for current time
! In  ligrel           : current LIGREL (if not present: on all model)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=15)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=24) :: chgeom, chcara(18), chharm, chtime
    integer :: n_harm
    character(len=19) :: ligrel
    character(len=16) :: option
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(field_type.eq.'EPSI_ELGA')
    ASSERT(field_disc.eq.'ELGA')
    option = 'EPSI_ELGA'
    chtime = '&&NMEXTR_COMP.CHTIME'
    chharm = '&&NMEXTR_COMP.CHHARM'
    if (present(ligrelz)) then
        ligrel = ligrelz
    else
        ligrel = modelz(1:8)//'.MODELE'
    endif
    n_harm = 0
!
! - Time field
!
    call mecact('V', chtime, 'MAILLA', meshz, 'INST_R',&
                ncmp=1, nomcmp='INST', sr=time)
!
! - Geometry field
!
    call megeom(modelz, chgeom)
!
! - Elementary characteristics fields
!
    call mecara(cara_elemz, chcara)
!
! - Fourier field
!
    call meharm(modelz, n_harm, chharm)
!
! - Init fields
!
    call inical(nbin  , lpain, lchin, nbout, lpaout,&
                lchout)
!
! - Input fields
!
    lpain(1)  = 'PGEOMER'
    lchin(1)  = chgeom(1:19)
    lpain(2)  = 'PDEPLAR'
    lchin(2)  = disp_curr(1:19)
    lpain(3)  = 'PMATERC'
    lchin(3)  = matez(1:19)
    lpain(4)  = 'PTEMPSR'
    lchin(4)  = chtime(1:19)
    lpain(5)  = 'PVARCPR'
    lchin(5)  = varc_curr(1:19)
    lpain(6)  = 'PVARCRR'
    lchin(6)  = varc_refe(1:19)
    lpain(7)  = 'PCACOQU'
    lchin(7)  = chcara(7)(1:19)
    lpain(8)  = 'PCOMPOR'
    lchin(8)  = ds_constitutive%compor(1:19)
    lpain(9)  = 'PCAGEPO'
    lchin(9)  = chcara(5)(1:19)
    lpain(10) = 'PCAORIE'
    lchin(10) = chcara(1)(1:19)
    lpain(11) = 'PNBSP_I'
    lchin(11) = chcara(16)(1:19)
    lpain(12) = 'PFIBRES'
    lchin(12) = chcara(17)(1:19)
    lpain(13) = 'PCAMASS'
    lchin(13) = chcara(12)(1:19)
    lpain(14) = 'PHARMON'
    lchin(14) = chharm(1:19)
    lpain(15) = 'PSTRXMR'
    lchin(15) = strx_curr(1:19)
!
! - Output field
!
    lpaout(1) = 'PDEFOPG'
    lchout(1) = field
!
! - Computation
!
    call calcul('S'  , option, ligrel, nbin  , lchin,&
                lpain, nbout , lchout, lpaout, 'V'  ,&
                'OUI')
!
end subroutine
