subroutine dyna_comp_fuse(mesh, comp_noli, comp_fuse)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_elas.h"
#include "asterfort/comp_init.h"
#include "asterfort/carces.h"
#include "asterfort/cescar.h"
#include "asterfort/cesfus.h"
#include "asterfort/detrsd.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: comp_noli
    character(len=19), intent(in) :: comp_fuse
!
! --------------------------------------------------------------------------------------------------
!
! DYNA_VIBRA//HARM/GENE
!
! Fuse COMPOR <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh : name of mesh
! In  comp_noli : name of NOLI COMPOR <CARTE>
! In  comp_fuse : name of fuse of NOLI COMPOR <CARTE> and ELAS COMPOR <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nc
    parameter (nc = 2)
    character(len=19) :: chs(nc)
    aster_logical :: l_cumu(nc)
    real(kind = 8) :: coef_real(nc)
    complex(kind = 8) :: coef_cplx(nc)
!
    integer :: ibid, nb_cmp
    character(len=19) :: comp_elas
    character(len=19) :: comp_elas_s, comp_noli_s, comp_fuse_s
    aster_logical :: l_cplx, l_etat_init
!
    data l_cumu      /.false._1,.false./
    data coef_real   /1.d0, 1.d0/
!
! --------------------------------------------------------------------------------------------------
!
    comp_elas = '&&DYNA_COMP_ELAS'
    comp_elas_s = '&&DYNA_COMP_ELAS_S'
    comp_noli_s = '&&DYNA_COMP_NOLI_S'
    comp_fuse_s = '&&DYNA_COMP_FUSE_S'
    l_cplx = .false.
    l_etat_init = .false.
!
! - Create ELAS COMPOR <CARTE>
!
    call comp_init(mesh, comp_elas, 'V', nb_cmp)
    call comp_meca_elas(comp_elas, nb_cmp, l_etat_init)
!
! - Transform ELAS COMPOR <CARTE> in <CHAM_ELEM_S>
!
    call carces(comp_elas, 'ELEM', ' ', 'V', comp_elas_s,&
                'A', ibid)
!
! - Transform NOLI COMPOR <CARTE> in <CHAM_ELEM_S>
!
    call carces(comp_noli, 'ELEM', ' ', 'V', comp_noli_s,&
                'A', ibid)
!
! - Fuse the <CARTE>
!
    chs(1) = comp_elas_s
    chs(2) = comp_noli_s
    call cesfus(nc, chs, l_cumu, coef_real, coef_cplx,&
                l_cplx, 'V', comp_fuse_s)
!
! - Transform in <CARTE>
!
    call cescar(comp_fuse_s, comp_fuse, 'G')
!
    call detrsd('CHAMP', comp_elas)
    call detrsd('CHAMP', comp_elas_s)
    call detrsd('CHAMP', comp_fuse_s)
    call detrsd('CHAMP', comp_noli_s)
!
end subroutine
