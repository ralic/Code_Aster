subroutine cfmxsd(mesh_      , model_     , nume_ddl        , list_func_acti  , sddyna,&
                  sdcont_defi, sdcont_solv, ligrel_link_cont, ligrel_link_xfem, sd_iden_rela)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfcrsd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmap.h"
#include "asterfort/cfmmci.h"
#include "asterfort/cfmmma.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfmxme.h"
#include "asterfort/cfmxr0.h"
#include "asterfort/infdbg.h"
#include "asterfort/wkvect.h"
#include "asterfort/xxmxme.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: mesh_
    character(len=*), intent(in) :: model_
    character(len=24), intent(in) :: nume_ddl
    integer, intent(in) :: list_func_acti(*)
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
    character(len=19), intent(in) :: ligrel_link_cont
    character(len=19), intent(in) :: ligrel_link_xfem
    character(len=24), intent(in) :: sd_iden_rela
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Prepare contact solving datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_ddl         : name of numbering object (NUME_DDL)
! In  mesh             : name of mesh
! In  model            : name of model
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
! In  list_func_acti   : list of active functionnalities
! In  ligrel_link_cont : name of LIGREL for contact
! In  ligrel_link_xfem : name of LIGREL for contact with xfem
! In  sd_iden_rela     : name of object for identity relations between dof
! In  sddyna           : name of dynamic solving datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=8) :: model, mesh
    integer :: zbouc, ztaco
    integer :: nb_cont_zone
    aster_logical :: l_cont_disc, l_cont_cont, l_cont_xfem, l_cont_mail, l_cont_allv
    character(len=14) :: nume_ddl_frot
    character(len=24) :: crnudd
    aster_logical, pointer :: v_crnudd(:) => null()
    character(len=24) :: maxdep
    real(kind=8), pointer :: v_maxdep(:) => null()
    character(len=24) :: nosdco
    character(len=24), pointer :: v_nosdco(:) => null()
    character(len=24) :: mboucl
    integer, pointer :: v_mboucl(:) => null()
    character(len=24) :: tabcof
    real(kind=8), pointer :: v_tabcof(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
!
! - Initializations
!
    nume_ddl_frot = '&&CFMXSD.NUMDF'
    nb_cont_zone  = cfdisi(sdcont_defi,'NZOCO')
    model = model_
    mesh  = mesh_
!
! - Contact method
!
    l_cont_mail = cfdisl(sdcont_defi,'FORMUL_MAILLEE')
    l_cont_xfem = cfdisl(sdcont_defi,'FORMUL_XFEM')
    l_cont_cont = cfdisl(sdcont_defi,'FORMUL_CONTINUE')
    l_cont_disc = cfdisl(sdcont_defi,'FORMUL_DISCRETE')
    l_cont_allv = cfdisl(sdcont_defi,'ALL_VERIF')
!
! - Create VALE_CONT datastructure
!
    call cfmxr0(sdcont_defi, sdcont_solv, mesh)
!
! - Create pairing datastructure
!
    if (l_cont_mail) then
        call cfmmap(mesh, sdcont_defi, sdcont_solv)
    endif
!
! - Create loop counters datastructure
!
    zbouc  = cfmmvd('ZBOUC')
    mboucl = sdcont_solv(1:14)//'.MBOUCL'
    call wkvect(mboucl, 'V V I', zbouc, vi = v_mboucl)
!
! - Create datastructure for datastructure names
!
    nosdco = sdcont_solv(1:14)//'.NOSDCO'
    call wkvect(nosdco, 'V V K24', 5, vk24 = v_nosdco)
    v_nosdco(1) = nume_ddl_frot
    v_nosdco(2) = ligrel_link_cont
    v_nosdco(3) = ligrel_link_xfem
    v_nosdco(4) = sd_iden_rela
!
! - Create datastructure for coefficients
!
    ztaco = cfmmvd('ZTACO')
    tabcof = sdcont_solv(1:14)//'.TABL.COEF'
    call wkvect(tabcof, 'V V R', nb_cont_zone*ztaco, vr = v_tabcof)
!
! - Init coefficients
!
    call cfmmci(sdcont_defi, sdcont_solv)
!
! - Create datastructure for renumbering flag
!
    if (l_cont_cont) then
        crnudd = sdcont_solv(1:14)//'.NUDD'
        call wkvect(crnudd, 'V V L', 1, vl = v_crnudd)
        if (l_cont_allv) then
            v_crnudd(1) = .false.
        else
            v_crnudd(1) = .true.
        endif
    endif
!
! - Create datastructure for geometric loop parameter
!
    maxdep = sdcont_solv(1:14)//'.MAXD'
    call wkvect(maxdep, 'V V R', 1, vr = v_maxdep)
    v_maxdep(1) = -1.d0
!
! - Create datastructures for solving
!
    if (.not.l_cont_allv) then
!
! ---- Print
!
        if (niv .ge. 2) then
            write (ifm,*) '<CONTACT> CREATION SD DE RESOLUTION'
        endif
!
! ---- Create datastructure for "meshed" method
!
        if (l_cont_mail) then
            call cfmmma(sdcont_defi, sdcont_solv)
        endif
!
! ---- Create datastructure for "DISCRET" method
!
        if (l_cont_disc) then
            call cfcrsd(mesh, nume_ddl, sdcont_defi, sdcont_solv)
        endif
!
! ---- Create datastructure for "CONTINUE" method
!
        if (l_cont_cont) then
            call cfmxme(nume_ddl, sddyna, sdcont_defi, sdcont_solv)
        endif
!
! ---- Create datastructure for "XFEM" method
!
        if (l_cont_xfem) then
            call xxmxme(mesh, model, list_func_acti, sdcont_defi, sdcont_solv)
        endif
!
    endif
!
end subroutine
