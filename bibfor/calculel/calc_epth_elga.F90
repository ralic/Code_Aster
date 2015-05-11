subroutine calc_epth_elga(fami   , ndim  , poum  , kpg  , ksp,&
                          j_mater, xyzgau, repere, epsi_ther)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/matrot.h"
#include "asterfort/get_elas_type.h"
#include "asterfort/get_meta_phasis.h"
#include "asterfort/get_meta_type.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utrcyl.h"
#include "asterfort/verift.h"
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
    character(len=*), intent(in) :: fami
    integer, intent(in) :: ndim
    character(len=*), intent(in) :: poum
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    integer, intent(in) :: j_mater
    real(kind=8), intent(in) :: xyzgau(3)
    real(kind=8), intent(in) :: repere(7)
    real(kind=8), intent(out) :: epsi_ther(6)
!
! --------------------------------------------------------------------------------------------------
!
! Compute thermic strains
! 
! For isoparametric elements
!
! --------------------------------------------------------------------------------------------------
!
! In  fami         : Gauss family for integration point rule
! In  ndim         : dimension of space
! In  poum         : parameters evaluation 
!                     '-' for previous temperature
!                     '+' for current temperature
!                     'T' for current and previous temperature
! In  kpg          : current point gauss
! In  ksp          : current "sous-point" gauss
! In  j_mater      : coded material address
! In  xyzgau       : coordinates of current Gauss point
! In  repere       : definition of basis (for non-isotropic materials)
! Out epsi_ther    : thermal strain tensor
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: elem_name
    character(len=16) :: elas_keyword, rela_comp
    character(len=32) :: valk(2)
    integer :: elas_type, meta_type, nb_phasis, iret
    real(kind=8) :: angl(3), epsthe(3), phase(2)
    real(kind=8) :: dire(3), orig(3), p_glob_loca(3, 3), epsi_ther_vect(6), epsi_ther_scal
    real(kind=8) :: vepst1(6), vepst2(6), zcold, zhot
    integer :: iadzi, iazk24, icompo
!
! --------------------------------------------------------------------------------------------------
!
    epsi_ther(1:6) = 0.d0
!
! - Get elasticity type
!
    call get_elas_type(j_mater, elas_type, elas_keyword)
    ASSERT(elas_type.le.3)
!
! - Comportment
!
    call tecach('NNN', 'PCOMPOR', 'L', iret, iad=icompo)
    if (iret.eq.0) then
        rela_comp  = zk16(icompo)
    else
        rela_comp = 'Unknown'
    endif
!
! - Strains for metallurgical laws: not possible except META_LEMA_ANI
!
    if (elas_keyword.eq.'ELAS_META') then
        if (rela_comp.ne.'META_LEMA_ANI') then
            call tecael(iadzi, iazk24)
            elem_name = zk24(iazk24-1+3) (1:8)
            valk(1) = elem_name
            valk(2) = 'EPVC_ELGA'
            call utmess('F', 'COMPOR5_11', nk = 2, valk = valk)
        endif
    endif
!
! - Non-isotropic elasticity: prepare basis
!
    if (elas_type.gt.1)  then
        if (repere(1) .gt. 0.d0) then
            angl(1) = repere(2)
            angl(2) = repere(3)
            angl(3) = repere(4)
            call matrot(angl, p_glob_loca)
        else
            dire(1) = repere(2)
            dire(2) = repere(3)
            dire(3) = repere(4)
            orig(1) = repere(5)
            orig(2) = repere(6)
            orig(3) = repere(7)
            call utrcyl(xyzgau, dire, orig, p_glob_loca)
        endif
    endif
!
! - Compute (local) thermic strains
!
    if (elas_type.eq.1) then
        if (rela_comp.eq.'META_LEMA_ANI') then
            call get_meta_type(meta_type, nb_phasis)
            ASSERT(nb_phasis.eq.2)
            call get_meta_phasis(fami     , '+' , kpg   , ksp , meta_type,&
                                 nb_phasis, phase, zcold_ = zcold, zhot_ = zhot)
            call verift(fami, kpg, ksp, '+', j_mater,&
                        vepsth=epsthe)
            epsi_ther_scal = zhot*epsthe(1) + zcold*epsthe(2)
        else
            call verift(fami, kpg, ksp, poum, j_mater,&
                        epsth = epsi_ther_scal)
        endif        
        epsi_ther(1) = epsi_ther_scal
        epsi_ther(2) = epsi_ther_scal
        epsi_ther(3) = epsi_ther_scal
    else if (elas_type.eq.2)  then
        call verift(fami, kpg, ksp, poum, j_mater,&
                    vepsth=epsi_ther_vect)
        epsi_ther_vect(4) = 0.d0
        epsi_ther_vect(5) = 0.d0
        epsi_ther_vect(6) = 0.d0
    else if (elas_type.eq.3) then
        call verift(fami, kpg, ksp, poum, j_mater,&
                    vepsth=epsi_ther_vect)
        epsi_ther_vect(3) = epsi_ther_vect(2)
        epsi_ther_vect(2) = epsi_ther_vect(1)
        epsi_ther_vect(4) = 0.d0
        epsi_ther_vect(5) = 0.d0
        epsi_ther_vect(6) = 0.d0
    else
        ASSERT(.false.)
    endif
!
! - Non-isotropic elasticity: rotate strains
!
    if (elas_type.gt.1)  then
        vepst1(1)=epsi_ther_vect(1)
        vepst1(2)=epsi_ther_vect(4)
        vepst1(3)=epsi_ther_vect(2)
        vepst1(4)=epsi_ther_vect(5)
        vepst1(5)=epsi_ther_vect(6)
        vepst1(6)=epsi_ther_vect(3)
        call utpslg(1, 3, p_glob_loca, vepst1, vepst2)
        epsi_ther(1)=vepst2(1)
        epsi_ther(2)=vepst2(3)
        epsi_ther(3)=vepst2(6)
        epsi_ther(4)=vepst2(2)
        epsi_ther(5)=vepst2(4)
        epsi_ther(6)=vepst2(5)
        if (ndim .eq. 2) epsi_ther(3) = epsi_ther_vect(3)
    endif
!
end subroutine
