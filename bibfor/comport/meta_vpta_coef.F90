subroutine meta_vpta_coef(rela_comp, lgpg       , fami     , kpg      , j_mater  ,&
                          l_temp   , temp       , meta_type, nb_phasis, phas_prev,&
                          phas_curr, zalpha_curr, young    ,  deuxmu  , coef     ,&
                          trans)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/get_meta_comp.h"
#include "asterfort/get_meta_plas_t.h"
#include "asterfort/get_meta_visc.h"
#include "asterfort/get_meta_mixd.h"
#include "asterfort/get_meta_hard.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=16), intent(in) :: rela_comp
    integer, intent(in) :: lgpg 
    character(len=4), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: j_mater
    logical, intent(in) :: l_temp
    real(kind=8), intent(in) :: temp
    integer, intent(in) :: meta_type
    integer, intent(in) :: nb_phasis
    real(kind=8), intent(in) :: phas_prev(*)
    real(kind=8), intent(in) :: phas_curr(*)
    real(kind=8), intent(in) :: zalpha_curr
    real(kind=8), intent(in) :: young
    real(kind=8), intent(in) :: deuxmu
    real(kind=8), intent(out) :: coef
    real(kind=8), intent(out) :: trans
!
! --------------------------------------------------------------------------------------------------
!
! Metallurgy - Comportment
!
! Compute coefficient for second member
! Effect of command variables phasis variation on transformation plasticity
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_comp     : comportment relation
! In  lgpg          : length of integration point
! In  fami          : integration point type
! In  kpg           : integration point number
! In  j_mater       : coded material address
! In  l_temp        : .true. if temperature command variable is affected
! In  temp          : temperature
! In  meta_type     : type of metallurgy
!                       0 - No metallurgy
!                       1 - Steel
!                       2 - Zirconium
! In  nb_phasis     : number of phasis
! In  phas_prev     : previous phasis
! In  phas_curr     : current phasis
! In  zalpha_curr   : sum of "cold" phasis
! In  young         : Young modulus
! In  deuxmu        : e/(1.d0+nu)
! Out coef          : coefficient for command variable second member
! Out trans         : transformation for command variable second member
!
! --------------------------------------------------------------------------------------------------
!
    integer :: j_vari
    integer :: i_phasis, ksp
    real(kind=8) :: epsp(5), r0(5)
    real(kind=8) :: kpt(4), fpt(4)
    real(kind=8) :: eta(5), n(5), unsurn(5), c(5), m(5)
    real(kind=8) :: rprim, deltaz, fmel
    logical :: l_visc, l_elas, l_plas_tran, l_hard_line
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(nb_phasis.le.4)
    trans     = 0.d0
    coef      = 1.d0
    ksp       = 1
    fpt(:)    = 0.d0
    kpt(:)    = 0.d0
    eta(:)    = 0.d0
    n(:)      = 0.d0
    unsurn(:) = 0.d0
    c(:)      = 0.d0
    m(:)      = 0.d0
    epsp(:)   = 0.d0
    r0(:)     = 0.d0
!
! - Cumulated plastic strain
!
    call jevech('PVARIPR', 'L', j_vari)
    do i_phasis = 1, nb_phasis+1
        epsp(i_phasis) = zr(j_vari+lgpg*(kpg-1)-1+i_phasis)
    end do
!
! - Is elastic ?
!
    l_elas = zr(j_vari+lgpg*(kpg-1)-1+nb_phasis+2).lt.0.5d0
!
! - Characteristics of comportment law
!
    call get_meta_comp(rela_comp,&
                       l_visc = l_visc,&
                       l_hard_line = l_hard_line, l_plas_tran = l_plas_tran)
!
! - Transformation plasticity parameters
!
    if (l_plas_tran) then
        call get_meta_plas_t('+'      , fami     , kpg      , ksp      , j_mater    ,&
                             meta_type, nb_phasis, phas_prev, phas_curr, zalpha_curr,&
                             kpt      , fpt)
    endif
!
! - Visco-plasticity parameters
!
    if (l_visc) then
        call get_meta_visc('+'      , fami     , kpg, ksp, j_mater,&
                           meta_type, nb_phasis, eta, n  , unsurn ,&
                           c        , m)
    endif
!
! - Compute Sum(iphase) [kpt * fpt]
!
    trans     = 0.d0
    do i_phasis = 1, nb_phasis
        deltaz = (phas_curr(i_phasis)-phas_prev(i_phasis))
        if (deltaz.gt.0) then
            trans = trans+kpt(i_phasis)*fpt(i_phasis)*deltaz
        endif
    end do
!
! - Compute coefficient
!
    if (l_elas.or.l_visc) then
        coef = 1.d0
    else
!
! ----- Mixing law: yield
!
        call get_meta_mixd('+'   , fami     , kpg      , ksp        , j_mater     ,&
                           l_visc, meta_type, nb_phasis, zalpha_curr, fmel  = fmel)
!
! ----- Get point on hardening curve
!
        call get_meta_hard('+'        , fami     , kpg      , ksp   , j_mater,&
                           l_hard_line, meta_type, nb_phasis, l_temp, temp   ,&
                           young      , epsp     , r0)
!
! ----- Compute coefficient
!
        rprim = 0.d0
        if (zalpha_curr .gt. 0.d0) then
            do i_phasis = 1, nb_phasis
                rprim = rprim + phas_curr(i_phasis)*r0(i_phasis)
            end do
            rprim = rprim/zalpha_curr
        else
            rprim = 0.d0
        endif
        rprim = (1.d0-fmel)*r0(nb_phasis+1)+fmel*rprim
        coef  = 1.d0-(1.5d0*deuxmu)/(1.5d0*deuxmu+rprim)
    endif
!
end subroutine
