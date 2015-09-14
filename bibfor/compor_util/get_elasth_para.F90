subroutine get_elasth_para(fami     , j_mater     , poum   , ipg       , ispg,&
                           elas_type, elas_keyword, materi_, temp_vale_, &
                           alpha    , alpha_l     , alpha_t, alpha_n)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
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
    integer, intent(in) :: j_mater
    character(len=*), intent(in) :: poum
    integer, intent(in) :: ipg
    integer, intent(in) :: ispg
    integer, intent(in) :: elas_type
    character(len=16), intent(in) :: elas_keyword
    character(len=8), optional, intent(in) :: materi_
    real(kind=8), optional, intent(in) :: temp_vale_
    real(kind=8), optional, intent(out) :: alpha(2)
    real(kind=8), optional, intent(out) :: alpha_l
    real(kind=8), optional, intent(out) :: alpha_t
    real(kind=8), optional, intent(out) :: alpha_n
!
! --------------------------------------------------------------------------------------------------
!
! Comportment utility
!
! Get elastic parameters for thermic dilatation
!
! --------------------------------------------------------------------------------------------------
!
! In  fami         : Gauss family for integration point rule
! In  j_mater      : coded material address
! In  poum         : '-' or '+' for parameters evaluation (previous or current temperature)
! In  ipg          : current point gauss
! In  ispg         : current "sous-point" gauss
! In  elas_type    : Type of elasticity
!                       1 - Isotropic
!                       2 - Orthotropic
!                       3 - Transverse isotropic
! In  elas_keyword : keyword factor linked to type of elasticity parameters
! In  materi       : name of material if multi-material Gauss point (PMF)
! In  temp_vale    : specifi temperature (example: mean temperature for structural elements)
! Out alpha        : thermic dilatation ratio (isotropic)
!                     if   META -> alpha(1) for hot phasis and alpha(2) for cold phasis
!                     else alpha(1) only
! Out alpha_l      : thermic dilatation ratio - Direction L (Orthotropic/Transverse isotropic)
! Out alpha_t      : thermic dilatation ratio - Direction T (Orthotropic)
! Out alpha_n      : thermic dilatation ratio - Direction N (Orthotropic/Transverse isotropic)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbresm
    parameter (nbresm=3)
    integer :: icodre(nbresm)
    character(len=16) :: nomres(nbresm)
    real(kind=8) :: valres(nbresm)
!
    character(len=8) :: para_name, materi
    character(len=8) :: valk(2)
    real(kind=8) :: para_vale
    integer :: nbres, nb_para, i
    real(kind=8) :: alpha_c, alpha_f, alpha_a
    integer :: iadzi, iazk24
!
! --------------------------------------------------------------------------------------------------
!
    nb_para   = 0
    para_name = ' '
    para_vale = 0.d0
    materi    = ' '
    if (present(materi_)) then
        materi    = materi_
    endif
    if (present(temp_vale_)) then
        nb_para   = 1
        para_vale = temp_vale_
        para_name = 'TEMP'
    endif
!
! - Get parameters
!
    if (elas_type.eq.1) then
        if (elas_keyword.eq.'ELAS_HYPER') then
            call utmess('F','COMPOR5_6')
        elseif (elas_keyword.eq.'ELAS_META') then
            nbres     = 2
            nomres(1) = 'C_ALPHA'
            nomres(2) = 'F_ALPHA'
            call rcvalb(fami  , ipg, ispg, poum, j_mater,&
                        materi, elas_keyword, nb_para, para_name, [para_vale],&
                        nbres , nomres, valres, icodre, 1)
            alpha_c   = valres(1)
            alpha_f   = valres(2)
            alpha(1)  = alpha_c
            alpha(2)  = alpha_f
        else
            nbres     = 1
            nomres(1) = 'ALPHA'
            call rcvalb(fami  , ipg, ispg, poum, j_mater,&
                        materi, elas_keyword, nb_para, para_name, [para_vale],&
                        nbres , nomres, valres, icodre, 1)
            alpha_a   = valres(1)
            alpha(1)  = alpha_a
            alpha(2)  = 0.d0
        endif
    elseif (elas_type.eq.2) then
        nbres     = 3
        nomres(1) = 'ALPHA_L'
        nomres(2) = 'ALPHA_T'
        nomres(3) = 'ALPHA_N'
        call rcvalb(fami  , ipg, ispg, poum, j_mater,&
                    materi, elas_keyword, nb_para, para_name, [para_vale],&
                    nbres , nomres, valres, icodre, 1)
        alpha_l = valres(1)
        alpha_t = valres(2)
        alpha_n = valres(3)
    elseif (elas_type.eq.3) then
        nbres     = 2
        nomres(1) = 'ALPHA_L'
        nomres(2) = 'ALPHA_N'
        call rcvalb(fami , ipg, ispg, poum, j_mater,&
                    materi, elas_keyword, nb_para, para_name, [para_vale],&
                    nbres , nomres, valres, icodre, 1)
        alpha_l = valres(1)
        alpha_n = valres(2)
    else
        ASSERT(.false.)
    endif
!
! - Test
!
    do i = 1, nbres
        if (icodre(i).ne.0) then
            call tecael(iadzi, iazk24)
            valk(1) = zk24(iazk24-1+3) (1:8)
            valk(2) = nomres(i)
            call utmess('F', 'COMPOR5_32', nk=2, valk=valk)
        endif
    end do
!
end subroutine
