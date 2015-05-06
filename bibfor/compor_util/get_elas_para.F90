subroutine get_elas_para(fami     , j_mater, poum, ipg, ispg, &
                         elas_type,&
                         time     , temp,&
                         e   , nu  , g,&
                         e1  , e2  , e3,&
                         nu12, nu13, nu23,&
                         g1  , g2  , g3)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/hypmat.h"
#include "asterfort/get_elas_type.h"
#include "asterfort/rcvalb.h"
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
    integer, intent(out) :: elas_type
    real(kind=8), optional, intent(in) :: time
    real(kind=8), optional, intent(in) :: temp
    real(kind=8), optional, intent(out) :: e
    real(kind=8), optional, intent(out) :: nu
    real(kind=8), optional, intent(out) :: g
    real(kind=8), optional, intent(out) :: e1
    real(kind=8), optional, intent(out) :: e2
    real(kind=8), optional, intent(out) :: e3
    real(kind=8), optional, intent(out) :: nu12
    real(kind=8), optional, intent(out) :: nu13
    real(kind=8), optional, intent(out) :: nu23
    real(kind=8), optional, intent(out) :: g1
    real(kind=8), optional, intent(out) :: g2
    real(kind=8), optional, intent(out) :: g3
!
! --------------------------------------------------------------------------------------------------
!
! Comportment utility
!
! Get elastic parameters
!
! --------------------------------------------------------------------------------------------------
!
! In  fami         : Gauss family for integration point rule
! In  j_mater      : coded material address
! In  time         : current time
! In  time         : current temperature
! In  poum         : '-' or '+' for parameters evaluation (previous or current temperature)
! In  ipg          : current point gauss
! In  ispg         : current "sous-point" gauss
! Out elas_type    : Type of elasticity
!                 1 - Isotropic
!                 2 - Orthotropic
!                 3 - Transverse isotropic
! Out e            : Young modulus (isotropic)
! Out nu           : Poisson ratio (isotropic)
! Out e1           : Young modulus - Direction 1 (Orthotropic/Transverse isotropic)
! Out e2           : Young modulus - Direction 2 (Orthotropic)
! Out e3           : Young modulus - Direction 3 (Orthotropic/Transverse isotropic)
! Out nu12         : Poisson ratio - Coupling 1/2 (Orthotropic/Transverse isotropic)
! Out nu13         : Poisson ratio - Coupling 1/3 (Orthotropic/Transverse isotropic)
! Out nu23         : Poisson ratio - Coupling 2/3 (Orthotropic)
! Out g1           : shear ratio (Orthotropic)
! Out g2           : shear ratio (Orthotropic)
! Out g3           : shear ratio (Orthotropic)
! Out g            : shear ratio (isotropic/Transverse isotropic)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbresm
    parameter (nbresm=9)
    integer :: icodre(nbresm)
    character(len=16) :: nomres(nbresm)
    real(kind=8) :: valres(nbresm)
!
    character(len=8) :: para_name(2)
    real(kind=8) :: para_vale(2)
    integer :: nbres, nb_para
    real(kind=8) :: c10, c01, c20, k
    real(kind=8) :: un
    character(len=16) :: elas_keyword
!
! --------------------------------------------------------------------------------------------------
!
    un             = 1.d0
    nb_para        = 0
    para_name(1:2) = ' '
    para_vale(1:2) = 0.d0
    if (present(time)) then
        nb_para   = nb_para + 1
        para_name(nb_para) = 'INST'
        para_vale(nb_para) = time
    endif
    if (present(temp)) then
        nb_para   = nb_para + 1
        para_name(nb_para) = 'TEMP'
        para_vale(nb_para) = temp
    endif
!
! - Get type of elasticity (Isotropic/Orthotropic/Transverse isotropic)
!
    call get_elas_type(j_mater, elas_type, elas_keyword)
!
    if (elas_type.eq.1) then
        if (elas_keyword.eq.'ELAS_HYPER') then
            call hypmat(fami, ipg, ispg, poum, j_mater,&
                        c10, c01, c20, k)
            nu = (3.d0*k-4.0d0*(c10+c01))/(6.d0*k+4.0d0*(c10+c01))
            if (present(e)) then
                e  = 4.d0*(c10+c01)*(un+nu)
            endif
        else
            nomres(1) = 'E'
            nomres(2) = 'NU'
            nbres     = 2
            call rcvalb(fami, ipg, ispg, poum, j_mater,&
                        ' ', elas_keyword, nb_para, para_name, [para_vale],&
                        nbres, nomres, valres, icodre, 1)
            if (present(e)) then
                e  = valres(1)
            endif
            nu = valres(2)
        endif
        if (present(g)) then
            ASSERT(present(nu))
            g = 1.d0/((1.d0+nu)*(1.d0-2.d0*nu))
        endif
    elseif (elas_type.eq.2) then
        nomres(1) = 'E_L'
        nomres(2) = 'E_T'
        nomres(3) = 'E_N'
        nomres(4) = 'NU_LT'
        nomres(5) = 'NU_LN'
        nomres(6) = 'NU_TN'
        nomres(7) = 'G_LT'
        nomres(8) = 'G_LN'
        nomres(9) = 'G_TN'
        nbres     = 9
        call rcvalb(fami, ipg, ispg, poum, j_mater,&
                    ' ', elas_keyword, nb_para, para_name, [para_vale],&
                    nbres, nomres, valres, icodre, 1)
        e1 = valres(1)
        e2 = valres(2)
        e3 = valres(3)
        nu12 = valres(4)
        nu13 = valres(5)
        nu23 = valres(6)
        g1 = valres(7)
        g2 = valres(8)
        g3 = valres(9)
    elseif (elas_type.eq.3) then
        nomres(1) = 'E_L'
        nomres(2) = 'E_N'
        nomres(3) = 'NU_LT'
        nomres(4) = 'NU_LN'
        nomres(5) = 'G_LN'
        nbres     = 5
        call rcvalb(fami, ipg, ispg, poum, j_mater,&
                    ' ', elas_keyword, nb_para, para_name, [para_vale],&
                    nbres, nomres, valres, icodre, 1)
        e1   = valres(1)
        e3   = valres(2)
        nu12 = valres(3)
        nu13 = valres(4)
        g    = valres(5)
    else
        ASSERT(.false.)
    endif

end subroutine
