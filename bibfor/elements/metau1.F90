subroutine metau1(l_meta)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/get_meta_type.h"
#include "asterfort/get_meta_phasis.h"
#include "asterfort/get_elas_para.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/verift.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    logical, intent(out) :: l_meta
!
! --------------------------------------------------------------------------------------------------
!
! Metallurgy
!
! Compute CHAR_MECA_TEMP_R - 2D case
!
! --------------------------------------------------------------------------------------------------
!
! Out l_meta : .true. if metallurgy exists
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbres
    parameter (nbres=2)
    character(len=8) :: nomres(nbres)
    integer :: icodre(nbres)
    real(kind=8) :: valres(nbres)  
!
    real(kind=8) :: zalpha, zalpha_comp
    real(kind=8) :: coef, coef1, coef2
    real(kind=8) :: young, nu
    real(kind=8) :: epsth, epsthe(2)
    real(kind=8) :: dfdx(9), dfdy(9)
    real(kind=8) :: poids, r
    real(kind=8) :: phasis(7) 
    integer :: nb_node, ispg, kp, npg, i_node, elas_type, k
    integer :: meta_type, nb_phasis
    integer :: ipoids, ivf, idfde
    integer :: j_geom, j_mate, j_mater, j_vect
!
! --------------------------------------------------------------------------------------------------
!
    l_meta    = .true.
    ispg      = 1
    nomres(1) = 'PHASE_REFE'(1:8)
    nomres(2) = 'EPSF_EPSC_TREF'(1:8)
!
! - Get metallurgy type
!
    call get_meta_type(meta_type, nb_phasis)
    if (meta_type.eq.0) then
        l_meta = .false.
        goto 999
    endif
!
! - Finite element informations
!
    call elrefe_info(fami='RIGI',nno=nb_node, npg=npg,&
                     jpoids=ipoids,jvf=ivf,jdfde=idfde)
!
! - Geometry
!
    call jevech('PGEOMER', 'L', j_geom)
!
! - Material parameters
! 
    call jevech('PMATERC', 'L', j_mate)
!
! - Coded material address
!
    j_mater = zi(j_mate)
!
! - Output field
!
    call jevech('PVECTUR', 'E', j_vect)
!
    do kp = 1, npg
!
        k = (kp-1)*nb_node
!
! ----- Shape functions derivatives
!
        call dfdm2d(nb_node, kp, ipoids, idfde, zr(j_geom),&
                    poids, dfdx, dfdy)
!
! ----- Get phasis
!
        call get_meta_phasis('RIGI'   , '+'   , kp    , ispg       , meta_type,&
                             nb_phasis, phasis, zalpha, zalpha_comp)
!
! ----- Axi-symmetric case
!
        if (lteatt('AXIS','OUI')) then
            r = 0.d0
            do i_node = 1, nb_node
                r = r + zr(j_geom+2* (i_node-1))*zr(ivf+k+i_node-1)
            end do
            poids = poids*r
            do i_node = 1, nb_node
                k = (kp-1)*nb_node
                dfdx(i_node) = dfdx(i_node) + zr(ivf+k+i_node-1)/r
            end do
        endif
!
! ----- Compute thermic strain
!
        call verift('RIGI', kp, 1, '+', j_mater,&
                    vepsth=epsthe)
!
! ----- Get elastic parameters
!
        call get_elas_para('RIGI', j_mater , '+', kp,&
                           ispg, elas_type,&
                           e  = young , nu = nu)
        ASSERT(elas_type.eq.1)
!
! ----- Get thermal parameters
!
        call rcvalb('RIGI', kp, ispg, '+', j_mater,&
                    ' ', 'ELAS_META', 0, ' ', [0.d0],&
                    2, nomres, valres, icodre, 1)
!
! ----- Compute
!
        coef  = young/(1.d0-2.d0*nu)
        coef1 = zalpha_comp* (epsthe(1)-(1-valres(1))*valres(2))
        coef2 = zalpha*      (epsthe(2)+valres(1)*valres(2))
        epsth = coef1 + coef2
        poids = poids*coef*epsth
!
        do i_node = 1, nb_node
            k = (kp-1)*nb_node
            zr(j_vect+2*i_node-2) = zr(j_vect+2*i_node-2) + poids*dfdx(i_node)
            zr(j_vect+2*i_node-1) = zr(j_vect+2*i_node-1) + poids*dfdy(i_node)
        end do
    end do
!
999 continue
end subroutine
