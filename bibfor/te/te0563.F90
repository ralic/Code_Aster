subroutine te0563(option, nomte)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
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
! aslint: disable=W0104
!
    character(len=16), intent(in) :: option
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Options: NORME_L2
!          NORME_FROB
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ncmp_coor, ncmp_vale, ncmp_coef, npg, iret
    integer :: j_coor_elga, j_vale, j_coef, j_resu, j_calc
    integer :: ndim, nno, nnos, ivf, idfdx, jgano, jtab1(2), jtab2(2)
    integer :: jtab3(2), ibid, ipg, icmp
    real(kind=8) :: resu, poids_pg, vale_pg, calc_elem
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(option.eq.'NORME_L2' .or. option.eq.'NORME_FROB')
!
! - Finite element parameters
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ibid, ivf, idfdx, jgano)
!
! - Input fields
!
    call jevech('PCOORPG', 'L', j_coor_elga)
    call jevech('PCHAMPG', 'L', j_vale)
    call jevech('PCOEFR' , 'L', j_coef)
    call jevech('PCALCI' , 'L', j_calc)
!
! - Output fields
!
    call jevech('PNORME', 'E', j_resu)
!
! - Informations of input fields
!
    call tecach('OON', 'PCHAMPG', 'L', 2, jtab1,&
                iret)
    ncmp_vale = jtab1(2)/npg
!
    call tecach('OON', 'PCOORPG', 'L', 2, jtab2,&
                iret)
    ncmp_coor = jtab2(2)/npg
!
    call tecach('OON', 'PCOEFR', 'L', 2, jtab3,&
                iret)
    ncmp_coef = jtab3(2)
!
    ASSERT(ncmp_coef .ge. ncmp_vale)
!
! - Sum on Gauss points of Norm * Norm
!
    resu = 0.d0
    do ipg = 1, npg
        poids_pg = zr(j_coor_elga + ncmp_coor*(ipg-1)+ncmp_coor-1)
        vale_pg  = 0.d0
        do icmp = 1, ncmp_vale
            vale_pg = vale_pg + zr(j_coef-1+icmp) * &
                      zr(j_vale-1+ncmp_vale*(ipg-1)+icmp) * &
                      zr(j_vale-1+ncmp_vale*(ipg-1)+icmp)
        enddo
        resu = resu + vale_pg * poids_pg
    enddo

    calc_elem = zi(j_calc)
  
    if (calc_elem.lt.0) then
        zr(j_resu) = resu
    else
        zr(j_resu) = sqrt(resu)
    endif
!
end subroutine
