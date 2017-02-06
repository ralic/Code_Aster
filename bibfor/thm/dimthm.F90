subroutine dimthm(ndlno, ndlnm, ndim)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/lteatt.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    integer, intent(in)  :: ndim
    integer, intent(out) :: ndlno
    integer, intent(out) :: ndlnm
!
! --------------------------------------------------------------------------------------------------
!
! THM - Initializations
!
! Get number of dof on boundary
!
! --------------------------------------------------------------------------------------------------
!
!     NDDLNO NOMBRE DE DDL DES NOEUDS EXTREMITE DE SEGMENTS
!     NDDLM  NOMBRE DE DDL DES NOEUDS MILIEU DE SEGMENTS OU FACE
!
! --------------------------------------------------------------------------------------------------
!
    ndlno = 0
    ndlnm = 0
    if (lteatt('TYPMOD3','SUSHI')) then
        ndlnm = 2
    else
        if (lteatt('MECA','OUI')) then
            ndlnm = ndim
        endif
    endif
!
    if (lteatt('TYPMOD3','SUSHI')) then
        ndlno = 0
    else
        if (lteatt('MECA','OUI')) then
            ndlno = ndim
        endif
        if (lteatt('THER','OUI')) then
            ndlno = ndlno + 1
        endif
        if (lteatt('HYDR1','1') .or. lteatt('HYDR1','2')) then
            ndlno = ndlno + 1
        endif
        if (lteatt('HYDR2','1') .or. lteatt('HYDR2','2')) then
            ndlno = ndlno + 1
        endif
    endif
!
end subroutine
