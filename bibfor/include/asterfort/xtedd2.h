! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine xtedd2(ndim, jnne, ndeple, jnnm, nddl,&
                      option, lesclx, lmaitx, lcontx, stano,&
                      lact, jddle, jddlm, nfhe, nfhm,&
                      lmulti, heavno, mmat, vtmp)
        integer, intent(in) :: ndim
        integer, intent(in) :: jnne(3)
        integer, intent(in) :: ndeple
        integer, intent(in) :: jnnm(3)
        integer, intent(in) :: nddl
        character(len=16), intent(in) :: option
        aster_logical, intent(in) :: lesclx
        aster_logical, intent(in) :: lmaitx
        aster_logical, intent(in) :: lcontx
        integer, intent(in) :: stano(*)
        integer, intent(in) :: lact(8)
        integer, intent(in) :: jddle(2)
        integer, intent(in) :: jddlm(2)
        integer, intent(in) :: nfhe
        integer, intent(in) :: nfhm
        aster_logical, intent(in) :: lmulti
        integer, intent(in) :: heavno(8)
        real(kind=8), optional, intent(out) :: mmat(336, 336)
        real(kind=8), optional, intent(out) :: vtmp(336)
    end subroutine xtedd2
end interface
