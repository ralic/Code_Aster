!
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
    subroutine cafmsu(ifa, cont, tange, maxfa, nface,&
                      fkss, dfks1, dfks2, mobfas, dmob1s,&
                      dmob2s, fmw, fm1w, fm2w)
        integer :: nface
        integer :: maxfa
        integer :: ifa
        aster_logical :: cont
        aster_logical :: tange
        real(kind=8) :: fkss
        real(kind=8) :: dfks1(1+maxfa, nface)
        real(kind=8) :: dfks2(1+maxfa, nface)
        real(kind=8) :: mobfas
        real(kind=8) :: dmob1s
        real(kind=8) :: dmob2s
        real(kind=8) :: fmw(nface)
        real(kind=8) :: fm1w(1+maxfa, nface)
        real(kind=8) :: fm2w(1+maxfa, nface)
    end subroutine cafmsu
end interface
