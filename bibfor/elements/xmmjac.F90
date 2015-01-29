subroutine xmmjac(alias, geom, dff, jac)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
    character(len=8) :: alias
    real(kind=8) :: dff(3, 9), geom(18), jac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEM GR GLISS - UTILITAIRE)
!
! CALCUL DU JACOBIEN D'UN ELEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
! IN  GEOM   : VECTEUR GEOMETRIE ACTUALISEE
! IN  DFF    : DERIVEES PREMIERES DES FONCTIONS DE FORME EN XI YI
! OUT JAC    : VALEUR DU JACOBIEN
!
!
!
!
    integer :: i
    real(kind=8) :: dxds, dyds, dzds
    real(kind=8) :: dxde, dxdk, dyde, dydk, dzde, dzdk
!
! ----------------------------------------------------------------------
!
    dxds = 0.d0
    dyds = 0.d0
    dzds = 0.d0
    dxde = 0.d0
    dyde = 0.d0
    dzde = 0.d0
    dxdk = 0.d0
    dydk = 0.d0
    dzdk = 0.d0
!
    if (alias(1:5) .eq. 'SE2') then
        do 10 i = 1, 2
            dxds = dxds + geom(2*(i-1)+1)*dff(1,i)
            dyds = dyds + geom(2*(i-1)+2)*dff(1,i)
10      continue
        jac = sqrt(dxds**2+dyds**2+dzds**2)
    else if (alias(1:5).eq.'SE3') then
        do 20 i = 1, 3
            dxds = dxds + geom(2*(i-1)+1)*dff(1,i)
            dyds = dyds + geom(2*(i-1)+2)*dff(1,i)
20      continue
        jac = sqrt(dxds**2+dyds**2+dzds**2)
    else if (alias(1:5).eq.'TR3') then
        do 30 i = 1, 3
            dxde = dxde + geom(3*i-2)*dff(1,i)
            dxdk = dxdk + geom(3*i-2)*dff(2,i)
            dyde = dyde + geom(3*i-1)*dff(1,i)
            dydk = dydk + geom(3*i-1)*dff(2,i)
            dzde = dzde + geom(3*i)*dff(1,i)
            dzdk = dzdk + geom(3*i)*dff(2,i)
30      continue
        jac = sqrt((dyde*dzdk-dzde*dydk)**2+ (dzde*dxdk-dxde*dzdk)**2+ (dxde*dydk-dyde*dxdk)**2)
    else if (alias(1:5).eq.'TR6') then
        do 40 i = 1, 6
            dxde = dxde + geom(3*i-2)*dff(1,i)
            dxdk = dxdk + geom(3*i-2)*dff(2,i)
            dyde = dyde + geom(3*i-1)*dff(1,i)
            dydk = dydk + geom(3*i-1)*dff(2,i)
            dzde = dzde + geom(3*i)*dff(1,i)
            dzdk = dzdk + geom(3*i)*dff(2,i)
40      continue
        jac = sqrt((dyde*dzdk-dzde*dydk)**2+ (dzde*dxdk-dxde*dzdk)**2+ (dxde*dydk-dyde*dxdk)**2)
    else
        ASSERT(.false.)
    endif
!
end subroutine
