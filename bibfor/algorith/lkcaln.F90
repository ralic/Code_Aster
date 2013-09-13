subroutine lkcaln(s, b, vecn, retcom)
!
    implicit none
#include "asterc/r8miem.h"
#include "asterfort/lcprsc.h"
#include "asterfort/utmess.h"
    integer :: retcom
    real(kind=8) :: b, s(6), vecn(6)
! =================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =================================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
! =================================================================
! --- BUT : CALCUL DE N -------------------------------------------
! =================================================================
! IN  : S      : DEVIATEUR DES CONTRAINTES ------------------------
! --- : B      : PARAMETRE DU CALCUL DE LA NORMALE ----------------
! OUT : VECN   : N = (B*S/SII-I)/SQRT(B**2+3) ---------------------
! =================================================================
    integer :: i, ndt, ndi
    real(kind=8) :: sii, racine, un, trois, kron(6), zero
    real(kind=8) :: ptit
! =================================================================
! --- INITIALISATION DE PARAMETRE ---------------------------------
! =================================================================
    parameter       ( un      =   1.0d0  )
    parameter       ( trois   =   3.0d0  )
    parameter       ( zero    =   0.0d0  )
! =================================================================
    common /tdim/   ndt , ndi
! =================================================================
    data    kron    /un     ,un     ,un     ,zero   ,zero  ,zero/
! --- INITIALISATION ----------------------------------------------
! =================================================================
    retcom = 0
    ptit = r8miem()
    call lcprsc(s, s, sii)
    sii = sqrt (sii)
    if (sii .lt. ptit) then
        call utmess('A', 'COMPOR1_31')
        retcom = 1
        goto 1000
    endif
! =================================================================
! --- CALCUL DE N -------------------------------------------------
! =================================================================
    racine = sqrt(b*b + trois)
    do 10 i = 1, ndt
        vecn(i) = (b*s(i)/sii-kron(i))/racine
10  end do
! =================================================================
1000  continue
end subroutine
