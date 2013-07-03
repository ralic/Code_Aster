subroutine lkcalg(dfdsig, vecn, g, devgii)
!
    implicit      none
#include "asterfort/lcdevi.h"
#include "asterfort/lcprsc.h"
#include "asterfort/r8inir.h"
    real(kind=8) :: dfdsig(6), vecn(6), g(6), devgii
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
! --- BUT : CALCUL DE G=df/dsig-(df/dsig*n)*n----------------------
! =================================================================
! IN  : DFDSIG : df/dsig ------------------------------------------
!        VECN  : --------------------------------------------------
! OUT : G : G=df/dsig-(df/dsig*n)*n -------------------------------
!      DEVGII : SECOND INVARIANT DE G -----------------------------
! =================================================================
    common /tdim/   ndt , ndi
    integer :: ndi, ndt, i
    real(kind=8) :: devg(6), fact1
!
! =================================================================
! --- CALCUL DE G -------------------------------------------------
! =================================================================
    call r8inir(6, 0.d0, g, 1)
!
    call lcprsc(dfdsig, vecn, fact1)
!
    do 10 i = 1, ndt
        g(i) = dfdsig(i) - fact1 * vecn(i)
10  end do
! =================================================================
! --- CALCUL DU DEVIATEUR DE G ET DE SA NORME ---------------------
! =================================================================
    call lcdevi(g, devg)
    call lcprsc(devg, devg, devgii)
    devgii = sqrt(devgii)
! =================================================================
end subroutine
