subroutine lkdepv(nbmat, mater, depsv, ddepsv, dgamv,&
                  ddgamv)
!
    implicit    none
#include "asterfort/lcdevi.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lctrma.h"
#include "asterfort/r8inir.h"
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), depsv(6), ddepsv(6)
    real(kind=8) :: dgamv, ddgamv(6)
! =================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --- BUT : DERIVEE DE LA DEFORMATION VISQUEUSE ET DU PARAMETRE
! ---- D ECROUISSAGE VISQUEUX
! =================================================================
! IN  : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
! ---         :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ---         :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
! --- : DT    : PAS DE TEMPS --------------------------------------
!     : DEPSV : DEFORMATIONS VISQUEUSES ---------------------------
! OUT : DDEPSV: DEFORMATIONS DEVIATORIQUES VISQUEUSES -------------
!     : DGAMV : PARAMETRE D ECROUISSAGE VISQUEUX ------------------
!     :DDGAMV : DERIVEE DU PARAMETRE D ECROUISSAGE VISQUEUX PAR  --
!     :         RAPPORT A DEPS ------------------------------------
! =================================================================
    common /tdim/   ndt , ndi
    integer :: i, k, ndi, ndt
    real(kind=8) :: zero, un, mun, deux, trois
    real(kind=8) :: devia(6, 6), deviat(6, 6)
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( zero    =  0.0d0   )
    parameter       ( un      =  1.0d0   )
    parameter       ( mun     = -1.0d0   )
    parameter       ( deux    =  2.0d0   )
    parameter       ( trois   =  3.0d0   )
! =================================================================
! --- CALCUL DU DEVIATEUR DU TENSEUR DES DEFORMATIONS VISQUEUSES -
! =================================================================
!
    call lcdevi(depsv, ddepsv)
!
! =================================================================
! --- CALCUL DE DGAMV ------------------------------------
! =================================================================
!
    dgamv = 0.d0
!
    do 20 i = 1, ndt
!
        dgamv = dgamv + ddepsv(i)**2
!
20  end do
    dgamv = sqrt(deux/trois * dgamv)
! =================================================================
! --- MATRICE DE PROJECTION DEVIATORIQUE --------------------------
! =================================================================
!
    call r8inir(6*6, 0.d0, devia, 1)
!
    do 30 i = 1, 3
        do 40 k = 1, 3
            devia(i,k) = mun/trois
40      end do
30  end do
!
    do 50 i = 1, ndt
        devia(i,i) = devia(i,i)+ un
50  end do
!
    call lctrma(devia, deviat)
! =================================================================
! --- CALCUL DE DERIVEE DE DGAMV/DEPS ---------------------------
! =================================================================
!
    call r8inir(6, 0.d0, ddgamv, 1)
!
!      SI PAS DE VISCOSITE DGAMV=0
    if (dgamv .eq. zero) then
        do 60 i = 1, ndt
            ddgamv(i) = zero
60      end do
    else
        call lcprmv(deviat, ddepsv, ddgamv)
        do 70 i = 1, ndt
            ddgamv(i) = deux/trois*ddgamv(i)/dgamv
70      end do
    endif
!
! =================================================================
end subroutine
