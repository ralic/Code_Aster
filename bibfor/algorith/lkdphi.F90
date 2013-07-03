subroutine lkdphi(nbmat, mater, de, seuilv, dfdsv,&
                  dphi)
!
    implicit    none
#include "asterfort/lcprmv.h"
#include "asterfort/r8inir.h"
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), de(6, 6)
    real(kind=8) :: dphi(6), dfdsv(6), seuilv
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
! --- BUT : DERIVEE DE L AMPLITUDE DES DEFORMATIONS IRREVERSIBLES -
! ----------PAR RAPPORT A DEPS
! =================================================================
! IN  : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
! ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
!-----: DE    :  MATRICE HYPOELASTIQUE ----------------------------
!-----: SEUILV:  SEUIL VISCOPLASTIQUE -----------------------------
!-----: DFDSV :  DERIVEE DU CRITERE VISCOPLASTIQUE PAR RAPPORT A LA
!----------------CONTRAINTE
! OUT : DPHI  :  AMPLITUDE DES DEFORMATIONS IRREVERSIBLES  --------
!       DPHI   = A*n/PA*(fv(SIG,XIV)/PA)**(n-1).dfv/dsig*De
! =================================================================
    common /tdim/   ndt , ndi
    integer :: i, ndi, ndt
    real(kind=8) :: un, zero
    real(kind=8) :: pa, a, n, aa(6)
    parameter       (un     =  1.0d0  )
    parameter       (zero   =  0.0d0  )
! =================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
! =================================================================
    pa = mater(1,2)
    a = mater(21,2)
    n = mater(22,2)
!
! =================================================================
! --- MATRICE INTERMEDIAIRE ---------------------------------------
! =================================================================
    call r8inir(6, 0.d0, aa, 1)
!
    call lcprmv(de, dfdsv, aa)
!
! =================================================================
! --- CALCUL DE DPHI/DDEPS ------------------------------------
! =================================================================
    do 10 i = 1, ndt
        if (seuilv .le. zero) then
            dphi(i) = zero
        else
            dphi(i) = a * n /pa * (seuilv/pa)**(n-un)*aa(i)
        endif
10  end do
! =================================================================
end subroutine
