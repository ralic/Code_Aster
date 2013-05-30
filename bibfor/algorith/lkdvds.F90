subroutine lkdvds(dt, nbmat, mater, gv, dfdsv,&
                  seuilv, dvds)
!
    implicit    none
    include 'asterfort/lcprte.h'
    include 'asterfort/r8inir.h'
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), dvds(6, 6), dt
    real(kind=8) :: gv(6), dfdsv(6), seuilv
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
! --- BUT : DERIVEE DE L AMPLITUDE DES DEFORMATIONS IRREVERSIBLES -
! ----------PAR RAPPORT A DEPS
! =================================================================
! IN  : DT    :  PAS DE TEMPS -------------------------------------
! ----: NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
! ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
! --- : GV : GV=dfv/dsig-(dfv/dsig*n)*n ---------------------------
!-----: DFDSV :  DERIVEE DU CRITERE VISCOPLASTIQUE PAR RAPPORT A LA
!----------------CONTRAINTE
!-----: SEUILV:  SEUIL VISCOPLASTIQUE -----------------------------
! OUT : DVDS  :  DERIVEE DE DEPSV/ DSIG  --------------------------
! =================================================================
    common /tdim/   ndt , ndi
    integer :: i, k, ndi, ndt
    real(kind=8) :: pa, aa(6, 6), a, n, un, zero
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( un  = 1.0d0 )
    parameter       ( zero = 0.0d0)
! =================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
! =================================================================
    pa = mater(1,2)
    a = mater(21,2)
    n = mater(22,2)
! =================================================================
! --- MATRICE INTERMEDIAIRE ---------------------------------------
! =================================================================
    call r8inir(6*6, 0.d0, aa, 1)
    call lcprte(dfdsv, gv, aa)
!
! =================================================================
! --- CALCUL DE DPHI/DDEPS ------------------------------------
! =================================================================
!
    do 10 i = 1, ndt
        do 20 k = 1, ndt
            if (seuilv .le. zero) then
                dvds(i,k) = zero
            else
                dvds(i,k) = a * n /pa * (seuilv/pa)**(n-un)* aa(i,k)* dt
            endif
20      end do
10  end do
! =================================================================
end subroutine
