subroutine lkelas(ndi, ndt, mod, nmat, mater,&
                  deps, sigd, de, k, mu)
    implicit none
    include 'asterfort/r8inir.h'
    include 'asterfort/trace.h'
    integer :: ndi, ndt, nmat
    real(kind=8) :: mater(nmat, 2)
    real(kind=8) :: sigd(6)
    real(kind=8) :: deps(6), de(6, 6), mu, k
    character(len=8) :: mod
! =================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!       MATRICE  HYPOELASTIQUE
!       IN  NDI    :  DIMENSION DE L ESPACE
!           NDT    :  2* NDI POUR LE CALCUL TENSORIEL
!           MOD    :  MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATER :  COEFFICIENTS MATERIAU
!           DEPS   :  INCREMENT DE DEFORMATION
!           SIGD   :  CONTRAINTE  A T
!       OUT DE     :  MATRICE HYPOELASTIQUE
!            K     :  MODULE DE COMPRESSIBILITE
!            G     :  MODULE DE CISAILLEMENT
!       -----------------------------------------------------------
    integer :: i, j
    real(kind=8) :: mue, ke, pa, nelas
    real(kind=8) :: invar1
    real(kind=8) :: deux, trois
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( deux   =  2.0d0  )
    parameter       ( trois  =  3.0d0  )
! =================================================================
! --- CALCUL DU PREMIER INVARIANT DES CONTRAINTES A LINSTANT T ----
! =================================================================
    invar1 = trace (ndi, sigd)
! =================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
! =================================================================
    mue = mater(4,1)
    ke = mater(5,1)
!
    pa = mater(1,2)
    nelas = mater(2,2)
!
! =================================================================
! --- CALCUL DES PARAMETRES AU TEMPS T + DT -----------------------
! =================================================================
    k = ke * (invar1/trois/pa)**nelas
    mu = mue * (invar1/trois/pa)**nelas
! =================================================================
! --- DEFINITION DE LA MATRICE HYPOLELASTIQUE ---------------------
! =================================================================
    call r8inir(6*6, 0.d0, de, 1)
    do 10 i = 1, 3
        do 20 j = 1, 3
            de(i,j) = k - deux*mu/trois
20      continue
10  continue
!
    do 30 i = 1, ndt
        de(i,i) = de(i,i) + deux*mu
30  continue
!
! =================================================================
end subroutine
