subroutine hujpic(kk, k, tin, vin, mater,&
                  yf, pc)
    implicit none
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
!  -----------------------------------------------------------
!  LOI DE HUJEUX: CALCUL DE LA PRESSION ISOTROPE CYCLIQUE
!  IN  KK       :  NUMERO D'ORDRE DU MECANISME (1 A 4)
!      K        :  MECANISME K = 8
!      TIN( )   :  CHAMPS DE CONTRAINTES
!      VIN      :  VARIABLES INTERNES ASSOCIEES
!      MATER    :  COEFFICIENT MATERIAU
!      YF       :  VECTEUR SOLUTION DU SYSTEME DE NEWTON LOCAL
!
!  OUT
!      PC     :  PRESSION ISOTROPE CYCLIQUE
!  -----------------------------------------------------------
    integer :: ndt, ndi, i, k, kk, nmod
    parameter     (nmod = 15)
!
    real(kind=8) :: vin(*), d, x4
    real(kind=8) :: tin(ndt), pc, d13, zero
    real(kind=8) :: epsvp, beta, pcref, pcr
    real(kind=8) :: i1, mater(22, 2), yf(nmod)
!
    common /tdim/ ndt  , ndi
!
    data   d13, zero /0.333333333334D0, 0.d0/
!
! ==================================================================
! --- VARIABLES INTERNES -------------------------------------------
! ==================================================================
!
    epsvp = yf(7)
    x4 = vin(21)
!
! ==================================================================
! --- CARACTERISTIQUES MATERIAU ------------------------------------
! ==================================================================
!
    beta = mater(2, 2)
    d = mater(3, 2)
    pcref = mater(7, 2)
    pcr = pcref*exp(-beta*epsvp)
!
! ======================================================================
! ----------------- CONSTRUCTION PRESSION ISOTROPE ---------------------
! ======================================================================
!
    i1 = zero
    do 10 i = 1, ndi
        i1=i1+d13*tin(i)
10  continue
!
! ======================================================================
! ------------ CONSTRUCTION PRESSION ISOTROPE CYCLIQUE -----------------
! ======================================================================
!
    pc = abs(i1)+d*pcr*x4
!
end subroutine
