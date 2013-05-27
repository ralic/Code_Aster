subroutine calcvh(nbmat, materf, eta, vp, sigeqe,&
                  vh, vg)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =====================================================================
    implicit      none
    integer :: nbmat
    real(kind=8) :: vh, vg, materf(nbmat, 2), eta, vp(3), sigeqe
! ======================================================================
! --- LOI DE HOEK BROWN :CALCUL DE TERMES DE LA FONCTION DE CHARGE (H/G)
! ======================================================================
! IN     NBMAT  NOMBRE DE DONNEES MATERIAU -----------------------------
! IN     MATERF DONNES MATERIAU ----------------------------------------
! IN     VP     VALEURS PROPRES DU DEVIATEUR ELASTIQUE -----------------
! IN/OUT VH     VALEUR DE LA FONCTION H --------------------------------
! IN/OUT VG     VALEUR DE LA FONCTION G --------------------------------
! ======================================================================
    real(kind=8) :: aux4, k, mu, un, trois
! ======================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( trois  =  3.0d0  )
! ======================================================================
    mu = materf(4,1)
    k = materf(5,1)
    aux4 = trois*mu/sigeqe
    vh = un/(eta+un)
    vg = vh*(trois*k*eta+aux4*vp(3))
! ======================================================================
end subroutine
