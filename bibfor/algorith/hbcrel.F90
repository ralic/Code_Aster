subroutine hbcrel(vp, gamma, dg, nbmat, materf,&
                  sigeqe, i1e, etap, parame, seuil)
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
! =====================================================================
    implicit      none
    integer :: nbmat
    real(kind=8) :: vp(3), gamma, materf(nbmat, 2), sigeqe, i1e, seuil, dg, etap
    real(kind=8) :: parame(4)
! ======================================================================
! --- HOEK BROWN : CALCUL DU CRITERE PLASTIQUE F(SIGE,DG) --------------
! ======================================================================
! IN  VP     VALEURS PROPRES DU DEVIATEUR DE SIGMA ---------------------
! IN  GAMMA  VALEUR DE LA VARIABLE D ECROUISSAGE GAMMA_PLUS ------------
! IN  DG     INCREMENT DE GAMMA ----------------------------------------
! IN  NBMAT  NOMBRE DE DONNEES MATERIAU -------------------------------
! IN  MATERF DONNEES MATERIAU ------------------------------------------
! IN  ETAP   VALEUR DE ETA A GAMMA_PLUS --------------------------------
! IN  PARAME VALEUR DES PARAMETRES D ECROUISSAGE -----------------------
! OUT SEUIL  VALEUR DU CRITERE PLASTIQUE -------------------------------
! ======================================================================
    real(kind=8) :: difsig, sig3, mu, k, sigbd, un, trois, neuf
    real(kind=8) :: aux1, aux2
! ======================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( trois  =  3.0d0  )
    parameter       ( neuf   =  9.0d0  )
! ======================================================================
! --- RECUPERATION DES DONNEES MATERIAU --------------------------------
! ======================================================================
    mu = materf(4,1)
    k = materf(5,1)
    sigbd = materf(14,2)
! ======================================================================
! --- CALCUL DES VALEURS PROPRES DE SIG --------------------------------
! ======================================================================
    difsig = (vp(3)-vp(1))*(un - trois*mu*dg/(sigeqe*(etap+un)))
    sig3 = vp(3)*(un - trois*mu*dg/(sigeqe*(etap+un))) + (i1e - neuf*k*etap*dg/(etap+un))/trois
! ======================================================================
! --- CALCUL DU SEUIL --------------------------------------------------
! ======================================================================
    aux1 = -sig3*parame(2)+parame(1)
    aux2 = parame(3)*(un+sig3/sigbd)
    seuil = (difsig - aux2)**2 - aux1
! ======================================================================
end subroutine
