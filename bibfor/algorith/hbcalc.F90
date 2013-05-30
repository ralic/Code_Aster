subroutine hbcalc(fmoins, gamma, dg, nbmat, materf,&
                  i1e, sigeqe, vp, etap, vh,&
                  vg, parame, derive, incrg)
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
! ======================================================================
    implicit      none
    include 'asterfort/u2mess.h'
    integer :: nbmat
    real(kind=8) :: fmoins, incrg, gamma, dg, i1e, sigeqe, derive(5)
    real(kind=8) :: vp(3), materf(nbmat, 2), etap, vh, vg, parame(4)
! ======================================================================
! --- LOI DE HOEK BROWN : CALCUL DE L INCREMENT de GAMMA (VAR ECROUIS.)
! ======================================================================
! IN   FMOINS  CRITERE PLASTIQUE A L ITERATION PRECEDENTE --------------
! IN   GAMMA   VARIABLE D ECROUISSAGE GAMMA A L ITERATION PRECEDENTE ---
! IN   NBMAT   NOMBRE DE DONNEES MATERIAU ------------------------------
! IN   MATERF  DONNEES MATERIAU ----------------------------------------
! IN   VP      VALEURS PROPRES Du DEVIATEUR ELASTIQUE SE ---------------
! IN   I1E     PREMIER INVARIANT ELASTIQUE -----------------------------
! IN   DG      VALEUR DE DELTA GAMMA A L ITERATION PRECEDENTE ----------
! IN   ETAP    VALEUR DE ETA A L ITERATION PRECEDENTE ------------------
! IN   VH, VG  VALEUR DES FONCTIONS H ET G A L ITERATION PRECEDENTE ----
! IN   PARAME  VALEUR DES PARAMETRES DE LA LOI A L ITERATION PRECEDENTE
! IN   DERIVE  VALEUR DES DERIVEES DES PARAMETRES A L ITER PRECEDENTE --
! OUT  INCRG   VALEUR DE L INCREMENT POUR DGAMMA A L ITERATION COURANTE
! ======================================================================
    real(kind=8) :: dfdga, derh, derg, aux1, aux2, aux3, aux4
    real(kind=8) :: vm, ds, dm
    real(kind=8) :: a1, a2, a3, a4, sigbd, mu, un, deux, trois, eps
! ======================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( deux   =  2.0d0  )
    parameter       ( trois  =  3.0d0  )
    parameter       ( eps    =  1.0d-12 )
! ======================================================================
! --- INITIALISATION DES PARAMETRES ------------------------------------
! ======================================================================
    sigbd = materf(14,2)
    mu = materf(4,1)
    a1 = vp(3)+i1e/trois
    a2 = vp(3)-vp(1)
    a3 = un/sigbd
    a4 = trois*mu/sigeqe
    derh = derive(4)
    derg = derive(5)
    vm = parame(2)
    ds = derive(1)
    dm = derive(2)
    aux1 = -a2*a4*(derh*dg+vh)
    aux2 = -derive(3)*(un+a3*(a1-vg*dg))+parame(3)*a3*(derg*dg+vg)
    aux3 = a2*(un-a4*vh*dg)-parame(3)*(un+a3*(a1-vg*dg))
    aux4 = ds -dm*(a1-vg*dg) +vm*(derg*dg+vg)
! ======================================================================
    dfdga = deux*aux3*(aux1 + aux2) - aux4
    if (abs(dfdga) .lt. eps) then
        call u2mess('F', 'ALGORITH3_87')
    endif
    incrg = -fmoins/dfdga
! ======================================================================
end subroutine
