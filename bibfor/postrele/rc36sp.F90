subroutine rc36sp(nbm, ima, ipt, c, k,&
                  cara, mati, pi, mi, matj,&
                  pj, mj, mse, nbthp, nbthq,&
                  ioc1, ioc2, spij, typeke, spmeca,&
                  spther)
! aslint: disable=W1504
    implicit   none
    include 'asterfort/rcsp01.h'
    include 'asterfort/u2mess.h'
    integer :: nbm, ima(*), ipt, nbthp, nbthq
    real(kind=8) :: c(*), k(*), cara(*), mati(*), matj(*), pi, mi(*), pj, mj(*)
    real(kind=8) :: mse(*), spij, typeke, spmeca, spther
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!     CALCUL DU SN
!
! IN  : IMA    : NUMERO DE LA MAILLE TRAITEE
! IN  : IPT    : NUMERO DU NOEUD TRAITE
! IN  : C      : INDICES DE CONTRAINTES
! IN  : K      : INDICES DE CONTRAINTES
! IN  : CARA   : CARACTERISTIQUES ELEMENTAIRES
! IN  : MATI   : MATERIAU ASSOCIE A L'ETAT STABILISE I
! IN  : PI     : PRESSION ASSOCIEE A L'ETAT STABILISE I
! IN  : MI     : MOMENTS ASSOCIEES A L'ETAT STABILISE I
! IN  : MATJ   : MATERIAU ASSOCIE A L'ETAT STABILISE J
! IN  : PJ     : PRESSION ASSOCIEE A L'ETAT STABILISE J
! IN  : MJ     : MOMENTS ASSOCIEES A L'ETAT STABILISE J
! IN  : PJ     : PRESSION ASSOCIEE A L'ETAT STABILISE J
! IN  : MSE    : MOMENTS DUS AU SEISME
! IN  : NBTHP  : NOMBRE DE CALCULS THERMIQUE POUR L'ETAT STABILISE P
! IN  : NBTHQ  : NOMBRE DE CALCULS THERMIQUE POUR L'ETAT STABILISE Q
! IN  : IOC1   : NUMERO OCCURRENCE SITUATION DE L'ETAT STABILISE P
! IN  : IOC2   : NUMERO OCCURRENCE SITUATION DE L'ETAT STABILISE Q
! OUT : SPIJ   : AMPLITUDE DE VARIATION DES CONTRAINTES TOTALES
!     ------------------------------------------------------------------
!
    integer :: icmp, ioc1, ioc2
    real(kind=8) :: pij, d0, ep, inert, nu, e, alpha, mij, eab, xx, alphaa
    real(kind=8) :: alphab, sp1, sp2, sp3, sp4, sp5, sp6, spp, spq
! DEB ------------------------------------------------------------------
!
! --- DIFFERENCE DE PRESSION ENTRE LES ETATS I ET J
!
    pij = abs( pi - pj )
!
! --- VARIATION DE MOMENT RESULTANT
!
    mij = 0.d0
    do 10 icmp = 1, 3
        xx = mse(icmp) + abs( mi(icmp) - mj(icmp) )
        mij = mij + xx**2
10  end do
    mij = sqrt( mij )
!
! --- LE MATERIAU
!
    e = max ( mati(2) , matj(2) )
    nu = max ( mati(3) , matj(3) )
    alpha = max ( mati(4) , matj(4) )
    alphaa = max ( mati(7) , matj(7) )
    alphab = max ( mati(8) , matj(8) )
    eab = max ( mati(9) , matj(9) )
!
! --- LES CARACTERISTIQUES
!
    inert = cara(1)
    d0 = cara(2)
    ep = cara(3)
!
! CAS DE KE_MECA (PAS DE PARTITION MECANIQUE - THERMIQUE)
!
    if (typeke .lt. 0.d0) then
!
! ------ CALCUL DU SP:
!        -------------
        sp1 = k(1)*c(1)*pij*d0 / 2 / ep
        sp2 = k(2)*c(2)*d0*mij / 2 / inert
        sp3 = k(3)*e*alpha / 2 / (1.d0-nu)
!
        sp4 = k(3)*c(3)*eab
        sp5 = e*alpha / (1.d0-nu)
!
! ------ ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P
!
        call rcsp01(nbm, ima, ipt, sp3, sp4,&
                    sp5, alphaa, alphab, nbthp, ioc1,&
                    sp6)
!
        spp = sp1 + sp2 + sp6
!
!
! ------ ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE Q
!
        call rcsp01(nbm, ima, ipt, sp3, sp4,&
                    sp5, alphaa, alphab, nbthq, ioc2,&
                    sp6)
!
        spq = sp1 + sp2 + sp6
!
        spij = max ( spp, spq )
!
! --- CAS DE KE_MIXTE (PARTITION MECANIQUE - THERMIQUE)
!
    else if (typeke.gt.0.d0) then
!
! ------ CALCUL DU SP:
!        -------------
        sp1 = k(1)*c(1)*pij*d0 / 2 / ep
        sp2 = k(2)*c(2)*d0*mij / 2 / inert
!
        spmeca=sp1+sp2
!
        sp3 = k(3)*e*alpha / 2 / (1.d0-nu)
        sp4 = k(3)*c(3)*eab
        sp5 = e*alpha / (1.d0-nu)
!
! ------ ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P
!
        call rcsp01(nbm, ima, ipt, sp3, sp4,&
                    sp5, alphaa, alphab, nbthp, ioc1,&
                    sp6)
!
        spp = sp6
        spther = max(spther,spp)
!
! ------ ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE Q
!
        call rcsp01(nbm, ima, ipt, sp3, sp4,&
                    sp5, alphaa, alphab, nbthq, ioc2,&
                    sp6)
!
        spq = sp6
        spther = max(spther,spq)
!
    else
        call u2mess('F', 'POSTRCCM_31')
!
    endif
!
end subroutine
