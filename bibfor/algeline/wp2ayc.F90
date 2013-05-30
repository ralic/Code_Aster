subroutine wp2ayc(lmatra, lmasse, lamor, sigma, lbloq,&
                  yh, yb, zh, zb, u1,&
                  u2, u3, n, solveu)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/mcmult.h'
    include 'asterfort/resoud.h'
    complex(kind=8) :: sigma, u1(*), u2(*), u3(*), yh(*), yb(*), zh(*), zb(*)
    integer :: lmatra, lmasse, lamor, n, lbloq(*)
    character(len=19) :: solveu
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!                   T               T
!     CALCUL (ZH,ZB)   = A * (YH,YB)
!
!     OU A EST L' OPERATEUR (COMPLEXE) DONT ON CHERCHE UNE
!     APPROXIMATION DES ELEMENTS PROPRES
!     ------------------------------------------------------------------
! IN  LMATRA : I : FACTORISEE LDLT (DANS C) DE LA MATRICE DYNAMIQUE
! IN  LMASSE : I : MATRICE DE MASSE
! IN  LAMOR  : I : MATRICE D' AMORTISSEMENT
! IN  SIGMA  : C : VALEUR DU SHIFT
! IN  YH     : C : PARTIE SUPERIEUR DE Y
! IN  YB     : C : PARTIE INFERIEURE DE Y
! IN  N      : I : DIMENSION DE MATRICE
! IN  LBLOQ  : I : TYPE DES DDL (LBOLOQ(I) = 0 <=> DDL(I) = BLOQUE)
! OUT ZH     : C : PARTIE SUPERIEURE DU RESULTAT
! OUT ZB     : C : PARTIE INFERIEURE DU RESULTAT
! VAR U1     : C : VECTEUR DE TRAVAIL, EN SORTIE VAUT AMOR *YH
! VAR U2     : C : VECTEUR DE TRAVAIL, EN SORTIE VAUT MASSE*YB
! VAR U3     : C : VECTEUR DE TRAVAIL, EN SORTIE VAUT MASSE*YH
! VAR V      : C : VECTEUR DE TRAVAIL
! IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!     ------------------------------------------------------------------
!
!
    integer :: i
    real(kind=8) :: rbid
    character(len=1) :: kbid
    character(len=19) :: k19bid, matass, chcine, criter
    integer :: iret
!     ------------------------------------------------------------------
!
! INIT. OBJETS ASTER
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    matass=zk24(zi(lmatra+1))
    chcine=' '
    criter=' '
    k19bid=' '
!
    call mcmult('ZERO', lamor, yh, u1, 1,&
                .false.)
    call mcmult('ZERO', lmasse, yb, u2, 1,&
                .false.)
    call mcmult('ZERO', lmasse, yh, u3, 1,&
                .false.)
!-RM-DEB
!     LA BOUCLE 5 REALISE LE PRODUIT PAR MASSE*INV(MASSE_REG)*MASSR
!     OR CETTE MATRICE EST EGALE A MASSE
!---> VOIR CE QUI SE PASSE QUAND LA BOUCLE EST SUPPRIMEE
    do 5, i = 1, n, 1
    u3(i) = u3(i)*lbloq(i)
    u2(i) = u2(i)*lbloq(i)
    5 end do
!-RM-FIN
    do 10, i = 1, n, 1
    u1(i) = u1(i) + sigma*u3(i) + u2(i)
    10 end do
    call resoud(matass, k19bid, solveu, chcine, 1,&
                k19bid, k19bid, kbid, rbid, u1,&
                criter, .false., 0, iret)
    do 20, i = 1, n, 1
    zh(i) = - u1(i)
    zb(i) = (yh(i) - sigma*u1(i))*lbloq(i)
    20 end do
end subroutine
