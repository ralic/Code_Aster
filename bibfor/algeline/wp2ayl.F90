subroutine wp2ayl(appr, lmatra, lmasse, lamor, sigma,&
                  lbloq, yh, yb, zh, zb,&
                  u1, u2, u3, u4, v,&
                  n, solveu)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/mrmult.h'
    include 'asterfort/resoud.h'
    character(len=1) :: appr
    complex(kind=8) :: v(*), sigma
    real(kind=8) :: u1(*), u2(*), u3(*), u4(*), yh(*), yb(*), zh(*), zb(*)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!                   T               T
!     CALCUL (ZH,ZB)   = A * (YH,YB)
!
!     OU A EST L' OPERATEUR (REEL) DONT ON CHERCHE UNE APPROXIMATION
!     DES ELEMENTS PROPRES
!     ------------------------------------------------------------------
! IN  APPR   : K : INDICATEUR D' APPROCHE ('R'/'I') POUR A
! IN  LMATRA : I : FACTORISEE LDLT (DANS C) DE LA MATRICE DYNAMIQUE
! IN  LMASSE : I : MATRICE DE MASSE
! IN  LAMOR  : I : MATRICE D' AMORTISSEMENT
! IN  SIGMA  : C : VALEUR DU SHIFT
! IN  YH     : R : PARTIE SUPERIEUR DE Y
! IN  YB     : R : PARTIE INFERIEURE DE Y
! IN  N      : I : DIMENSION DE MATRICE
! IN  LBLOQ  : I : TYPE DES DDL (LBOLOQ(I) = 0 <=> DDL(I) = BLOQUE)
! OUT ZH     : R : PARTIE SUPERIEURE DU RESULTAT
! OUT ZB     : R : PARTIE INFERIEURE DU RESULTAT
! VAR U1     : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT AMOR *YH
! VAR U2     : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT MASSE*YB
! VAR U3     : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT MASSE*YH
! VAR U4     : R : VECTEUR DE TRAVAIL
! VAR V      : R : VECTEUR DE TRAVAIL
! IN  SOLVEU : K19: SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!     ------------------------------------------------------------------
!
!
    real(kind=8) :: zero, sr, rbid
    integer :: i
    complex(kind=8) :: cbid
    character(len=1) :: kbid
    character(len=19) :: k19bid, matass, chcine, criter
    integer :: iret
!     ------------------------------------------------------------------
! INIT. OBJETS ASTER
!-----------------------------------------------------------------------
    real(kind=8) :: si
!-----------------------------------------------------------------------
    matass=zk24(zi(lmatra+1))
    chcine=' '
    criter=' '
    k19bid=' '
    zero = 0.0d0
    sr = dble(sigma)
    si = dimag(sigma)
!
    call mrmult('ZERO', lamor, yh, u1, 1,&
                .false.)
    call mrmult('ZERO', lmasse, yb, u2, 1,&
                .false.)
    call mrmult('ZERO', lmasse, yh, u3, 1,&
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
    if (si .ne. zero) then
        do 10, i = 1, n, 1
        v(i) = dcmplx(u1(i)) + sigma*dcmplx(u3(i)) + dcmplx(u2(i))
10      continue
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, rbid, v,&
                    criter, .false., 0, iret)
        if (appr .eq. 'R') then
            do 20, i = 1, n, 1
            zh(i) = - dble(v(i))
            zb(i) = (yh(i) - dble(sigma*v(i)))*lbloq(i)
20          continue
        else
            do 21, i = 1, n, 1
            zh(i) = - dimag(v(i))
            zb(i) = - dimag(sigma*v(i))*lbloq(i)
21          continue
        endif
    else
        do 30, i = 1, n, 1
        u4(i) = u1(i) + sr*u3(i) + u2(i)
30      continue
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, u4, cbid,&
                    criter, .false., 0, iret)
        do 31, i = 1, n, 1
        zh(i) = -u4(i)
        zb(i) = (yh(i) - sr*u4(i))*lbloq(i)
31      continue
    endif
end subroutine
