subroutine xnewto(elp, name, num, nno, ndim,&
                  ptint, tabco, jtabls, ipp, ip,&
                  s, itemax, epsmax, ksi)
    implicit none
!
    include 'asterc/r8gaem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xdelt1.h'
    include 'asterfort/xdelt2.h'
    integer :: num, ndim, ipp, ip, nno, jtabls
    real(kind=8) :: ksi(ndim), s, ptint(*), tabco(*)
    integer :: itemax
    real(kind=8) :: epsmax
    character(len=6) :: name
    character(len=8) :: elp
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! TOLE CRS_1404
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
!             ALGORITHME DE NEWTON POUR CALCULER LES COORDONNEES
!                DE REFERENCE D'UN POINT PT MILIEU D'UNE ARETE
!
!     ENTREE
!       NUM     : NUMERO DE LA FONCTION A RESOUDRE (DANS XDELT1)
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       COORSG  : COORDONNEES DES 3 NOEUDS DE L'ARETE
!       S       : ABSCISSE CURVILIGNE DU POINT SUR L'ARETE
!       ITEMAX  : NOMBRE MAXI D'ITERATIONS DE NEWTON
!       EPSMAX  : RESIDU POUR CONVERGENCE DE NEWTON
!
!     SORTIE
!       KSI     : COORDONNEES DE REFERENCE DU POINT
!     --------------------------------------------------------------
!
    real(kind=8) :: eps
    real(kind=8) :: test, epsrel, epsabs, refe
    integer :: iter, i
    real(kind=8) :: zero
    parameter    (zero=0.d0)
    real(kind=8) :: dist, dmin
    real(kind=8) :: delta(ndim), ksim(ndim)
!
! ------------------------------------------------------------------
!
    call jemarq()
!
! --- POINT DE DEPART
!
    call vecini(ndim, zero, ksi)
    call vecini(ndim, zero, delta)
    iter = 0
    epsabs = epsmax/100.d0
    epsrel = epsmax
    dmin = r8gaem()
!
! --- DEBUT DE LA BOUCLE
!
20  continue
!-------------------------
!
!     FAIRE TANT QUE
!
!
! --- CALCUL DE LA QUANTITE A MINIMISER
!
    if (name .eq. 'XMILFI') then
        call xdelt2(elp, nno, ndim, ksi, ptint,&
                    tabco, jtabls, ipp, ip, delta)
    else if (name.eq. 'XINVAC') then
        call xdelt1(num, ndim, ksi(1), tabco, s,&
                    delta)
    endif
!
! --- ACTUALISATION
!
    do 30 i = 1, ndim
        ksi(i) = ksi(i) - delta(i)
30  end do
!
    iter = iter + 1
!
    do 40 i = 1, ndim
        dist = delta(i)*delta(i)
40  end do
    dist = sqrt(dist)
!
    if (dist .le. dmin) then
        do 50 i = 1, ndim
            ksim(i) = ksi(i)
50      continue
    endif
!
! --- CALCUL DE LA REFERENCE POUR TEST DEPLACEMENTS
!
    refe = zero
    do 60 i = 1, ndim
        refe = refe + ksi(i)*ksi(i)
60  end do
    if (refe .le. epsrel) then
        refe = 1.d0
        eps = epsabs
    else
        eps = epsrel
    endif
!
! --- CALCUL POUR LE TEST DE CONVERGENCE
!
    test = zero
    do 70 i = 1, ndim
        test = test + delta(i)*delta(i)
70  end do
    test = sqrt(test/refe)
!
! --- EVALUATION DE LA CONVERGENCE
!
    if ((test.gt.eps) .and. (iter.lt.itemax)) then
        goto 20
    else if ((iter.ge.itemax).and.(test.gt.eps)) then
        call u2mess('F', 'XFEM_67')
        do 80 i = 1, ndim
            ksi(i) = ksim(i)
80      end do
    endif
!
! --- FIN DE LA BOUCLE
!
    do 90 i = 1, ndim
        ksi(i)=ksi(i)-delta(i)
90  end do
!
    call jedema()
end subroutine
