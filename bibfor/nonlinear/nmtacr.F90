subroutine nmtacr(mode, ndimsi, mat, sigel, vim,&
                  epm, dp, sp, xi, f,&
                  g, fds, gds, fdp, gdp,&
                  fdx, gdx, dpmax, sig, tang)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
#include "blas/ddot.h"
    integer :: mode, ndimsi
    real(kind=8) :: mat(14), sigel(ndimsi), vim(9), epm(6), dp, sp, xi
    real(kind=8) :: f, g, fdp, fds, fdx, gdp, gds, gdx, dpmax, sig(ndimsi)
    real(kind=8) :: tang(6, 6)
!
!
! ----------------------------------------------------------------------
! TAHERI :  EVALUATION DES CRITERES ET DERIVEES
! ----------------------------------------------------------------------
! IN  MODE   SELECTION DES TERMES A CALCULER
!             0 -> DPMAX
!             1 -> F,G
!             2 -> F, G, FDS, GDS, FDP, GDP
!             3 -> F, G, FDS, GDS, FDX, GDX
!             4 -> SIG
!             5 -> TANG (PLASTICITE CLASSIQUE)
!             6 -> TANG (PLASTICITE A DEUX SURFACES)
! IN  NDIMSI DIMENSION DES TENSEURS
! IN  MAT    TABLEAU DES CONSTANTES MATERIAUX
! IN  SIGEL  DEVIATEUR DES CONTRAINTES ELASTIQUES
! IN  VIM    VARIABLES INTERNES EN T-
! IN  EPM    DEFORMATION PLASTIQUE EN T-
! IN  DP     INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
! IN  SP     CONTRAINTE DE PIC (SIGMA P)
! IN  XI     PILOTAGE DE EPN
! OUT F      VALEUR DU CRITERE DE PLASTICITE
! OUT G      VALEUR DU CRITERE DE CONTRAINTE MAXIMALE
! OUT FDS    DERIVEE DE F PAR RAPPORT A SIGMA P
! OUT GDS    DERIVEE DE G PAR RAPPORT A SIGMA P
! OUT FDP    DERIVEE DE F PAR RAPPORT A P
! OUT GDP    DERIVEE DE G PAR RAPPORT A P
! OUT FDX    DERIVEE DE F PAR RAPPORT A XI
! OUT GDX    DERIVEE DE G PAR RAPPORT A XI
! OUT DPMAX  BORNE SUPERIEURE POUR L'INCREMENT DE DEFO. PLAST.
! OUT SIG    CORRECTION DE CONTRAINTE (DEUXMU * DEP)
! OUT TANG   VARIATION :  DEP/SIGEL * PROJDEV
! ----------------------------------------------------------------------
!
    integer :: n, i, j
    real(kind=8) :: epndx(6), epn(6)
    real(kind=8) :: c, d, a(6), b(6), se(6), s0(6), seeq
    real(kind=8) :: v(6), semax
    real(kind=8) :: p, seq, m(6), meq, l(6), leq, q
    real(kind=8) :: tmp, cdp, ddp, sedp(6), seeqdp, s0dp(6), seqdp
    real(kind=8) :: mdp(6), meqdp, qdp, ldp(6), leqdp
    real(kind=8) :: cds, dds, ads(6), seds(6), seeqds, s0ds(6), seqds
    real(kind=8) :: mds(6), meqds, qds, lds(6), leqds
    real(kind=8) :: adx(6), bdx(6), sedx(6), seeqdx, s0dx(6), seqdx
    real(kind=8) :: mdx(6), meqdx, ldx(6), leqdx, qdx
    real(kind=8) :: pbs0, seqde(6), meqde(6), qde(6), fde(6)
    real(kind=8) :: pas0, leqde(6), gde(6)
    real(kind=8) :: pde(6), det
!
!
!
! -- CALCUL DES ELEMENTS COMMUNS
!
    p = vim(1) + dp
    tmp = exp(- mat(8) * p * (1.d0 - sp / mat(11) ))
    c = mat(10) + mat(9) * tmp
    d = 1.d0 - mat(6) * tmp
    do 10 n = 1, ndimsi
        epndx(n) = vim(2+n) - epm(n)
        b(n) = - xi * epndx(n)
        epn(n) = epm(n) - b(n)
        a(n) = (mat(11)-sp)*epm(n) + sp*b(n)
        se(n) = sigel(n) - c*a(n)
10  end do
    seeq = sqrt(1.5d0*ddot(ndimsi,se,1,se,1))
!
!
! -- CALCUL D'UNE BORNE POUR DP
!
    if (mode .eq. 0) then
        do 15 n = 1, ndimsi
            v(n) = sigel(n) - mat(10) * a(n)
15      continue
        semax = sqrt(1.5d0*ddot(ndimsi,v,1,v,1))
        if (seeq .gt. semax) semax=seeq
        dpmax = (semax - d*mat(4)) / 1.5d0 / (mat(2)+mat(11)*mat(10))
        goto 9999
    endif
!
!
! -- CALCUL DE SIGMA ET DES CRITERES F ET G
!
    if (seeq .ne. 0.d0) then
        do 20 n = 1, ndimsi
            s0(n) = se(n)/seeq
20      continue
    else
        do 21 n = 1, ndimsi
            s0(n) = 0.d0
21      continue
        s0(4) = 1.d0 / sqrt(1.5d0)
    endif
!
    if (mode .eq. 4) then
        do 23 n = 1, ndimsi
            sig(n) = 1.5d0*mat(2) * dp * s0(n)
23      continue
        goto 9999
    endif
!
    do 25 n = 1, ndimsi
        m(n) = b(n) + 1.5d0*dp*s0(n)
        l(n) = a(n) + 1.5d0 * mat(11) * dp * s0(n)
25  end do
!
    seq = seeq - 1.5d0 * (mat(2) + mat(11)*c) * dp
    meq = sqrt(1.5d0*ddot(ndimsi,m,1,m,1))
    leq = sqrt(1.5d0*ddot(ndimsi,l,1,l,1))
    if (meq .le. 0.d0) then
        q = mat(4)
    else
        q = mat(4) + mat(7) * meq**mat(5)
    endif
!
    f = seq - d*q
    if (dp .ne. 0.d0) f = f - mat(13) * dp**mat(12) * p**mat(14)
    g = c*leq + d*q - sp
    if (mode .eq. 1) goto 9999
!
!
! -- DERIVEES DES CRITERES PAR RAPPORT A SIGMA PIC
!
    tmp = mat(8)*p/mat(11) * exp(-mat(8)*p*(1-sp/mat(11)))
    cds = mat(9)*tmp
    dds = - mat(6)*tmp
!
    do 30 n = 1, ndimsi
        ads(n) = - epn(n)
        seds(n) = - cds*a(n) - c*ads(n)
30  end do
    seeqds = 1.5d0*ddot(ndimsi,se,1,seds,1)/seeq
    seqds = seeqds - 1.5d0*mat(11)*cds*dp
!
    do 40 n = 1, ndimsi
        s0ds(n) = (seds(n)*seq - seeqds*se(n)) / seeq**2
        mds(n) = 1.5d0 * dp * s0ds(n)
        lds(n) = ads(n) + 1.5d0*mat(11)*dp*s0ds(n)
40  end do
    meqds = 1.5d0 * ddot(ndimsi,m,1,mds,1) / meq
    leqds = 1.5d0 * ddot(ndimsi,l,1,lds,1) / leq
    qds = mat(7) * mat(5) * meq**(mat(5)-1) * meqds
!
    fds = seqds - dds*q - d*qds
    gds = cds*leq + c*leqds + dds*q + d*qds - 1.d0
!
!
! -- DERIVEE DES CRITERES PAR RAPPORT A P
!
    if (mode .eq. 2 .or. mode .ge. 5) then
        tmp = - mat(8)*(1-sp/mat(11)) * exp(-mat(8)*p*(1-sp/mat(11)))
        cdp = mat(9)*tmp
        ddp = - mat(6)*tmp
!
        do 50 n = 1, ndimsi
            sedp(n) = - cdp*a(n)
50      continue
        seeqdp = 1.5d0*ddot(ndimsi,se,1,sedp,1)/seeq
        seqdp = seeqdp - 1.5d0 * (mat(2) + mat(11)*c + mat(11)*cdp*dp)
!
        do 60 n = 1, ndimsi
            s0dp(n) = (sedp(n)*seeq - se(n)*seeqdp) / seeq**2
            mdp(n) = 1.5d0 * (s0(n)+dp*s0dp(n))
            ldp(n) = mat(11) * mdp(n)
60      continue
        meqdp = 1.5d0 * ddot(ndimsi,m,1,mdp,1)/meq
        leqdp = 1.5d0 * ddot(ndimsi,l,1,ldp,1)/leq
        qdp = mat(7) * mat(5) * meq**(mat(5)-1) * meqdp
!
        fdp = seqdp - ddp*q - d*qdp - mat(13) * (mat(14)*dp+mat(12)*p) * p**(mat(14)-1) * dp**(ma&
              &t(12)-1)
        gdp = cdp*leq + c*leqdp + ddp*q + d*qdp
!
        if (mode .eq. 2) goto 9999
    endif
!
!
! -- DERIVEES PAR RT A SIGEL ET CONSTRUCTION DE LA MATRICE TANGENTE
    if (mode .eq. 5 .or. mode .eq. 6) then
        tmp = 1.5d0/meq * 1.5d0*dp/seeq
        pbs0 = 1.5d0 * ddot(ndimsi,b,1,s0,1)
        do 62 n = 1, ndimsi
            seqde(n) = 1.5d0*s0(n)
            meqde(n) = tmp * (b(n) - pbs0*s0(n))
            qde(n) = mat(7) * mat(5) * meq**(mat(5)-1) * meqde(n)
            fde(n) = seqde(n) - d*qde(n)
62      continue
!
        if (mode .eq. 6) then
            tmp = 1.5d0/leq * 1.5d0*mat(11)*dp/seeq
            pas0 = 1.5d0 * ddot(ndimsi,a,1,s0,1)
            do 64 n = 1, ndimsi
                leqde(n) = tmp * (a(n) - pas0*s0(n))
                gde(n) = c*leqde(n) + d*qde(n)
64          continue
        endif
!
        if (mode .eq. 5) then
            do 66 n = 1, ndimsi
                pde(n) = - fde(n)/fdp
66          continue
        else
            det = fdp*gds - gdp*fds
            do 68 n = 1, ndimsi
                pde(n) = (fds*gde(n) - gds*fde(n)) / det
68          continue
        endif
!
        tmp = 1.5d0*dp/seeq
        do 70 i = 1, ndimsi
            do 71 j = 1, ndimsi
                tang(i,j) = 1.5d0*(s0(i)*pde(j) - tmp*s0(i)*s0(j))
71          continue
70      continue
        do 72 n = 1, ndimsi
            tang(n,n) = tang(n,n) + tmp
72      continue
        do 73 i = 1, 3
            do 74 j = 1, 3
                tang(i,j) = tang(i,j) - tmp/3.d0
74          continue
73      continue
!
        goto 9999
    endif
!
!
! -- DERIVEES DES CRITERES PAR RAPPORT A XI
!
    if (mode .eq. 3) then
        do 80 n = 1, ndimsi
            adx(n) = - sp * epndx(n)
            bdx(n) = - epndx(n)
            sedx(n) = - c * adx(n)
80      continue
!
        seeqdx = 1.5d0*ddot(ndimsi,se,1,sedx,1)/seeq
        seqdx = seeqdx
!
        do 90 n = 1, ndimsi
            s0dx(n) = (sedx(n)*seq - seeqdx*se(n)) / seeq**2
            mdx(n) = bdx(n) + 1.5d0 * dp * s0dx(n)
            ldx(n) = adx(n) + 1.5d0 * mat(11) * dp * s0dx(n)
90      continue
        meqdx = 1.5d0 * ddot(ndimsi,m,1,mdx,1) / meq
        leqdx = 1.5d0 * ddot(ndimsi,l,1,ldx,1) / leq
        qdx = mat(7) * mat(5) * meq**(mat(5)-1) * meqdx
!
        fdx = seqdx - d*qdx
        gdx = c*leqdx + d*qdx
!
        goto 9999
    endif
!
!
9999  continue
end subroutine
