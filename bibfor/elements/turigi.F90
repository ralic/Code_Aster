subroutine turigi(nomte, nbrddl, k)
! aslint: disable=W1306
    implicit none
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
    include 'jeveux.h'
    include 'asterc/r8pi.h'
    include 'asterfort/bcoudc.h'
    include 'asterfort/bcoude.h'
    include 'asterfort/carcou.h'
    include 'asterfort/elref5.h'
    include 'asterfort/jevech.h'
    include 'asterfort/kcoude.h'
    include 'asterfort/klg.h'
    include 'asterfort/klgcou.h'
    include 'asterfort/matini.h'
    include 'asterfort/mavec.h'
    include 'asterfort/moytem.h'
    include 'asterfort/rcvala.h'
    include 'asterfort/u2mess.h'
    character(len=16) :: nomte
    integer :: nbrddl, nc
    integer :: ndim, nnos, idfdk, jdfd2, jgano
    real(kind=8) :: b(4, nbrddl), k(nbrddl, nbrddl)
! ......................................................................
!
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          TUYAU
!                          OPTION : RIGI_MECA
!    - ARGUMENTS:
!        DONNEES:      B, MASS1,MASS,K : MATRICES
!                      DIMENSIONNEES PAR LE TE0582 APPELANT
!
! ......................................................................
!
!
!
!     VARIABLES LOCALES
!
    integer :: nbres, icoude, jnbspi, nbsecm, nbcoum, nspg
    parameter (nbres=9)
    character(len=8) :: nomres(nbres), nompar
    integer :: icodre(nbres)
    parameter (nbsecm=32,nbcoum=10)
    real(kind=8) :: poicou(2*nbcoum+1), poisec(2*nbsecm+1)
    real(kind=8) :: valres(nbres), valpar, theta
    real(kind=8) :: e, nu, h, a, l
    real(kind=8) :: pi, deuxpi, fi, sinfi
    real(kind=8) :: beta, cisail, g, poids, r, rayon
    real(kind=8) :: c(4, 4), xpg(4)
    real(kind=8) :: pgl(3, 3), pgl4(3, 3)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), omega
    integer :: nno, npg, nbcou, nbsec, m
    integer :: ipoids, ivf, iret
    integer :: imate, imatuu, icagep, igeom, nbpar
    integer :: igau, icou, isect, i, j, lorien, icoud2, mmt
    integer :: jcoopg
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, jcoopg, ivf, idfdk,&
                jdfd2, jgano)
!
!     DIMENSION DE LA MATRICE STOCKEE SOUS FORME VECTEUR
    nc = nbrddl* (nbrddl+1)/2
!
    pi = r8pi()
    deuxpi = 2.d0*pi
!
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou = zi(jnbspi-1+1)
    nbsec = zi(jnbspi-1+2)
!
!     -- CALCUL DES POIDS DES COUCHES ET DES SECTEURS:
    poicou(1) = 1.d0/3.d0
    do 10 i = 1, nbcou - 1
        poicou(2*i) = 4.d0/3.d0
        poicou(2*i+1) = 2.d0/3.d0
10  end do
    poicou(2*nbcou) = 4.d0/3.d0
    poicou(2*nbcou+1) = 1.d0/3.d0
    poisec(1) = 1.d0/3.d0
    do 20 i = 1, nbsec - 1
        poisec(2*i) = 4.d0/3.d0
        poisec(2*i+1) = 2.d0/3.d0
20  end do
    poisec(2*nbsec) = 4.d0/3.d0
    poisec(2*nbsec+1) = 1.d0/3.d0
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCAGEPO', 'L', icagep)
    h = zr(icagep+1)
    a = zr(icagep) - h/2.d0
! A= RMOY, H = EPAISSEUR
!
!
    m = 3
    if (nomte .eq. 'MET6SEG3') m = 6
!
!
!
    do 30 i = 1, npg
        xpg(i) = zr(jcoopg-1+i)
30  end do
!     --- RECUPERATION DES ORIENTATIONS ---
    call jevech('PCAORIE', 'L', lorien)
    call carcou(zr(lorien), l, pgl, rayon, theta,&
                pgl1, pgl2, pgl3, pgl4, nno,&
                omega, icoud2)
    if (icoud2 .ge. 10) then
        icoude = icoud2 - 10
        mmt = 0
        if (h/a .gt. (0.25d0)) then
            call u2mess('A', 'ELEMENTS4_54')
        endif
    else
        icoude = icoud2
        mmt = 1
    endif
    call jevech('PMATERC', 'L', imate)
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
!       -- CALCUL DES TEMPERATURES INF, SUP ET MOY
!          (MOYENNE DES NNO NOEUDS) ET DES COEF. DES POLY. DE DEGRE 2 :
!          ------------------------------------------------------------
    nspg=(2*nbsec + 1)*(2*nbcou + 1)
    iret=0
    call moytem('RIGI', npg, nspg, '+', valpar,&
                iret)
    if (iret .ne. 0) valpar=0.d0
    nbpar = 1
    nompar = 'TEMP'
    call rcvala(zi(imate), ' ', 'ELAS', nbpar, nompar,&
                valpar, 2, nomres, valres, icodre,&
                1)
    e = valres(1)
    nu = valres(2)
! DEFINITION DE LA MATRICE DE COMPORTEMENT C
!
    beta = 1.d0/ (1.d0-nu**2)
    g = 1.d0/ (2.d0* (1.d0+nu))
    cisail = 1.d0
!
! ON MULTIPLIERA PAR E PLUS LOIN
!
    c(1,1) = beta
    c(1,2) = nu*beta
    c(1,3) = 0.d0
    c(1,4) = 0.d0
!
    c(2,1) = nu*beta
    c(2,2) = beta
    c(2,3) = 0.d0
    c(2,4) = 0.d0
!
    c(3,1) = 0.d0
    c(3,2) = 0.d0
    c(3,3) = g
    c(3,4) = 0.d0
!
    c(4,1) = 0.d0
    c(4,2) = 0.d0
    c(4,3) = 0.d0
    c(4,4) = g*cisail
!
!
!     FIN DE LA CONSTRUCTION DE LA MATRICE DE COMPORTEMENT C
!
!  INITIALISATION DE LA MATRICE K
!
    call matini(nbrddl, nbrddl, 0.d0, k)
!
! BOUCLE SUR LES POINTS DE GAUSS
!
    do 90 igau = 1, npg
!
! BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
!
        do 80 icou = 1, 2*nbcou + 1
            if (mmt .eq. 0) then
                r = a
            else
                r = a + (icou-1)*h/ (2.d0*nbcou) - h/2.d0
            endif
!
! BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
!
            do 70 isect = 1, 2*nbsec + 1
                if (icoude .eq. 0) then
                    call bcoude(igau, icou, isect, l, h,&
                                a, m, nno, nbcou, nbsec,&
                                zr(ivf), zr(idfdk), zr(jdfd2), mmt, b)
                    poids = zr(ipoids-1+igau)*poicou(icou)*poisec( isect)* (l/2.d0)*h*deuxpi/ (4.&
                            &d0*nbcou*nbsec)*r
!
                else if (icoude.eq.1) then
                    fi = (isect-1)*deuxpi/ (2.d0*nbsec)
!               FI = FI - OMEGA
                    sinfi = sin(fi)
                    l = theta* (rayon+r*sinfi)
                    call bcoudc(igau, icou, isect, h, a,&
                                m, omega, xpg, nno, nbcou,&
                                nbsec, zr(ivf), zr(idfdk), zr(jdfd2), rayon,&
                                theta, mmt, b)
                    poids = zr(ipoids-1+igau)*poicou(icou)*poisec( isect)* (l/2.d0)*h*deuxpi/ (4.&
                            &d0*nbcou*nbsec)*r
!
                endif
!  LE DERNIER TERME C'EST R DU  R DFI DX DZETA
!
                call kcoude(nbrddl, poids, b, c, k)
70          continue
80      continue
90  end do
!
! FIN DU CALCUL DE LA MATRICE DE RIGIDITE
!
! MULTIPLICATION PAR LE MODULE DE YOUNG E
!
    do 110 i = 1, nbrddl
        do 100 j = 1, nbrddl
            k(i,j) = e*k(i,j)
100      continue
110  end do
!
!  CHANGEMENT DE REPERE :
!  PASSAGE DU REPERE LOCAL AU REPERE GLOBAL
!
    if (icoude .eq. 0) then
        call klg(nno, nbrddl, pgl, k)
    else if (icoude.eq.1) then
        call klgcou(nno, nbrddl, pgl1, pgl2, pgl3,&
                    pgl4, k)
    endif
!
! STOCKAGE DE LA MATRICE DE RIGIDITE
!
    call jevech('PMATUUR', 'E', imatuu)
    call mavec(k, nbrddl, zr(imatuu), nc)
!
end subroutine
