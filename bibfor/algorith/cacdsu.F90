subroutine cacdsu(maxfa, maxdim, alpha, ndim, nno,&
                  nface, geom, vol, mface, dface,&
                  xface, normfa, kdiag, yss, c,&
                  d)
    implicit none
    include 'asterfort/assert.h'
    integer :: maxfa, maxdim, ndim, nno, nface
    real(kind=8) :: alpha, vol
    real(kind=8) :: geom(ndim, nno)
    real(kind=8) :: mface(maxfa), dface(maxfa), xface(maxdim, maxfa)
    real(kind=8) :: kdiag(6), yss(maxdim, maxfa, maxfa)
    real(kind=8) :: c(maxfa, maxfa), d(maxfa, maxfa)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRS_1404
!
!  PAR CONVENTION LA FACE I A POUR PREMIER SOMMET LE SOMMET I
!  CE SERA PLUS COMPLIQUE EN 3D !!!
! ## VARIABLES IN :
!         NDIM DIMENSION D ESPACE
!         NFACE NOMBRE DE FACES
!         KDIAG(3)   PERMEABILITE INTRINSEQUE (DIAGONALE)
!         MFACE : MESURE DES FACES
!         DFACE : DISTANCE DU CENTRE DE GRAVITÉ AUX FACES
!         DFACE N EST PAS XG - XFACE
!         XFACE : COORDONNES DES CENTRES DE FACES
!         NORMFA NORMALE SORTANTE
! ## VARIABLES OUT :
!         YSS,C,D
!
!  ((DFACE(KFA)*MFACE(KFA))/DIM):     CORRESPOND A LA MESURE DU CONE
!                                     DE SOMMET X_K ET DE BASE SIGMA
!
!
! REMARQUE :
!
!    POUR LE MOMENT ON PRENDS BETA=SQRT(DIM) MAIS ON PEUT
!    FAIRE VARIER CETTE VALEUR
!
! ----------------------------------------------------------------------
!
    integer :: maxfa1, maxdi1
    parameter    (maxfa1=6,maxdi1=3)
!
    integer :: ifa, jfa, kfa
    integer :: idim, jdim
!
    real(kind=8) :: dim, sqdim, beta
    real(kind=8) :: mcone(maxfa)
    real(kind=8) :: normfa(maxdim, maxfa)
    real(kind=8) :: xg(maxdi1)
    real(kind=8) :: ndx(maxfa1, maxfa1), kint(maxdi1, maxdi1)
    real(kind=8) :: kuni(maxdi1, maxdi1), ky, ky1
!
! ----------------------------------------------------------------------
!
    call assert(maxfa1.eq.maxfa)
    call assert(maxdi1.eq.maxdim)
!
    if (ndim .eq. 2) then
        kint(1,1)=kdiag(1)
        kint(2,2)=kdiag(2)
        kint(1,2)=kdiag(3)
        kint(2,1)=kdiag(3)
!
        kuni(1,1)=1.d0
        kuni(2,2)=1.d0
        kuni(1,2)=0.d0
        kuni(2,1)=0.d0
    else if (ndim.eq.3) then
        kint(1,1)=kdiag(1)
        kint(2,2)=kdiag(2)
        kint(3,3)=kdiag(3)
        kint(1,2)=kdiag(4)
        kint(1,3)=kdiag(5)
        kint(2,3)=kdiag(6)
        kint(2,1)=kdiag(4)
        kint(3,1)=kdiag(5)
        kint(3,2)=kdiag(6)
!
        kuni(1,1)=1.d0
        kuni(2,2)=1.d0
        kuni(3,3)=1.d0
        kuni(1,2)=0.d0
        kuni(1,3)=0.d0
        kuni(2,3)=0.d0
        kuni(2,1)=0.d0
        kuni(3,1)=0.d0
        kuni(3,2)=0.d0
    else
        call assert(.false.)
    endif
    dim=ndim
    sqdim=sqrt(dim)
    beta=sqdim*sqrt(alpha)
    do 10 idim = 1, ndim
        xg(idim)=geom(idim,nno)
10  end do
!
!======================== INITIALISATION========================
    do 20 ifa = 1, maxfa
        do 20 jfa = 1, maxfa
            do 21 idim = 1, maxdim
                yss(idim,ifa,jfa)=0.d0
21          continue
            c(ifa,jfa)=0.d0
            d(ifa,jfa)=0.d0
20      continue
!
! =========================== CALCUL DE YSS==========================
    do 30 ifa = 1, nface
        mcone(ifa)=(dface(ifa)*mface(ifa))/dim
!
!
        do 31 jfa = 1, nface
            ndx(ifa,jfa) = 0.d0
            do 32 idim = 1, ndim
                ndx(ifa,jfa)=ndx(ifa,jfa)+ normfa(idim,ifa)*(xface(&
                idim,jfa)-xg(idim))
32          continue
31      continue
30  end do
    do 40 ifa = 1, nface
        do 40 jfa = 1, nface
            if (jfa .eq. ifa) then
                do 41 idim = 1, ndim
                    yss(idim,ifa,ifa)= (mface(ifa)/vol)*normfa(idim,&
                    ifa)+ (beta/dface(ifa))* (1.d0-(mface(ifa)/vol)*&
                    ndx(ifa,ifa))*normfa(idim,ifa)
41              continue
            else
                do 42 idim = 1, ndim
                    yss(idim,ifa,jfa)= (mface(jfa)/vol)*normfa(idim,&
                    jfa) -(beta/(dface(ifa)*vol))*mface(jfa)* ndx(jfa,&
                    ifa)*normfa(idim,ifa)
42              continue
            endif
40      continue
!
    do 50 ifa = 1, nface
        do 50 jfa = 1, nface
            c(ifa,jfa) = 0.d0
            d(ifa,jfa) = 0.d0
            do 50 kfa = 1, nface
                do 52 idim = 1, ndim
                    ky = 0.d0
                    ky1 = 0.d0
                    do 51 jdim = 1, ndim
                        ky = ky + mcone(kfa)*kint(idim,jdim)* yss( jdim,kfa,jfa)
                        ky1 = ky1 + mcone(kfa)*kuni(idim,jdim)* yss(jdim,kfa,jfa)
51                  continue
                    c(ifa,jfa) = c(ifa,jfa) + yss(idim,kfa,ifa)*ky
                    d(ifa,jfa) = d(ifa,jfa) + yss(idim,kfa,ifa)*ky1
52              continue
50          continue
end subroutine
