subroutine lcegeo(nno, npg, ipoids, ivf, idfde,&
                  geom, typmod, compor, ndim, dfdi,&
                  deplm, ddepl, elgeom)
!
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
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/matinv.h'
    include 'asterfort/nmgeom.h'
    include 'asterfort/pmat.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    integer :: nno, npg, ipoids, ivf, idfde, ndim
    character(len=8) :: typmod(2)
    character(len=16) :: compor(*)
    real(kind=8) :: geom(3, nno), elgeom(10, npg), dfdi(nno, 3)
    real(kind=8) :: deplm(3, nno), ddepl(3, nno)
!
! ----------------------------------------------------------------------
!
! CALCUL D'ELEMENTS GEOMETRIQUES SPECIFIQUES AUX LOIS DE COMPORTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG    : NOMBRE DE POINTS DE GAUSS
! IN  IPOIDS : POIDS DES POINTS DE GAUSS
! IN  IVF    : VALEUR  DES FONCTIONS DE FORME
! IN  IDFDE  : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOM   : COORDONEES DES NOEUDS
! IN  TYPMOD : TYPE DE MODELISATION
! IN  COMPOR : COMPORTEMENT
!
! OUT ELGEOM  : TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES AUX LOIS
!               DE COMPORTEMENT (DIMENSION MAXIMALE FIXEE EN DUR, EN
!               FONCTION DU NOMBRE MAXIMAL DE POINT DE GAUSS)
!
!
!
!
    integer :: kpg, k, i, nddl, j
    real(kind=8) :: rac2, lc, dfdx(27), dfdy(27), dfdz(27), poids, r, r8bid
    real(kind=8) :: l(3, 3), fmm(3, 3), df(3, 3), f(3, 3)
    real(kind=8) :: volume, surfac
    real(kind=8) :: deplp(3, 27), geomm(3, 27), epsbid(6), id(3, 3)
    logical :: laxi
    data    id/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    rac2 = sqrt(2.d0)
    laxi = typmod(1) .eq. 'AXIS'
    nddl = ndim*nno
!
! --- CALCUL DE LA LONGUEUR CARACTERISTIQUE POUR LA LOI
! --- BETON_DOUBLE_DP
!
    if (compor(1)(1:15) .eq. 'BETON_DOUBLE_DP' .or.&
        ( compor(1)(1:7) .eq. 'KIT_DDI' .and. compor(9)(1:15) .eq. 'BETON_DOUBLE_DP' )) then
!
        if (typmod(1)(1:2) .eq. '3D') then
!
            volume = 0.d0
            do 10 kpg = 1, npg
                call dfdm3d(nno, kpg, ipoids, idfde, geom,&
                            dfdx, dfdy, dfdz, poids)
                volume = volume + poids
10          continue
            if (npg .ge. 9) then
                lc = volume ** 0.33333333333333d0
            else
                lc = rac2 * volume ** 0.33333333333333d0
            endif
            elseif(typmod(1)(1:6).eq.'D_PLAN' .or.typmod(1)(1:4)&
        .eq.'AXIS')then
            surfac = 0.d0
            do 40 kpg = 1, npg
                k = (kpg-1)*nno
                call dfdm2d(nno, kpg, ipoids, idfde, geom,&
                            dfdx, dfdy, poids)
                if (laxi) then
                    r = 0.d0
                    do 30 i = 1, nno
                        r = r + geom(1,i)*zr(ivf+i+k-1)
30                  continue
                    poids = poids*r
                endif
                surfac = surfac + poids
40          continue
!
            if (npg .ge. 5) then
                lc = surfac ** 0.5d0
            else
                lc = rac2 * surfac ** 0.5d0
            endif
!
        else
            call assert(.false.)
        endif
!
        do 50 kpg = 1, npg
            elgeom(1,kpg) = lc
50      continue
    endif
!
! --- ELEMENTS GEOMETRIQUES POUR META_LEMA_INI
!
    if (compor(1)(1:13) .eq. 'META_LEMA_ANI') then
        if (laxi) then
            do 130 kpg = 1, npg
                elgeom(1,kpg) = 0.d0
                elgeom(2,kpg) = 0.d0
                elgeom(3,kpg) = 0.d0
130          continue
        else
            do 100 kpg = 1, npg
                elgeom(1,kpg) = 0.d0
                elgeom(2,kpg) = 0.d0
                elgeom(3,kpg) = 0.d0
                do 110 i = 1, ndim
                    do 120 k = 1, nno
                        elgeom(i,kpg) = elgeom(i,kpg) + geom(i,k)*zr( ivf-1+nno*(kpg-1)+k)
120                  continue
110              continue
100          continue
        endif
    endif
!
! --- ELEMENTS GEOMETRIQUES POUR MONOCRISTAL: ROTATION DU RESEAU
!
    if (compor(1) .eq. 'MONOCRISTAL') then
!       ROTATION RESEAU DEBUT
!       CALCUL DE L = DF*F-1
        call dcopy(nddl, geom, 1, geomm, 1)
        call daxpy(nddl, 1.d0, deplm, 1, geomm,&
                   1)
        call dcopy(nddl, deplm, 1, deplp, 1)
        call daxpy(nddl, 1.d0, ddepl, 1, deplp,&
                   1)
        do 200 kpg = 1, npg
            call nmgeom(ndim, nno, .false., .true., geom,&
                        kpg, ipoids, ivf, idfde, deplp,&
                        .true., r8bid, dfdi, f, epsbid,&
                        r)
            call nmgeom(ndim, nno, .false., .true., geomm,&
                        kpg, ipoids, ivf, idfde, ddepl,&
                        .true., r8bid, dfdi, df, epsbid,&
                        r)
            call daxpy(9, -1.d0, id, 1, df,&
                       1)
            call matinv('S', 3, f, fmm, r8bid)
            call pmat(3, df, fmm, l)
            do 272 i = 1, 3
                do 273 j = 1, 3
                    elgeom(3*(i-1)+j,kpg)=l(i,j)
273              continue
272          continue
200      continue
    endif
!
end subroutine
