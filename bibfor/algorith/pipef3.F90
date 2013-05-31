subroutine pipef3(ndim, nno, nddl, npg, lgpg,&
                  wref, vff, dfde, mate, geom,&
                  vim, ddepl, deplm, ddepl0, ddepl1,&
                  dtau, copilo, typmod)
!
! ======================================================================
! COPYRIGHT (C) 2007 NECS - BRUNO ZUBER   WWW.NECS.FR
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
! aslint: disable=W1306
    implicit none
    include 'asterc/r8vide.h'
    include 'asterfort/nmfici.h'
    include 'asterfort/pipeba.h'
    include 'asterfort/r8inir.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    integer :: mate, npg, lgpg, nno, ndim, nddl
    real(kind=8) :: geom(nddl), vim(lgpg, npg), ddepl(nddl), deplm(nddl)
    real(kind=8) :: wref(npg), vff(nno, npg), dfde(2, nno, npg)
    real(kind=8) :: ddepl0(nddl), ddepl1(nddl), dtau, copilo(5, npg)
    character(len=8) :: typmod(2)
!
!-----------------------------------------------------------------------
!
!  PILOTAGE PRED_ELAS POUR LES ELEMENTS DE JOINT 3D
!
! IN  : GEOM, MATE, VIM, DDEPL, DEPLM, DDEPL0, DDELP1, DTAU, NPG
! OUT : COPILO
!-----------------------------------------------------------------------
!
    integer :: i, j, kpg
    real(kind=8) :: up(nddl), ud(nddl), sup(3), sud(3), b(3, 60), poids
!-----------------------------------------------------------------------
!
!
! DEPLACEMENT U(ETA) = UP + ETA * UD
!
    call dcopy(nddl, deplm, 1, up, 1)
    call daxpy(nddl, 1.d0, ddepl, 1, up,&
               1)
    call daxpy(nddl, 1.d0, ddepl0, 1, up,&
               1)
    call dcopy(nddl, ddepl1, 1, ud, 1)
!
! BOUCLE SUR LES POINTS DE GAUSS :
!
    do 10 kpg = 1, npg
!
!      SAUT AU POINT DE GAUSS : SU(ETA) = SUP + ETA * SUD
        call nmfici(nno, nddl, wref(kpg), vff(1, kpg), dfde(1, 1, kpg),&
                    geom, poids, b)
!
        do 30 i = 1, 3
            sup(i) = 0.d0
            sud(i) = 0.d0
            do 40 j = 1, nddl
                sup(i) = sup(i) + b(i,j)*up(j)
                sud(i) = sud(i) + b(i,j)*ud(j)
40          continue
30      continue
!
!      INITIALISATION DES COEFFICIENTS DE PILOTAGE
        call r8inir(4, 0.d0, copilo(1, kpg), 1)
        copilo(5,kpg) = r8vide()
!
!      APPEL DU PILOTAGE PRED_ELAS SPECIFIQUE A LA LOI DE COMPORTEMENT
        call pipeba(3, mate, sup, sud, vim(1, kpg),&
                    dtau, copilo(1, kpg))
!
10  end do
!
end subroutine
