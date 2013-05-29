subroutine nmpr3d(mode, nno, npg, poidsg, vff,&
                  dff, geom, p, vect, matc)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    include 'asterfort/assert.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/subaco.h'
    include 'asterfort/subacv.h'
    include 'asterfort/sumetr.h'
    integer :: mode, nno, npg
    real(kind=8) :: poidsg(npg), vff(nno, npg), dff(2, nno, npg)
    real(kind=8) :: geom(3, nno), p(npg)
    real(kind=8) :: vect(3, nno), matc(3, nno, 3, nno)
!
!.......................................................................
!     MODE=1 : CALCUL DU SECOND MEMBRE POUR DES EFFORTS DE PRESSION
!     MODE=2 : CALCUL DE LA RIGIDITE DUE A LA PRESSION (SI SUIVEUSE)
!.......................................................................
! IN  MODE    SELECTION SECOND MEMBRE (1) OU BIEN RIGIDITE (2)
! IN  NNO     NOMBRE DE NOEUDS
! IN  NPG     NOMBRE DE POINTS D'INTEGRATION
! IN  POIDSG  POIDS DES POINTS D'INTEGRATION
! IN  VFF     VALEUR DES FONCTIONS DE FORME AUX POINTS D'INTEGRATION
! IN  DFF     DERIVEE DES FONCTIONS DE FORME AUX POINTS D'INTEGRATION
! IN  GEOM    COORDONNEES DES NOEUDS
! IN  P       PRESSION AUX POINTS D'INTEGRATION
! OUT VECT    VECTEUR SECOND MEMBRE                      (MODE = 1)
! OUT MATC    MATRICE CARREE NON SYMETRIQUE DE RIGIDITE  (MODE = 2)
!.......................................................................
!
    integer :: kpg, n, i, m, j
    real(kind=8) :: cova(3, 3), metr(2, 2), jac, cnva(3, 2)
    real(kind=8) :: t1, t2, t3, t, acv(2, 2)
!
!
!    INITIALISATION
    if (mode .eq. 1) call r8inir(nno*3, 0.d0, vect, 1)
    if (mode .eq. 2) call r8inir(nno*nno*9, 0.d0, matc, 1)
!
!    INTEGRATION AUX POINTS DE GAUSS
!
    do 100 kpg = 1, npg
!
!      CALCUL DES ELEMENTS GEOMETRIQUES DIFFERENTIELS
        call subaco(nno, dff(1, 1, kpg), geom, cova)
        call sumetr(cova, metr, jac)
!
!      CALCUL DU SECOND MEMBRE
        if (mode .eq. 1) then
            do 10 n = 1, nno
                do 11 i = 1, 3
                    vect(i,n) = vect(i,n) - poidsg(kpg)*jac * p(kpg) * cova(i,3)*vff(n,kpg)
11              continue
10          continue
!
!      CALCUL DE LA RIGIDITE
        else if (mode.eq.2) then
            call subacv(cova, metr, jac, cnva, acv)
!
!        MATRICE NON SYMETRIQUE DV.MATC.DU
            do 20 m = 1, nno
                do 21 j = 1, 3
                    do 30 n = 1, nno
                        do 31 i = 1, 3
                            t1 = (&
                                 dff(1, m, kpg)*cnva(j, 1) + dff(2, m, kpg)*cnva(j, 2)) * vff(n,&
                                 kpg)*cova(i, 3&
                                 )
                            t2 = dff(1,m,kpg)*cova(j,3) * vff(n,kpg)* cnva(i,1)
                            t3 = dff(2,m,kpg)*cova(j,3) * vff(n,kpg)* cnva(i,2)
                            t = poidsg(kpg) * p(kpg) * jac * (t1 - t2 - t3)
                            matc(i,n,j,m) = matc(i,n,j,m) + t
31                      continue
30                  continue
21              continue
20          continue
        else
            call assert(.false.)
        endif
!
100  end do
!
end subroutine
