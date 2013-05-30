subroutine pidefo(ndim, npg, kpg, compor, fm,&
                  epsm, epsp, epsd, copilo)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/r8inir.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    include 'blas/dnrm2.h'
    integer :: ndim, kpg, npg
    character(len=16) :: compor(*)
    real(kind=8) :: epsm(6), epsp(6), epsd(6)
    real(kind=8) :: fm(3, 3)
    real(kind=8) :: copilo(5, npg)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS/DEFORMATION)
!
! PILOTAGE PAR DEFORMATION
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NPG    : NOMBRE DE POINTS DE GAUSS
! IN  KPG    : NUMERO DU POINT DE GAUSS
! IN  COMPOR : COMPORTEMENT
! IN  FM     : GRADIENT DE LA TRANSFORMATION AU TEMPS MOINS
! IN  EPSM   : DEFORMATIONS AU TEMPS MOINS
! IN  EPSP   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
! IN  EPSD   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
! OUT COPILO : COEFFICIENTS A0 ET A1 POUR CHAQUE POINT DE GAUSS
!
!
!
!
    logical :: grand
    integer :: ndimsi
    integer :: indi(6), indj(6), prac(6)
    real(kind=8) :: ff
    real(kind=8) :: rac2
    real(kind=8) :: em(6), epsmno
    integer :: ij, kl, i, j, k, l
!
    data indi /1,2,3,2,3,3/
    data indj /1,2,3,1,1,2/
    data prac /0,0,0,1,1,1/
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    rac2 = sqrt(2.d0)
    grand = compor(3) .ne. 'PETIT'
    ndimsi = 2*ndim
!
! --- TRANSPORT DU TENSEUR DES DEFORMATIONS E := F E FT
!
    if (grand) then
        call dcopy(ndimsi, epsm, 1, em, 1)
        call r8inir(ndimsi, 0.d0, epsm, 1)
!
        do 50 ij = 1, ndimsi
            do 55 kl = 1, ndimsi
                i = indi(ij)
                j = indj(ij)
                k = indi(kl)
                l = indj(kl)
                ff = (fm(i,k)*fm(j,l) + fm(i,l)*fm(j,k)) / 2
                ff = ff * rac2**prac(ij) * rac2**prac(kl)
                epsm(ij) = epsm(ij) + ff*em(kl)
55          continue
50      continue
    endif
!
! --- INCREMENT DE DEFORMATION PROJETE
!
    epsmno = dnrm2(ndimsi,epsm ,1)
    copilo(1,kpg) = ddot(ndimsi, epsm,1, epsp,1)/epsmno
    copilo(2,kpg) = ddot(ndimsi, epsm,1, epsd,1)/epsmno
!
!
end subroutine
