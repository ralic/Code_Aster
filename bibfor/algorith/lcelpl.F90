subroutine lcelpl(mod, loi, nmat, materd, materf,&
                  timed, timef, deps, nvi, vind,&
                  vinf, nr, yd, yf, sigd,&
                  sigf, drdy)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE CRS_1404
!
! ----------------------------------------------------------------
!   MISE A JOUR DES VARIABLES INTERNES EN ELASTICITE
!
!   POST-TRAITEMENTS SPECIFIQUES AUX LOIS
!
!   CAS GENERAL :
!      VINF = VIND
!      VINF(NVI) = 0.0
! ----------------------------------------------------------------
!  IN
!     MOD    :  TYPE DE MODELISATION
!     LOI    :  NOM DE LA LOI
!     NMAT   :  DIMENSION MATER ET DE NBCOMM
!     MATERD :  COEF MATERIAU A T
!     MATERF :  COEF MATERIAU A T+DT
!     TIMED  :  INSTANT T
!     TIMEF  :  INSTANT T+DT
!     IDPLAS :  INDICATEUR PLASTIQUE
!     NVI    :  NOMBRE VARIABLES INTERNES
!     VIND   :  VARIABLES INTERNES A T
!     SIGD   :  CONTRAINTES A T
!     SIGF   :  CONTRAINTES A T+DT
!     NR     :  NB EQUATION SYSTEME INTEGRE A RESOUDRE
!     YD     :  VECTEUR SOLUTION A T
!     YF     :  VECTEUR SOLUTION A T+DT
!  OUT
!     VINF   :  VARIABLES INTERNES A T+DT
!     DRDY   :  MATRICE JACOBIENNE POUR BETON_BURGER_FP
! ----------------------------------------------------------------
!     ------------------------------------------------------------
    include 'asterfort/burjac.h'
    include 'asterfort/burlnf.h'
    include 'asterfort/irrlnf.h'
    include 'asterfort/lceqvn.h'
    common /tdim/   ndt  , ndi
!     ------------------------------------------------------------
    character(len=16) :: loi
    integer :: nmat, nvi, nr, i, j, ndi, ndt
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: vinf(nvi), vind(nvi), dy(nr), drdy(nr, nr)
    real(kind=8) :: timed, timef, dt, yd(nr), yf(nr)
    real(kind=8) :: deps(6), sigf(6), sigd(6)
    character(len=8) :: mod
! ----------------------------------------------------------------
    if (loi(1:7) .eq. 'IRRAD3M') then
        call irrlnf(nmat, materf, vind, 0.0d0, vinf)
    else if (loi(1:15) .eq. 'BETON_BURGER_FP') then
        dt = timef-timed
        call burlnf(nvi, vind, nmat, materd, materf,&
                    dt, nr, yd, yf, vinf,&
                    sigf)
        do 10 i = 1, nr
            dy(i) = yf(i)-yd(i)
            do 20 j = 1, nr
                drdy(i,j) = 0.d0
20          continue
10      continue
        call burjac(mod, nmat, materd, materf, nvi,&
                    vind, timed, timef, yd, yf,&
                    dy, nr, drdy)
    else if (loi(1:4).eq.'LETK') then
        call lceqvn(nvi-3, vind, vinf)
        vinf(5) = 0.d0
        vinf(6) = 0.d0
        vinf(7) = 0.d0
    else if (loi(1:6).eq.'HUJEUX') then
! --- PAS DE MODIFICATION PARTICULIERE
! --- CAS DIFFERENT DE LA GENERALITE
    else
!
! --- CAS GENERAL :
!        VINF  = VIND ,  ETAT A T+DT = VINF(NVI) = 0 = ELASTIQUE
        call lceqvn(nvi-1, vind, vinf)
        vinf(nvi) = 0.0d0
    endif
!
end subroutine
