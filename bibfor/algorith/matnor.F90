subroutine matnor(fami, kpg, ksp, imat, nmat,&
                  poum, coefel, coefpl, ndt, nvi,&
                  nr)
    implicit none
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
!       ----------------------------------------------------------------
!     NORTON   : RECUPERATION DU MATERIAU A T ET T+DT
!                  NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
!                  MATER(*,1) = E , NU , ALPHA
!                  MATER(*,2) = EPS0 , K , H1 , H2 , DELTA1 , DELTA2 ,
!                               H1ST , H2ST , KC , BIGS , SMALLS ,
!                               EPSC
!                  VARIABLES INTERNES :
!     ----------------------------------------------------------------
!     IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!         KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
!         IMAT   :  ADRESSE DU MATERIAU CODE
!         NMAT   :  DIMENSION  DE MATER
!         POUM   :  '+' OU '-'
!         NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!     OUT COEFEL :  COEFFICIENTS MATERIAU POUR LA PARTIE ELASTIQUE
!         COEFPL :  COEFFICIENTS MATERIAU POUR LA PARTIE NON LINEAIRE
!         NVI    :  NB DE VARIABLES INTERNES
!         NR     :  NB DE COMPOSANTES SYSTEME NL
!     ----------------------------------------------------------------
#include "asterfort/rcvalb.h"
    integer :: kpg, ksp, nmat, ndt, nvi, nr, imat, cerr(5)
    real(kind=8) :: coefel(nmat), coefpl(nmat)
    character(len=*) :: fami, poum
    character(len=8) :: nomc(5)
!
! -   RECUPERATION MATERIAU -----------------------------------------
!
    nomc(1) = 'E'
    nomc(2) = 'NU'
    nomc(3) = 'ALPHA'
    nomc(4) = 'N'
    nomc(5) = 'UN_SUR_K'
!
!
! -   RECUPERATION MATERIAU A (T)
!
    call rcvalb(fami, kpg, ksp, poum, imat,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                3, nomc(1), coefel, cerr(1), 0)
!
    if (cerr(3) .ne. 0) coefel(3) = 0.d0
!
    call rcvalb(fami, kpg, ksp, poum, imat,&
                ' ', 'LEMAITRE', 0, ' ', 0.d0,&
                2, nomc(4), coefpl, cerr(4), 1)
!
!     NOMBRE DE COEF MATERIAU
    coefpl(nmat)=2
    nvi=7
!
!     LA LOI 'NORTON' NE STOCKE PAS L'INDICATEUR DE PLASTICITE
    nr = ndt + nvi
!
end subroutine
