subroutine dxtloe(flex, memb, mefl, ctor, coupmf,&
                  depl, ener)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/utvtsv.h"
    aster_logical :: coupmf
    real(kind=8) :: flex(*), memb(*), mefl(*), ctor
    real(kind=8) :: depl(*), ener(*)
!-----------------------------------------------------
!     CALCUL DE L'ENERGIE DE DEFORMATION OU CINETIQUE
!            SUR UNE MAILLE QUADRANGLE
!     IN  FLEX   : MATRICE DE FLEXION CARREE
!     IN  MEMB   : MATRICE DE MEMBRANE CARREE
!     IN  MEFL   : MATRICE MEMBRANE - FLEXION CARREE
!     IN  CTOR   : COEFF DE TORSION
!     IN  DEPL   : DEPLACEMENT DANS LE REPERE LOCAL
!     OUT ENER   : 3 TERMES POUR ENER_POT (EPOT_ELEM) OU
!                           POUR ENER_CIN (ECIN_ELEM)
!-----------------------
    integer :: if(45), jf(45)
    integer :: im(21), jm(21)
    integer :: ifm(36), jfm(36)
    integer :: imf(18), jmf(18)
    integer :: jz(3)
    integer :: km(6), kf(9)
    real(kind=8) :: coef
    real(kind=8) :: cf(45), cfm(36), cmf(18)
    real(kind=8) :: deplm(6), deplf(9)
    real(kind=8) :: matloc(171), matf(45), matm(21)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, k
!-----------------------------------------------------------------------
    data cf/1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,1.d0,&
     &     2*-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,&
     &     2*1.d0,-1.d0,1.d0,2*-1.d0,1.d0,2*-1.d0,2*1.d0,-1.d0,2*1.d0,&
     &     -1.d0,2*1.d0,-1.d0,1.d0/
    data cfm/2*1.d0,2*-1.d0,2*1.d0,2*1.d0,2*-1.d0,2*1.d0,2*1.d0,&
     &     2*-1.d0,2*1.d0,2*1.d0,2*-1.d0,2*1.d0,2*1.d0,2*-1.d0,2*1.d0,&
     &     2*1.d0,2*-1.d0,2*1.d0/
    data cmf/1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,&
     &     -1.d0,2*1.d0,-1.d0,1.d0/
!     ------------------------------------------------------------------
    data jf/6,9,10,13,14,15,39,40,41,45,48,49,50,54,55,58,59,60,64,65,&
     &     66,108,109,110,114,115,116,120,123,124,125,129,130,131,135,&
     &     136,139,140,141,145,146,147,151,152,153/
    data if/1,19,21,10,12,11,28,30,29,31,46,48,47,49,51,37,39,38,40,&
     &     42,41,55,57,56,58,60,59,61,73,75,74,76,78,77,79,81,64,66,65,&
     &     67,69,68,70,72,71/
!     ------------------------------------------------------------------
    data jm/1,2,3,22,23,28,29,30,35,36,79,80,85,86,91,92,93,98,99,104,&
     &     105/
    data im/1,7,8,13,14,15,19,20,21,22,25,26,27,28,29,31,32,33,34,35,&
     &     36/
!     ------------------------------------------------------------------
    data jfm/4,5,7,8,11,12,37,38,46,47,56,57,43,44,52,53,62,63,106,&
     &     107,121,122,137,138,112,113,127,128,143,144,118,119,133,134,&
     &     149,150/
    data ifm/1,2,13,14,7,8,19,20,31,32,25,26,21,22,33,34,27,28,37,38,&
     &     49,50,43,44,39,40,51,52,45,46,41,42,53,54,47,48/
!     ------------------------------------------------------------------
    data jmf/24,25,26,31,32,33,81,82,83,94,95,96,87,88,89,100,101,102/
    data imf/3,15,9,4,16,10,5,17,11,6,18,12,23,35,29,24,36,30/
!     ------------------------------------------------------------------
    data jz/21,78,171/
!     ------------------------------------------------------------------
    data km/1,2,7,8,13,14/
    data kf/3,4,5,9,10,11,15,16,17/
!     ------------------------------------------------------------------
!                          ---- RAZ MATLOC
    do 10 i = 1, 171
        matloc(i) = 0.d0
 10 end do
!                          ---- TERMES DE FLEXION
    do 20 k = 1, 45
        matloc(jf(k)) = cf(k)*flex(if(k))
        matf(k) = matloc(jf(k))
 20 end do
!                          ---- TERMES DE MEMBRANE
    do 30 k = 1, 21
        matloc(jm(k)) = memb(im(k))
        matm(k) = matloc(jm(k))
 30 end do
!                          ---- TERMES DE COUPLAGE FLEXION/MEMBRANE
    do 40 k = 1, 36
        matloc(jfm(k)) = cfm(k)*mefl(ifm(k))
 40 end do
!                          ---- TERMES DE COUPLAGE MEMBRANE/FLEXION
    do 50 k = 1, 18
        matloc(jmf(k)) = cmf(k)*mefl(imf(k))
 50 end do
!                          ---- TERMES DE ROTATION / Z
    coef = ctor*min(flex(11),flex(21),flex(41),flex(51),flex(71), flex(81))
    matloc(jz(1)) = coef
    matloc(jz(2)) = coef
    matloc(jz(3)) = coef
!     ------------------------------------------------------------------
    call utvtsv('ZERO', 18, matloc, depl, ener(1))
    if (coupmf) then
        ener(2) = 0.d0
        ener(3) = 0.d0
    else
!        --------- ENER EN MEMBRANE ----------
        do 60 k = 1, 6
            deplm(k) = depl(km(k))
 60     continue
        call utvtsv('ZERO', 6, matm, deplm, ener(2))
!        --------- ENER EN FLEXION ----------
        do 70 k = 1, 9
            deplf(k) = depl(kf(k))
 70     continue
        call utvtsv('ZERO', 9, matf, deplf, ener(3))
    endif
    ener(1) = 0.5d0*ener(1)
    if (abs(ener(1)) .gt. 1.d-6) then
        ener(2) = 0.5d0*ener(2)/ener(1)
        ener(3) = 0.5d0*ener(3)/ener(1)
    else
        ener(2) = 0.d0
        ener(3) = 0.d0
    endif
end subroutine
