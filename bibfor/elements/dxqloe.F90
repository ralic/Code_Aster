subroutine dxqloe(flex, memb, mefl, ctor, coupmf,&
                  depl, ener)
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
    implicit none
#include "jeveux.h"
#include "asterfort/utvtsv.h"
    logical(kind=1) :: coupmf
    real(kind=8) :: flex(*), memb(*), mefl(*), ctor
    real(kind=8) :: depl(*), ener(*)
!----------------------------------------------------------
!     CALCUL DE L'ENERGIE DE DEFORMATION OU CINETIQUE
!            SUR UNE MAILLE TRIANGLE
!     IN  FLEX   : MATRICE DE FLEXION CARREE
!     IN  MEMB   : MATRICE DE MEMBRANE CARREE
!     IN  MEFL   : MATRICE MEMBRANE - FLEXION CARREE
!     IN  CTOR   : COEFF DE TORSION
!     IN  DEPL   : DEPLACEMENT DANS LE REPERE LOCAL
!     OUT ENER   : 3 TERMES POUR ENER_POT (EPOT_ELEM) OU
!                           POUR ENER_CIN (ECIN_ELEM)
!----------------------------------------------------------
    integer :: if(78), jf(78)
    integer :: im(36), jm(36)
    integer :: ifm(60), jfm(60)
    integer :: imf(36), jmf(36)
    integer :: jz(4)
    integer :: km(8), kf(12)
    real(kind=8) :: coef
    real(kind=8) :: cf(78), cfm(60), cmf(36)
    real(kind=8) :: deplm(8), deplf(12)
    real(kind=8) :: matloc(300), matf(78), matm(36)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, k
!-----------------------------------------------------------------------
    data cf/1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,1.d0,&
     &     2*-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,&
     &     2*1.d0,-1.d0,1.d0,2*-1.d0,1.d0,2*-1.d0,2*1.d0,-1.d0,2*1.d0,&
     &     -1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,&
     &     2*1.d0,-1.d0,1.d0,2*-1.d0,1.d0,2*-1.d0,1.d0,2*-1.d0,2*1.d0,&
     &     -1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,1.d0/
    data cfm/2*1.d0,2*-1.d0,2*1.d0,2*1.d0,2*-1.d0,2*1.d0,2*1.d0,&
     &     2*-1.d0,2*1.d0,2*1.d0,2*-1.d0,2*1.d0,2*1.d0,2*-1.d0,2*1.d0,&
     &     2*1.d0,2*-1.d0,2*1.d0,2*1.d0,2*-1.d0,2*1.d0,2*1.d0,2*-1.d0,&
     &     2*1.d0,2*1.d0,2*-1.d0,2*1.d0,2*1.d0,2*-1.d0,2*1.d0/
    data cmf/1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,&
     &     -1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,&
     &     2*1.d0,-1.d0,2*1.d0,-1.d0,2*1.d0,-1.d0,1.d0/
!     ------------------------------------------------------------------
    data jf/6,9,10,13,14,15,39,40,41,45,48,49,50,54,55,58,59,60,64,65,&
     &     66,108,109,110,114,115,116,120,123,124,125,129,130,131,135,&
     &     136,139,140,141,145,146,147,151,152,153,213,214,215,219,220,&
     &     221,225,226,227,231,234,235,236,240,241,242,246,247,248,252,&
     &     253,256,257,258,262,263,264,268,269,270,274,275,276/
    data if/1,25,27,13,15,14,37,39,38,40,61,63,62,64,66,49,51,50,52,&
     &     54,53,73,75,74,76,78,77,79,97,99,98,100,102,101,103,105,85,&
     &     87,86,88,90,89,91,93,92,109,111,110,112,114,113,115,117,116,&
     &     118,133,135,134,136,138,137,139,141,140,142,144,121,123,122,&
     &     124,126,125,127,129,128,130,132,131/
!     ------------------------------------------------------------------
    data jm/1,2,3,22,23,28,29,30,35,36,79,80,85,86,91,92,93,98,99,104,&
     &     105,172,173,178,179,184,185,190,191,192,197,198,203,204,209,&
     &     210/
    data im/1,9,10,17,18,19,25,26,27,28,33,34,35,36,37,41,42,43,44,45,&
     &     46,49,50,51,52,53,54,55,57,58,59,60,61,62,63,64/
!     ------------------------------------------------------------------
    data jmf/24,25,26,31,32,33,81,82,83,94,95,96,87,88,89,100,101,102,&
     &     174,175,176,193,194,195,180,181,182,199,200,201,186,187,188,&
     &     205,206,207/
    data imf/3,19,11,4,20,12,5,21,13,6,22,14,29,45,37,30,46,38,7,23,&
     &     15,8,24,16,31,47,39,32,48,40,55,71,63,56,72,64/
!     ------------------------------------------------------------------
    data jfm/4,5,7,8,11,12,37,38,46,47,56,57,43,44,52,53,62,63,106,&
     &     107,121,122,137,138,112,113,127,128,143,144,118,119,133,134,&
     &     149,150,211,212,232,233,254,255,217,218,238,239,260,261,223,&
     &     224,244,245,266,267,229,230,250,251,272,273/
    data ifm/1,2,17,18,9,10,25,26,41,42,33,34,27,28,43,44,35,36,49,50,&
     &     65,66,57,58,51,52,67,68,59,60,53,54,69,70,61,62,73,74,89,90,&
     &     81,82,75,76,91,92,83,84,77,78,93,94,85,86,79,80,95,96,87,88/
!     ------------------------------------------------------------------
    data km/1,2,7,8,13,14,19,20/
    data kf/3,4,5,9,10,11,15,16,17,21,22,23/
!     ------------------------------------------------------------------
    data jz/21,78,171,300/
!     ------------------------------------------------------------------
!                          ---- RAZ MATLOC
    do 10 i = 1, 300
        matloc(i) = 0.d0
10  end do
!                          ---- TERMES DE FLEXION
    do 20 k = 1, 78
        matloc(jf(k)) = cf(k)*flex(if(k))
        matf(k) = matloc(jf(k))
20  end do
!                          ---- TERMES DE MEMBRANE
    do 30 k = 1, 36
        matloc(jm(k)) = memb(im(k))
        matm(k) = matloc(jm(k))
30  end do
!                          ---- TERMES DE COUPLAGE FLEXION/MEMBRANE
    do 40 k = 1, 60
        matloc(jfm(k)) = cfm(k)*mefl(ifm(k))
40  end do
!                          ---- TERMES DE COUPLAGE MEMBRANE/FLEXION
    do 50 k = 1, 36
        matloc(jmf(k)) = cmf(k)*mefl(imf(k))
50  end do
!                          ---- TERMES DE ROTATION / Z
    coef = ctor*min(&
           flex(14), flex(27), flex(53), flex(66), flex(92), flex(105), flex(131), flex(144))
    matloc(jz(1)) = coef
    matloc(jz(2)) = coef
    matloc(jz(3)) = coef
    matloc(jz(4)) = coef
!     ------------------------------------------------------------------
    call utvtsv('ZERO', 24, matloc, depl, ener(1))
    if (coupmf) then
        ener(2) = 0.d0
        ener(3) = 0.d0
    else
!        --------- ENER EN MEMBRANE ----------
        do 60 k = 1, 8
            deplm(k) = depl(km(k))
60      continue
        call utvtsv('ZERO', 8, matm, deplm, ener(2))
!        --------- ENER EN FLEXION ----------
        do 70 k = 1, 12
            deplf(k) = depl(kf(k))
70      continue
        call utvtsv('ZERO', 12, matf, deplf, ener(3))
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
