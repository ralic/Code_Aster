subroutine inmat5(elrefa, nno, nnos, npg, mganos,&
                  mgano2)
    implicit   none
#include "jeveux.h"
#include "asterfort/assert.h"
    integer :: nnos, npg, nno, nbpgmx, nbnomx
    parameter (nbpgmx=1000,nbnomx=27)
    real(kind=8) :: mganos(nbpgmx, nbnomx), mgano2(nbpgmx, nbnomx)
    character(len=8) :: elrefa
! ----------------------------------------------------------------------
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
! ======================================================================
!     BUT :
!     POUR LES ELEMENTS QUADRATIQUES (NNOS /= NNO),
!     CALCULER LA MATRICE DE PASSAGE GAUSS -> NOEUDS (MGANO2)
!     A PARTIR DE LA MATRICE DE PASSAGE GAUSS -> NOEUDS_SOMMETS (MGANOS)
! ----------------------------------------------------------------------
    integer :: kpg, kno, knos, k
    real(kind=8) :: demi, nosom(nbnomx, nbnomx)
!
!     NBPGMX, NBNOMX SE REFERER A ELRACA
!
! DEB ------------------------------------------------------------------
    demi = 0.5d0
!
!
!     -- SI NNO=NNOS, IL N'Y A QU'A COPIER :
!     --------------------------------------
    if (nnos .eq. nno) then
        do 20,kpg = 1,npg
        do 10,kno = 1,nno
        mgano2(kpg,kno) = mganos(kpg,kno)
10      continue
20      continue
        goto 100
    endif
!
!
!     1) CALCUL DE NOSOM :
!     ---------------------
    do 40,kno = 1,nno
    do 30,knos = 1,nnos
    nosom(kno,knos) = 0.d0
30  continue
    40 end do
!
!     1.1) LES NOEUDS SOMMETS SONT TOUJOURS LES 1ERS :
    do 50,knos = 1,nnos
    nosom(knos,knos) = 1.d0
    50 end do
!
!
!     1.2) LES NOEUDS MILIEUX SE DEDUISENT DES SOMMETS :
    if ((elrefa.eq.'H20') .or. (elrefa.eq.'H27')) then
        ASSERT(nnos.eq.8)
        nosom(9,1) = demi
        nosom(9,2) = demi
        nosom(10,2) = demi
        nosom(10,3) = demi
        nosom(11,3) = demi
        nosom(11,4) = demi
        nosom(12,1) = demi
        nosom(12,4) = demi
        nosom(13,1) = demi
        nosom(13,5) = demi
        nosom(14,2) = demi
        nosom(14,6) = demi
        nosom(15,3) = demi
        nosom(15,7) = demi
        nosom(16,4) = demi
        nosom(16,8) = demi
        nosom(17,5) = demi
        nosom(17,6) = demi
        nosom(18,6) = demi
        nosom(18,7) = demi
        nosom(19,7) = demi
        nosom(19,8) = demi
        nosom(20,5) = demi
        nosom(20,8) = demi
!
        if (elrefa .eq. 'H27') then
            nosom(21,1) = demi/2
            nosom(21,2) = demi/2
            nosom(21,3) = demi/2
            nosom(21,4) = demi/2
!
            nosom(22,1) = demi/2
            nosom(22,2) = demi/2
            nosom(22,5) = demi/2
            nosom(22,6) = demi/2
!
            nosom(23,2) = demi/2
            nosom(23,3) = demi/2
            nosom(23,6) = demi/2
            nosom(23,7) = demi/2
!
            nosom(24,3) = demi/2
            nosom(24,4) = demi/2
            nosom(24,7) = demi/2
            nosom(24,8) = demi/2
!
            nosom(25,1) = demi/2
            nosom(25,4) = demi/2
            nosom(25,5) = demi/2
            nosom(25,8) = demi/2
!
            nosom(26,5) = demi/2
            nosom(26,6) = demi/2
            nosom(26,7) = demi/2
            nosom(26,8) = demi/2
!
            do 60,k = 1,8
            nosom(nbnomx,k) = demi/4
60          continue
        endif
!
!
    else if ((elrefa.eq.'P15').or.(elrefa.eq.'P18')) then
        ASSERT(nnos.eq.6)
        nosom(7,1) = demi
        nosom(7,2) = demi
        nosom(8,2) = demi
        nosom(8,3) = demi
        nosom(9,1) = demi
        nosom(9,3) = demi
        nosom(10,1) = demi
        nosom(10,4) = demi
        nosom(11,2) = demi
        nosom(11,5) = demi
        nosom(12,3) = demi
        nosom(12,6) = demi
        nosom(13,4) = demi
        nosom(13,5) = demi
        nosom(14,5) = demi
        nosom(14,6) = demi
        nosom(15,4) = demi
        nosom(15,6) = demi
!
        if (elrefa .eq. 'P18') then
!
            nosom(16,2) = demi/2
            nosom(16,1) = demi/2
            nosom(16,4) = demi/2
            nosom(16,5) = demi/2
!
            nosom(17,2) = demi/2
            nosom(17,5) = demi/2
            nosom(17,6) = demi/2
            nosom(17,3) = demi/2
!
            nosom(18,1) = demi/2
            nosom(18,3) = demi/2
            nosom(18,6) = demi/2
            nosom(18,4) = demi/2
!
        endif
!
    else if (elrefa.eq.'T10') then
        ASSERT(nnos.eq.4)
        nosom(5,1) = demi
        nosom(5,2) = demi
        nosom(6,2) = demi
        nosom(6,3) = demi
        nosom(7,1) = demi
        nosom(7,3) = demi
        nosom(8,1) = demi
        nosom(8,4) = demi
        nosom(9,2) = demi
        nosom(9,4) = demi
        nosom(10,3) = demi
        nosom(10,4) = demi
!
!
    else if (elrefa.eq.'P13') then
        ASSERT(nnos.eq.5)
        nosom(6,1) = demi
        nosom(6,2) = demi
        nosom(7,2) = demi
        nosom(7,3) = demi
        nosom(8,3) = demi
        nosom(8,4) = demi
        nosom(9,1) = demi
        nosom(9,4) = demi
        nosom(10,1) = demi
        nosom(10,5) = demi
        nosom(11,2) = demi
        nosom(11,5) = demi
        nosom(12,3) = demi
        nosom(12,5) = demi
        nosom(13,4) = demi
        nosom(13,5) = demi
!
!
    else if (elrefa.eq.'TR6') then
        ASSERT(nnos.eq.3)
        nosom(4,1) = demi
        nosom(4,2) = demi
        nosom(5,2) = demi
        nosom(5,3) = demi
        nosom(6,3) = demi
        nosom(6,1) = demi
!
!
    else if (elrefa.eq.'TR7') then
        ASSERT(nnos.eq.3)
        nosom(4,1) = demi
        nosom(4,2) = demi
        nosom(5,2) = demi
        nosom(5,3) = demi
        nosom(6,3) = demi
        nosom(6,1) = demi
        nosom(7,1) = demi/2
        nosom(7,2) = demi/2
        nosom(7,3) = demi/2
!
!
    else if (elrefa.eq.'QU8') then
        ASSERT(nnos.eq.4)
        nosom(5,1) = demi
        nosom(5,2) = demi
        nosom(6,2) = demi
        nosom(6,3) = demi
        nosom(7,3) = demi
        nosom(7,4) = demi
        nosom(8,4) = demi
        nosom(8,1) = demi
!
!
    else if (elrefa.eq.'QU9') then
        ASSERT(nnos.eq.4)
        nosom(5,1) = demi
        nosom(5,2) = demi
        nosom(6,2) = demi
        nosom(6,3) = demi
        nosom(7,3) = demi
        nosom(7,4) = demi
        nosom(8,4) = demi
        nosom(8,1) = demi
        nosom(9,1) = demi/2
        nosom(9,2) = demi/2
        nosom(9,3) = demi/2
        nosom(9,4) = demi/2
!
!
    else if (elrefa.eq.'SE3') then
        ASSERT(nnos.eq.2)
        nosom(3,1) = demi
        nosom(3,2) = demi
!
!
    else if (elrefa.eq.'SE4') then
        ASSERT(nnos.eq.2)
        nosom(3,1) = 2.d0/3.d0
        nosom(3,2) = 1.d0/3.d0
        nosom(4,1) = 1.d0/3.d0
        nosom(4,2) = 2.d0/3.d0
!
    else
        ASSERT(.false.)
    endif
!
!
!     2) ON MULTIPLIE : MGANO2=MGANOS * NOSOM :
!     ----------------------------------
    do 90,kno = 1,nno
    do 80,kpg = 1,npg
    do 70,knos = 1,nnos
    mgano2(kpg,kno) = mgano2(kpg,kno) + mganos(kpg,knos)* nosom(kno,knos)
!
70  continue
80  continue
    90 end do
!
!
!
100  continue
end subroutine
