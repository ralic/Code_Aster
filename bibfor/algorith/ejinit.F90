subroutine ejinit(nomte, iu, ip)
!
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
! person_in_charge: jerome.laverne at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
    character(len=16) :: nomte
    integer :: iu(3, 16), ip(4)
! ----------------------------------------------------------------------
!            DECALAGE D'INDICE POUR LES ELEMENTS DE JOINT HM
! ----------------------------------------------------------------------
! IN  NOMTE  NOM DE L'ELEMENT FINI
! OUT IU     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
! OUT IP     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION
! ----------------------------------------------------------------------
!
! EXEMPLE POUR QUAD8 (6 NOEUDS DEPL + 2 NOEUDS PRESS)
!     NUMEROTATION DE DDL :
!     U1_X, U1_Y   - IU(1,1) IU(2,1)
!     U2_X, U2_Y   - IU(1,2) IU(2,2)
!     U3_X, U3_Y   - IU(1,5) IU(2,5)
!     U4_X, U4_Y   - IU(1,4) IU(2,4)
!     U5_X, U5_Y   - IU(1,3) IU(2,3)
!     P6           - IP(2)
!     U7_X, U7_Y   - IU(1,6) IU(2,6)
!     P8           - IP(1)
!
!     RACCOURCIS
!          IU(1,1:3) => DEPL_X JOINT + (NOEUDS 1,2 et 5)
!          IU(2,1:3) => DEPL_Y JOINT + (NOEUDS 1,2 et 5)
!          IU(1,4:6) => DEPL_X JOINT - (NOEUDS 4,3 et 7)
!          IU(2,4:6) => DEPL_Y JOINT - (NOEUDS 4,3 et 7)
!          IP(1:2)   => PRESS (NOEUDS 8 et 6)
! ----------------------------------------------------------------------
!
    integer :: n
    integer :: uh20(16), ph20(4)
    integer :: up15(12), pp15(3)
    integer :: uq8(6), pq8(2)
    aster_logical :: ifqu8, ifh20, ifp15
! ----------------------------------------------------------------------
    data uh20 /1,2,3,4,9,10,11,12,5,6,7,8,17,18,19,20/
    data ph20 /13,14,15,16/
!
    data up15 /1,2,3,7,8,9,4,5,6,13,14,15/
!
    data pp15 /10,11,12/
!
    data uq8  /1,2,5,4,3,7/
    data pq8  /8,6/
! ----------------------------------------------------------------------
!     INDICATEURS DE TYPE DE MAILLE : QUAD8, PENTA15 ET HEXA20
!
    ifqu8 = (nomte.eq.'EJHYME_PLQU8') .or. (nomte.eq.'EJHYME_AXQU8') .or. (nomte.eq.'MFPLQU8')
    ifp15 = (nomte.eq.'EJHYME_PENTA15') .or. (nomte.eq.'MEFI_PENTA15')
    ifh20 = (nomte.eq.'EJHYME_HEXA20') .or. (nomte.eq.'MEFI_HEXA20')
!
!
    if (ifh20) then
!
        do 10 n = 1, 12
            iu(1,n) = 1 + (uh20(n)-1)*3
            iu(2,n) = 2 + (uh20(n)-1)*3
            iu(3,n) = 3 + (uh20(n)-1)*3
10      continue
!
        do 20 n = 13, 16
            iu(1,n) = 1 + (uh20(n)-1)*3 - 8
            iu(2,n) = 2 + (uh20(n)-1)*3 - 8
            iu(3,n) = 3 + (uh20(n)-1)*3 - 8
20      continue
!
        ip(1) = 1 + (ph20(1)-1)*3
        ip(2) = 1 + (ph20(2)-1)*3 - 2
        ip(3) = 1 + (ph20(3)-1)*3 - 4
        ip(4) = 1 + (ph20(4)-1)*3 - 6
!
    else if (ifp15) then
!
        do 30 n = 1, 9
            iu(1,n) = 1 + (up15(n)-1)*3
            iu(2,n) = 2 + (up15(n)-1)*3
            iu(3,n) = 3 + (up15(n)-1)*3
30      continue
!
        do 40 n = 10, 12
            iu(1,n) = 1 + (up15(n)-1)*3 - 6
            iu(2,n) = 2 + (up15(n)-1)*3 - 6
            iu(3,n) = 3 + (up15(n)-1)*3 - 6
40      continue
!
        ip(1) = 1 + (pp15(1)-1)*3
        ip(2) = 1 + (pp15(2)-1)*3 - 2
        ip(3) = 1 + (pp15(3)-1)*3 - 4
!
    else if (ifqu8) then
!
        do 50 n = 1, 5
            iu(1,n) = 1 + (uq8(n)-1)*2
            iu(2,n) = 2 + (uq8(n)-1)*2
50      continue
        iu(1,6) = 1 + (uq8(6)-1)*2 - 1
        iu(2,6) = 2 + (uq8(6)-1)*2 - 1
!
        ip(1) = 1 + (pq8(1)-1)*2 - 1
        ip(2) = 1 + (pq8(2)-1)*2
!
    else
!     NOM D'ELEMENT ILLICITE
        ASSERT(ifqu8.or.ifp15.or.ifh20)
    endif
!
end subroutine
