subroutine carand(randd, gr)
!
! ======================================================================
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! ROUTINE CARAND : TIRAGE STATISTIQUE SUR LOI UNIFORME
! AVEC FONCTION VRAIEMENT ALEATOIRE KLOKLO SI GR=0
! GR EST LA GRAINE
!
!
    implicit none
!
#include "asterc/kloklo.h"
    integer :: hvlue, lvlue, testv, nextn, time(9)
    real(kind=8) :: randd, gr
    integer :: ind, mplier, modlus, mobymp, momdmp
!
    common  /seed/nextn
!
    parameter (mplier=16807,modlus=2147483647,mobymp=127773,&
     &           momdmp=2836)
!
    ind = nint(gr)
    if (ind .eq. 0) then
        if (nextn .eq. 0) then
            call kloklo(time)
            nextn = time(5)+time(6)+time(7)
        endif
    else
        if (nextn .eq. 0) then
            nextn = ind
        endif
    endif
!
    hvlue = nextn / mobymp
    lvlue = mod(nextn, mobymp)
    testv = mplier*lvlue - momdmp*hvlue
    if (testv .gt. 0) then
        nextn = testv
    else
        nextn = testv + modlus
    endif
    randd = abs(dble(nextn)/dble(modlus))
!
end subroutine
