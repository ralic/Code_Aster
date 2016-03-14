function cfmmvd(vect)
!
implicit none
!
#include "asterfort/assert.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    integer :: cfmmvd
    character(len=5), intent(in) :: vect
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Utility
!
! Get size of some contact objects
!
! --------------------------------------------------------------------------------------------------
!
! In  vect             : type of contact object
! Out cfmmvd           : size of contact object
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: zmeth = 22, ztole = 3 , ztabf = 34, zcmcf = 13
    integer, parameter :: ztgde = 6 , zdirn = 6 , zdime = 18, zpoud = 3
    integer, parameter :: ztypm = 2 , zperc = 4 , ztypn = 2 , zmesx = 5
    integer, parameter :: zapme = 3 , zmaes = 4 , zresu = 30, zcmdf = 6
    integer, parameter :: zcmxf = 16, zexcl = 3 , zparr = 5 , zpari = 29
    integer, parameter :: ztaco = 2 , zeven = 5 , zcoco = 8 , ztacf = 4 
    integer, parameter :: zetat = 3
!
! --------------------------------------------------------------------------------------------------
!
    if (vect .eq. 'ZMETH') then
        cfmmvd = zmeth
    else if (vect.eq.'ZTOLE') then
        cfmmvd = ztole
    else if (vect.eq.'ZTABF') then
        cfmmvd = ztabf
    else if (vect.eq.'ZTACF') then
        cfmmvd = ztacf
    else if (vect.eq.'ZCMCF') then
        cfmmvd = zcmcf
    else if (vect.eq.'ZCMXF') then
        cfmmvd = zcmxf
    else if (vect.eq.'ZTGDE') then
        cfmmvd = ztgde
    else if (vect.eq.'ZDIRN') then
        cfmmvd = zdirn
    else if (vect.eq.'ZPOUD') then
        cfmmvd = zpoud
    else if (vect.eq.'ZTYPM') then
        cfmmvd = ztypm
    else if (vect.eq.'ZTYPN') then
        cfmmvd = ztypn
    else if (vect.eq.'ZMESX') then
        cfmmvd = zmesx
    else if (vect.eq.'ZAPME') then
        cfmmvd = zapme
    else if (vect.eq.'ZRESU') then
        cfmmvd = zresu
    else if (vect.eq.'ZCMDF') then
        cfmmvd = zcmdf
    else if (vect.eq.'ZPERC') then
        cfmmvd = zperc
    else if (vect.eq.'ZEXCL') then
        cfmmvd = zexcl
    else if (vect.eq.'ZPARR') then
        cfmmvd = zparr
    else if (vect.eq.'ZPARI') then
        cfmmvd = zpari
    else if (vect.eq.'ZCOCO') then
        cfmmvd = zcoco
    else if (vect.eq.'ZDIME') then
        cfmmvd = zdime
    else if (vect.eq.'ZMAES') then
        cfmmvd = zmaes
    else if (vect.eq.'ZETAT') then
        cfmmvd = zetat
    else if (vect.eq.'ZTACO') then
        cfmmvd = ztaco
    else if (vect.eq.'ZEVEN') then
        cfmmvd = zeven
    else
        ASSERT(.false.)
    endif
!
end function
