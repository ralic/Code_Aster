subroutine utlcal(typque, algo, valr)
    implicit      none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
!
#include "asterfort/assert.h"
    character(len=8) :: typque
    character(len=16) :: algo
    real(kind=8) :: valr
!
! ----------------------------------------------------------------------
!
!
! ROUTINE UTILITAIRE POUR LES LOIS DE COMPORTEMENTS
!   FAIT LE LIEN ENTRE LE NOM DE L'ALGORIHTME D'INTEGRATION LOCALE
!   ET SON N° (VALEUR REELLE)
!
! ----------------------------------------------------------------------
!
! IN  TYPQUE    : SI 'NOM_VALE' : TRADUCTION NOM -> VALEUR REELE
!                 SI 'VALE_NOM' : TRADUCTION VALEUR REELE -> NOM
! IN/OUT VALR   : VALEUR REELE
! IN/OUT VALR   : NOM DE L'ALGORITHME
!
!
    ASSERT(typque.eq.'NOM_VALE'.or. typque.eq.'VALE_NOM')
!
    if (typque .eq. 'NOM_VALE') then
!
!
        if (algo .eq. 'ANALYTIQUE') then
            valr = 0.d0
        else if (algo.eq.'SECANTE') then
            valr = 1.d0
        else if (algo.eq.'DEKKER') then
            valr = 2.d0
        else if (algo.eq.'NEWTON_1D') then
            valr = 3.d0
        else if (algo.eq.'NEWTON') then
            valr = 4.d0
        else if (algo.eq.'NEWTON_RELI') then
            valr = 5.d0
        else if (algo.eq.'RUNGE_KUTTA') then
            valr = 6.d0
        else if (algo.eq.'SPECIFIQUE') then
            valr = 7.d0
        else if (algo.eq.'SANS_OBJET') then
            valr = 8.d0
        else if (algo.eq.'BRENT') then
            valr = 9.d0
        else if (algo.eq.'NEWTON_PERT') then
            valr =10.d0
        else
            ASSERT(.false.)
        endif
!
!
    else if (typque.eq.'VALE_NOM') then
!
!
        if (valr .eq. 0.d0) then
            algo = 'ANALYTIQUE'
        else if (valr.eq.1.d0) then
            algo = 'SECANTE'
        else if (valr.eq.2.d0) then
            algo = 'DEKKER'
        else if (valr.eq.3.d0) then
            algo = 'NEWTON_1D'
        else if (valr.eq.4.d0) then
            algo = 'NEWTON'
        else if (valr.eq.5.d0) then
            algo = 'NEWTON_RELI'
        else if (valr.eq.6.d0) then
            algo = 'RUNGE_KUTTA'
        else if (valr.eq.7.d0) then
            algo = 'SPECIFIQUE'
        else if (valr.eq.8.d0) then
            algo = 'SANS_OBJET'
        else if (valr.eq.9.d0) then
            algo = 'BRENT'
        else if (valr.eq.10.d0) then
            algo = 'NEWTON_PERT'
        else
            ASSERT(.false.)
        endif
!
    endif
!
end subroutine
