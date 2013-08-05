subroutine cfreli(noma, nummai, nbnom, ksi1, ksi2,&
                  coefno)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit     none
#include "asterfort/assert.h"
#include "asterfort/elrfvf.h"
#include "asterfort/mmelty.h"
    character(len=8) :: noma
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: coefno(9)
    integer :: nummai, nbnom
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! COEFFICIENTS DE LA RELATION SUR NOEUDS MAITRES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : MAILLAGE
! IN  NBNOM  : NOIMBRE NOEUDS MAILLE MAITRE PORTANT DES DX/DY/DZ
! IN  NUMMAI : NUMERO ABSOLU DE LA MAILLE
! IN  KSIx   : COORDONNEES PARAMETRIQUES SUR LA MAILLE MAITRE
!                 DE LA "PROJECTION" M
! OUT COEFNO : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
!               MAITRES
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: zero, un
    parameter  ( zero   =  0.0d0  )
    parameter  ( un     =  1.0d0  )
    real(kind=8) :: ksi(3)
    integer :: k, ibid
    real(kind=8) :: ff(9)
    character(len=8) :: alias
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    do 10 k = 1, 9
        coefno(k) = zero
10  end do
!
! --- CARACTERISTIQUE DE LA MAILLE
!
    call mmelty(noma, nummai, alias, ibid, ibid)
!
! --- COEFFICIENTS SUR NOEUD MAITRE SUIVANT TYPE APPARIEMENT/ELEMENT
!
    ksi(1) = ksi1
    ksi(2) = ksi2
    ksi(3) = un - ksi1 - ksi2
!
    if (alias .eq. 'SE2') then
        call elrfvf('SE2', ksi, 2, ff, ibid)
        coefno(1) = - ff(1)
        coefno(2) = - ff(2)
        coefno(3) = zero
    else if (alias.eq.'SE3') then
        call elrfvf('SE3', ksi, 3, ff, ibid)
        coefno(1) = - ff(1)
        coefno(2) = - ff(2)
        coefno(3) = - ff(3)
    else if (alias(1:4).eq.'TR3') then
        call elrfvf('TR3', ksi, 3, ff, ibid)
        coefno(1) = - ff(1)
        coefno(2) = - ff(2)
        coefno(3) = - ff(3)
    else if (alias(1:4).eq.'TR6') then
        call elrfvf('TR6', ksi, 6, ff, ibid)
        coefno(1) = - ff(1)
        coefno(2) = - ff(2)
        coefno(3) = - ff(3)
        coefno(4) = - ff(4)
        coefno(5) = - ff(5)
        coefno(6) = - ff(6)
    else if (alias(1:4).eq.'TR7') then
        if (nbnom .eq. 7) then
            call elrfvf('TR7', ksi, 7, ff, ibid)
            coefno(1) = - ff(1)
            coefno(2) = - ff(2)
            coefno(3) = - ff(3)
            coefno(4) = - ff(4)
            coefno(5) = - ff(5)
            coefno(6) = - ff(6)
            coefno(7) = - ff(7)
        else if (nbnom.eq.6) then
            call elrfvf('TR6', ksi, 6, ff, ibid)
            coefno(1) = - ff(1)
            coefno(2) = - ff(2)
            coefno(3) = - ff(3)
            coefno(4) = - ff(4)
            coefno(5) = - ff(5)
            coefno(6) = - ff(6)
        else
            ASSERT(.false.)
        endif
    else if (alias(1:2).eq.'QU') then
        if (alias .eq. 'QU4') then
            call elrfvf('QU4', ksi, 4, ff, ibid)
            coefno(1) = - ff(1)
            coefno(2) = - ff(2)
            coefno(3) = - ff(3)
            coefno(4) = - ff(4)
        else if (alias.eq.'QU8') then
            call elrfvf('QU4', ksi, 4, ff, ibid)
            coefno(1) = - ff(1)
            coefno(2) = - ff(2)
            coefno(3) = - ff(3)
            coefno(4) = - ff(4)
        else if (alias.eq.'QU9') then
            if (nbnom .eq. 9) then
                call elrfvf('QU9', ksi, 9, ff, ibid)
                coefno(1) = - ff(1)
                coefno(2) = - ff(2)
                coefno(3) = - ff(3)
                coefno(4) = - ff(4)
                coefno(5) = - ff(5)
                coefno(6) = - ff(6)
                coefno(7) = - ff(7)
                coefno(8) = - ff(8)
                coefno(9) = - ff(9)
            else if (nbnom.eq.8) then
                call elrfvf('QU8', ksi, 8, ff, ibid)
                coefno(1) = - ff(1)
                coefno(2) = - ff(2)
                coefno(3) = - ff(3)
                coefno(4) = - ff(4)
                coefno(5) = - ff(5)
                coefno(6) = - ff(6)
                coefno(7) = - ff(7)
                coefno(8) = - ff(8)
            else
                ASSERT(.false.)
            endif
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
