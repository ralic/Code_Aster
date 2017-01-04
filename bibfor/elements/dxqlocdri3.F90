subroutine dxqlocdri3(gmefl, matloc)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    real(kind=8) :: gmefl(*)
    real(kind=8) :: matloc(*)
!----------------------------------------------------------
!     IN  GMEMB  : MATRICE DE MEMBRANE (DRILLING) CARREE
!     OUT MATLOC : MATRICE DE RIGIDITE OU DE MASSE LOCALE
!                  REMPLISSAGE DE MATELEM LOCAL (300 TERMES) AVEC
!                  LES TERMES DE MEMBRANE DRILLING DRZ
!----------------------------------------------------------
!
    integer :: imf(48), jmf(48)
    real(kind=8) ::  cmf(48)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: k
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------


      imf(1) = 1
      imf(2) = 9
      imf(3) = 5
      imf(4) = 13
      imf(5) = 21
      imf(6) = 17
      imf(7) = 2
      imf(8) = 10
      imf(9) = 6
      imf(10) = 14
      imf(11) = 22
      imf(12) = 18
      imf(13) = 25
      imf(14) = 26
      imf(15) = 33
      imf(16) = 34
      imf(17) = 29
      imf(18) = 30
      imf(19) = 3
      imf(20) = 11
      imf(21) = 7
      imf(22) = 15
      imf(23) = 23
      imf(24) = 19
      imf(25) = 27
      imf(26) = 35
      imf(27) = 31
      imf(28) = 37
      imf(29) = 38
      imf(30) = 39
      imf(31) = 45
      imf(32) = 46
      imf(33) = 47
      imf(34) = 41
      imf(35) = 42
      imf(36) = 43
      imf(37) = 4
      imf(38) = 12
      imf(39) = 8
      imf(40) = 16
      imf(41) = 24
      imf(42) = 20
      imf(43) = 28
      imf(44) = 36
      imf(45) = 32
      imf(46) = 40
      imf(47) = 48
      imf(48) = 44

      cmf(1) = 1.d0
      cmf(2) = -1.d0
      cmf(3) = 1.d0
      cmf(4) = 1.d0
      cmf(5) = -1.d0
      cmf(6) = 1.d0
      cmf(7) = 1.d0
      cmf(8) = -1.d0
      cmf(9) = 1.d0
      cmf(10) = 1.d0
      cmf(11) = -1.d0
      cmf(12) = 1.d0
      cmf(13) = 1.d0
      cmf(14) = 1.d0
      cmf(15) = -1.d0
      cmf(16) = -1.d0
      cmf(17) = 1.d0
      cmf(18) = 1.d0
      cmf(19) = 1.d0
      cmf(20) = -1.d0
      cmf(21) = 1.d0
      cmf(22) = 1.d0
      cmf(23) = -1.d0
      cmf(24) = 1.d0
      cmf(25) = 1.d0
      cmf(26) = -1.d0
      cmf(27) = 1.d0
      cmf(28) = 1.d0
      cmf(29) = 1.d0
      cmf(30) = 1.d0
      cmf(31) = -1.d0
      cmf(32) = -1.d0
      cmf(33) = -1.d0
      cmf(34) = 1.d0
      cmf(35) = 1.d0
      cmf(36) = 1.d0
      cmf(37) = 1.d0
      cmf(38) = -1.d0
      cmf(39) = 1.d0
      cmf(40) = 1.d0
      cmf(41) = -1.d0
      cmf(42) = 1.d0
      cmf(43) = 1.d0
      cmf(44) = -1.d0
      cmf(45) = 1.d0
      cmf(46) = 1.d0
      cmf(47) = -1.d0
      cmf(48) = 1.d0

      jmf(1) = 18
      jmf(2) = 19
      jmf(3) = 20
      jmf(4) = 42
      jmf(5) = 51
      jmf(6) = 61
      jmf(7) = 69
      jmf(8) = 70
      jmf(9) = 71
      jmf(10) = 75
      jmf(11) = 76
      jmf(12) = 77
      jmf(13) = 111
      jmf(14) = 117
      jmf(15) = 126
      jmf(16) = 132
      jmf(17) = 142
      jmf(18) = 148
      jmf(19) = 156
      jmf(20) = 157
      jmf(21) = 158
      jmf(22) = 162
      jmf(23) = 163
      jmf(24) = 164
      jmf(25) = 168
      jmf(26) = 169
      jmf(27) = 170
      jmf(28) = 216
      jmf(29) = 222
      jmf(30) = 228
      jmf(31) = 237
      jmf(32) = 243
      jmf(33) = 249
      jmf(34) = 259
      jmf(35) = 265
      jmf(36) = 271
      jmf(37) = 279
      jmf(38) = 280
      jmf(39) = 281
      jmf(40) = 285
      jmf(41) = 286
      jmf(42) = 287
      jmf(43) = 291
      jmf(44) = 292
      jmf(45) = 293
      jmf(46) = 297
      jmf(47) = 298
      jmf(48) = 299

!     ------------------------------------------------------------------


!                       ---- TERMES DE COUPLAGE MEMBRANE (DRILL) - FLEXION
    do k = 1, 48
        matloc(jmf(k)) = matloc(jmf(k)) + cmf(k)*gmefl(imf(k))
    end do


end subroutine
