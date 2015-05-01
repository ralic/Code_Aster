subroutine masstg(matin, matout)
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
    implicit none
! -----------------------------------------------------------
! ---  BUT : MISE SOUS LA FORME DES MATRICES DE MASSE DES
!            POU_D_TG(M) DE LA MATRICE ISSUE DE PTMA01
! -----------------------------------------------------------
    real(kind=8), intent(in) :: matin(78)
    real(kind=8), intent(out) :: matout(105)
!
    matout(1:21)   = matin(1:21)
    matout(22:28)  = 0.d0
    matout(29:34)  = matin(22:27)
    matout(35)     = 0.d0
    matout(36:42)  = matin(28:34)
    matout(43)     = 0.d0
    matout(44:51)  = matin(35:42)
    matout(52)     = 0.d0
    matout(53:61)  = matin(43:51)
    matout(62)     = 0.d0
    matout(63:72)  = matin(52:61)
    matout(73)     = 0.d0
    matout(74:84)  = matin(62:72)
    matout(85)     = 0.d0
    matout(86:91)  = matin(73:78)
    matout(92:105) = 0.d0
!
end subroutine
