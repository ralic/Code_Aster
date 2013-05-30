subroutine derpro(a, da, b, db, dab)
!
    implicit      none
    real(kind=8) :: a, da, b, db, dab
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DE DAB = A*DB+B*DA ----------------------------------
! ======================================================================
! IN  : A      : VALEUR DE LA FONCTION A -------------------------------
! --- : DA     : VALEUR DE LA DERIVEE DE A -----------------------------
! --- : B      : VALEUR DE LA FONCTION B -------------------------------
! --- : DB     : VALEUR DE LA DERIVEE DE B -----------------------------
! OUT : DAB    : VALEUR DE LA DERIVEE DE A*B ---------------------------
! ======================================================================
    dab = a*db + b*da
! ======================================================================
end subroutine
