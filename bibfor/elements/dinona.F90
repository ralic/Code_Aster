subroutine dinona(nomte, raide, klv)
! ----------------------------------------------------------------------
    implicit none
    character(len=16) :: nomte
    real(kind=8) :: klv(*), raide(*)
! ----------------------------------------------------------------------
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
!
! ======================================================================
!           ACTUALISATION DE LA MATRICE QUASI-TANGENTE
!
!  LES TERMES DIAGONAUX SONT LES SEULS A ETRE ADAPTES
!
! ======================================================================
!
!  IN
!     NOMTE : NOM DE L'ELEMENT
!     RAIDE : RAIDEUR QUASI-TANGENTE
!  OUT
!     KLV   : MATRICE DE RAIDEUR
!
! ======================================================================
!
    if (nomte .eq. 'MECA_DIS_TR_L') then
        klv(1) = raide(1)
        klv(3) = raide(2)
        klv(6) = raide(3)
        klv(10) = raide(4)
        klv(15) = raide(5)
        klv(21) = raide(6)
        klv(28) = raide(1)
        klv(36) = raide(2)
        klv(45) = raide(3)
        klv(55) = raide(4)
        klv(66) = raide(5)
        klv(78) = raide(6)
        klv(22) = -raide(1)
        klv(30) = -raide(2)
        klv(39) = -raide(3)
        klv(49) = -raide(4)
        klv(60) = -raide(5)
        klv(72) = -raide(6)
    else if (nomte.eq.'MECA_DIS_TR_N') then
        klv(1) = raide(1)
        klv(3) = raide(2)
        klv(6) = raide(3)
        klv(10) = raide(4)
        klv(15) = raide(5)
        klv(21) = raide(6)
    else if (nomte.eq.'MECA_DIS_T_L') then
        klv(1) = raide(1)
        klv(3) = raide(2)
        klv(6) = raide(3)
        klv(10) = raide(1)
        klv(15) = raide(2)
        klv(21) = raide(3)
        klv(7) = -raide(1)
        klv(12) = -raide(2)
        klv(18) = -raide(3)
    else if (nomte.eq.'MECA_DIS_T_N') then
        klv(1) = raide(1)
        klv(3) = raide(2)
        klv(6) = raide(3)
    else if (nomte.eq.'MECA_2D_DIS_T_L') then
        klv(1) = raide(1)
        klv(3) = raide(2)
        klv(6) = raide(1)
        klv(10) = raide(2)
        klv(4) = -raide(1)
        klv(8) = -raide(2)
    else if (nomte.eq.'MECA_2D_DIS_T_N') then
        klv(1) = raide(1)
        klv(3) = raide(2)
    else if (nomte.eq.'MECA_2D_DIS_TR_L') then
        klv(1) = raide(1)
        klv(3) = raide(2)
        klv(6) = raide(3)
        klv(10) = raide(1)
        klv(15) = raide(2)
        klv(21) = raide(3)
        klv(7) = -raide(1)
        klv(12) = -raide(2)
        klv(18) = -raide(3)
    else if (nomte.eq.'MECA_2D_DIS_TR_N') then
        klv(1) = raide(1)
        klv(3) = raide(2)
        klv(6) = raide(3)
    endif
!
end subroutine
