subroutine pmfbts(b, wi, vs, ve)
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
!    -------------------------------------------------------------------
!     CALCUL DE : WI BT VS
!     VS = VECTEUR DE FORCES INTERNES (ISSUE DE L'INTE. SUR LES FIBRES)
!     WI = POIDS DU PT DE GAUSS EN COURS
!     B = MATRICE B A CE POINT DE GAUSS ET BT SA TRANSPOSEE
!
!    -------------------------------------------------------------------
!
! IN R*8  VS(1) = INT(SIG DS)   = N0 EFFORT NORMAL INTERNE
! IN R*8  VS(2) = INT(SIG.Y DS) = -MZ0 MOMENT FLECHISSANT Z INTERNE
! IN R*8  VS(3) = INT(SIG.Z DS) = MY0 MOMENT FLECHISSANT Y INTERNE
! IN R*8  B(4) : LES QUATRES VALEURS NON NULLE ET DIFFERENTES DE B
!
! OUT R*8 VE(12) : FORCES ELEMENTAIRES DUES A CE POINT DE GAUSS
!
    real(kind=8) :: vs(3), b(4), wi, ve(12)
!
    ve(1)=-b(1)*vs(1)*wi
    ve(2)=-b(2)*vs(2)*wi
    ve(3)=-b(2)*vs(3)*wi
    ve(4)=0.d0
!
    ve(5)=b(3)*vs(3)*wi
    ve(6)=-b(3)*vs(2)*wi
    ve(7)=-ve(1)
    ve(8)=-ve(2)
    ve(9)=-ve(3)
    ve(10)=0.d0
!
    ve(11)=b(4)*vs(3)*wi
    ve(12)=-b(4)*vs(2)*wi
!
end subroutine
