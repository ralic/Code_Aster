subroutine i3tstf(k, f, desc, desctm, conexk,&
                  coordo, gauche, epsi)
    implicit  none
    include 'jeveux.h'
    integer :: k, f, desc(*), desctm(*), conexk(*)
    real(kind=8) :: coordo(*), epsi
    logical :: gauche
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     TEST DE GAUCHITUDE POUR QUADRANGLE
!     ------------------------------------------------------------------
! IN  K      : I : -
! IN  DESC   : I :  !--> OBJ MAILLE POINTEE (ET CE QU' ELLE POINTE)
! IN  DESCTM : I : -
! IN  F      : I : NUMERO LOCALE DE LA FACE TRAITEE
! IN  CONEXK : I : CONNECTIVITE DE LA MAILLE POINTEE
! IN  COORDO : R : TABLE GLOBALE DES COORDONEES
! OUT GAUCHE : R : REPONSE
!     ------------------------------------------------------------------
!
!
    integer :: ds1, sm(4), i, j, decf, adescm
    real(kind=8) :: tole
    real(kind=8) :: e1(3), e2(3), e3(3), cs(3, 4), lcara, c, t, s, r, zero
!
!======================================================================
!
! --- RECUPERATION DES NOEUDS SOMMET DE LA FACE ET DE SES COORDONNEES
!
    decf = 8 + f
    adescm = desctm(desc(k))
    do 10, i = 1, 4, 1
    ds1 = conexk(zi(adescm-1 + decf + (i-1)*6))
    sm(i) = conexk(zi(adescm-1 + decf + (i-1)*6))
    do 11, j = 1, 3, 1
    cs(j,i) = coordo(3*(ds1-1) + j)
11  continue
    10 end do
!
! --- DEFINITION DU REPERE LOCAL DE LA FACE
!     E1 : DEFINIT PAR L'ARETE N1 N2
!     E2 : DEFINIT PAR L'ARETE N1 N4
!     E3 : PERPENDICULAIRE A E1 E2
!
    zero = 0.0d0
    c = zero
    t = zero
    do 15, i = 1, 3, 1
    s = cs(i,2) - cs(i,1)
    r = cs(i,4) - cs(i,1)
    e1(i) = s
    e2(i) = r
    c = c + s*s
    t = t + r*r
    15 end do
    c = sqrt(c)
    t = sqrt(t)
    do 16, i = 1, 3, 1
    e1(i) = e1(i)/c
    e2(i) = e2(i)/t
    16 end do
    e3(1) = e1(2)*e2(3) - e1(3)*e2(2)
    e3(2) = e1(3)*e2(1) - e1(1)*e2(3)
    e3(3) = e1(1)*e2(2) - e1(2)*e2(1)
    c = zero
    do 17, i = 1, 3, 1
    c = c + e3(i)*e3(i)
    17 end do
    e3(1) = e3(1)/c
    e3(2) = e3(2)/c
    e3(3) = e3(3)/c
!
! --- LCARA : LONGUEUR DE LA PLUS PETITE ARETE
!     AFIN DE DEFINIR UNE PRECISION RELATIVE
!
    lcara = 1.d+50
    do 20, j = 1, 3, 1
    c = zero
    do 22, i = 1, 3, 1
    s = coordo(3*(sm(j+1)-1)+i)-coordo(3*(sm(j)-1)+i)
    c = c + s*s
22  continue
    c = sqrt( c )
    lcara = min ( c, lcara )
    20 end do
    c = zero
    do 24, i = 1, 3, 1
    s = coordo(3*(sm(4)-1)+i)-coordo(3*(sm(1)-1)+i)
    c = c + s*s
    24 end do
    c = sqrt( c )
    lcara = min ( c, lcara )
!
! --- FACE GAUCHE A UNE TOLERANCE PRES
!
    t = zero
    do 80, i = 1, 3, 1
    t = t + ( (cs(i,3)-cs(i,1)) * e3(i) )
    80 end do
    tole = lcara * epsi
    gauche = ( abs(t) .gt. tole )
!
end subroutine
