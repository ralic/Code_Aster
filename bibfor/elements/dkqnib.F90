subroutine dkqnib(qsi, eta, caraq4, nfx, nfy)
    implicit  none
    real(kind=8) :: qsi, eta, caraq4(*), nfx(12), nfy(12)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.======================================================================
!  DKQNIB -- DETERMINATION DES FONCTIONS DE FORME DES ROTATIONS
!            SUR LES DEPLACEMENTS DE MEMBRANE ET DE FLEXION
!            POUR LES ELEMENTS DE PLAQUE DKQ :
!            ON A L'INTERPOLATION SUIVANTE
!            BETA_X = NI*BETA_XI + PK*CK*ALPHA_K
!            BETA_Y = NI*BETA_YI + PK*SK*ALPHA_K
!
!            LES NI SONT LES FONCTIONS DE FORME CLASSIQUES DU
!            QUADRANGLE A 4 NOEUDS I.E. :
!              N1 = 1/4*(1-QSI)*(1-ETA)
!              N2 = 1/4*(1+QSI)*(1-ETA)
!              N3 = 1/4*(1+QSI)*(1+ETA)
!              N4 = 1/4*(1-QSI)*(1+ETA)
!
!            LES PK SONT QUADRATIQUES :
!              P5 = 1/2*(1-QSI**2)*(1-ETA)
!              P6 = 1/2*(1-ETA**2)*(1+QSI)
!              P7 = 1/2*(1-QSI**2)*(1+ETA)
!              P8 = 1/2*(1-ETA**2)*(1-QSI)
!
!            LES ALPHA DESIGNENT DES ROTATIONS SITUEES AU MILIEU DES
!            COTES DU TRIANGLE
!            ON RAPPELLE QUE ALPHA = AN*UN
!            LA MATRICE AN A UNE EXPRESSION PLUS SIMPLE QUE POUR LE DSQ
!            ELLE EST OBTENUE EN ECRIVANT QUE LES DEFORMATIONS DE
!            CISAILLEMENT SONT NULLES EN MOYENNE LE LONG DES
!            COTES DE L'ELEMENT
!
!            UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)
!
!   ARGUMENT        E/S  TYPE         ROLE
!    INT            IN    I       INDICE DU POINT D'INTEGRATION
!    R(*)           IN    R       TABLEAU DE CARACTERISTIQUES
!                                 GEOMETRIQUES DE L'ELEMENT :
!                                 COS ET SIN DES ANGLES, LONGUEUR
!                                 DES COTES ,...
!    NFX(12)        OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_X =  NFXI*WI + NFXJ*BETAXJ
!    NFY(12)        OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_Y =  NFYI*WI + NFYJ*BETAYJ
!     ------------------------------------------------------------------
    real(kind=8) :: l5, l6, l7, l8, c5, c6, c7, c8, s5, s6, s7, s8
    real(kind=8) :: n1, n2, n3, n4, p5, p6, p7, p8
    real(kind=8) :: unquar, undemi, un, deux, trois, quatre
!     ------------------------------------------------------------------
    unquar = 0.25d0
    undemi = 0.50d0
    un = 1.0d0
    deux = 2.0d0
    trois = 3.0d0
    quatre = 4.0d0
!
    c5 = caraq4(13)
    c6 = caraq4(14)
    c7 = caraq4(15)
    c8 = caraq4(16)
    s5 = caraq4(17)
    s6 = caraq4(18)
    s7 = caraq4(19)
    s8 = caraq4(20)
    l5 = caraq4( 9)
    l6 = caraq4(10)
    l7 = caraq4(11)
    l8 = caraq4(12)
!
    n1 = unquar*(un-qsi)*(un-eta)
    n2 = unquar*(un+qsi)*(un-eta)
    n3 = unquar*(un+qsi)*(un+eta)
    n4 = unquar*(un-qsi)*(un+eta)
!
    p5 = undemi*(un-qsi*qsi)*(un-eta)
    p6 = undemi*(un-eta*eta)*(un+qsi)
    p7 = undemi*(un-qsi*qsi)*(un+eta)
    p8 = undemi*(un-eta*eta)*(un-qsi)
!
    nfx(1) = trois/deux*(p5*c5/l5 - p8*c8/l8)
    nfx(2) = n1 - trois/quatre*(p5*c5*c5 + p8*c8*c8)
    nfx(3) = -trois/quatre*(p5*c5*s5 + p8*c8*s8)
!
    nfx(4) = trois/deux*(p6*c6/l6 - p5*c5/l5)
    nfx(5) = n2 - trois/quatre*(p6*c6*c6 + p5*c5*c5)
    nfx(6) = -trois/quatre*(p6*c6*s6 + p5*c5*s5)
!
    nfx(7) = trois/deux*(p7*c7/l7 - p6*c6/l6)
    nfx(8) = n3 - trois/quatre*(p7*c7*c7 + p6*c6*c6)
    nfx(9) = -trois/quatre*(p7*c7*s7 + p6*c6*s6)
!
    nfx(10) = trois/deux*(p8*c8/l8 - p7*c7/l7)
    nfx(11) = n4 - trois/quatre*(p8*c8*c8 + p7*c7*c7)
    nfx(12) = -trois/quatre*(p8*c8*s8 + p7*c7*s7)
!
    nfy(1) = trois/deux*(p5*s5/l5 - p8*s8/l8)
    nfy(2) = nfx(3)
    nfy(3) = n1 - trois/quatre*(p5*s5*s5 + p8*s8*s8)
!
    nfy(4) = trois/deux*(p6*s6/l6 - p5*s5/l5)
    nfy(5) = nfx(6)
    nfy(6) = n2 - trois/quatre*(p6*s6*s6 + p5*s5*s5)
!
    nfy(7) = trois/deux*(p7*s7/l7 - p6*s6/l6)
    nfy(8) = nfx(9)
    nfy(9) = n3 - trois/quatre*(p7*s7*s7 + p6*s6*s6)
!
    nfy(10) = trois/deux*(p8*s8/l8 - p7*s7/l7)
    nfy(11) = nfx(12)
    nfy(12) = n4 - trois/quatre*(p8*s8*s8 + p7*s7*s7)
!
end subroutine
