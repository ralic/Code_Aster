subroutine dktnib(qsi, eta, carat3, nfx, nfy)
    implicit  none
    real(kind=8) :: qsi, eta, carat3(*), nfx(9), nfy(9)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  DKTNIB -- DETERMINATION DES FONCTIONS DE FORME DES ROTATIONS
!            SUR LES DEPLACEMENTS DE MEMBRANE ET DE FLEXION
!            POUR LES ELEMENTS DE PLAQUE DKT :
!            ON A L'INTERPOLATION SUIVANTE
!            BETA_X = NI*BETA_XI + PK*CK*ALPHA_K
!            BETA_Y = NI*BETA_YI + PK*SK*ALPHA_K
!            LES NI SONT LES FONCTIONS DE FORME CLASSIQUES DU TRIANGLE
!            A 3 NOEUDS I.E. :
!              N1 = 1 - QSI - ETA
!              N2 =     QSI
!              N3 =           ETA
!
!            LES PK SONT QUADRATIQUES :
!              P4 = 4*QSI*(1-QSI-ETA)
!              P5 = 4*QSI*ETA
!              P6 = 4*ETA*(1-QSI-ETA)
!
!            LES ALPHA DESIGNENT DES ROTATIONS SITUEES AU MILIEU DES
!            COTES DU TRIANGLE
!            ON RAPPELLE QUE ALPHA = AN*UN
!            LA MATRICE AN A UNE EXPRESSION PLUS SIMPLE QUE POUR LE DST
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
!    NFX(9)         OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_X =  NFXI*WI + NFXJ*BETAXJ
!    NFY(9)         OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_Y =  NFYI*WI + NFYJ*BETAYJ
!     ------------------------------------------------------------------
    real(kind=8) :: l4, l5, l6, c4, c5, c6, s4, s5, s6, n1, n2, n3, p4, p5, p6
    real(kind=8) :: un, deux, trois, quatre
!     ------------------------------------------------------------------
!
    un = 1.0d0
    deux = 2.0d0
    trois = 3.0d0
    quatre = 4.0d0
!
    c4 = carat3(16)
    c5 = carat3(17)
    c6 = carat3(18)
    s4 = carat3(19)
    s5 = carat3(20)
    s6 = carat3(21)
    l4 = carat3(13)
    l5 = carat3(14)
    l6 = carat3(15)
!
    n1 = un - qsi - eta
    n2 = qsi
    n3 = eta
!
    p4 = quatre*qsi*(un-qsi-eta)
    p5 = quatre*qsi*eta
    p6 = quatre*eta*(un-qsi-eta)
!
    nfx(1) = trois/deux*(p4*c4/l4 - p6*c6/l6)
    nfx(2) = n1 - trois/quatre*(p4*c4*c4 + p6*c6*c6)
    nfx(3) = -trois/quatre*(p4*c4*s4 + p6*c6*s6)
!
    nfx(4) = trois/deux*(p5*c5/l5 - p4*c4/l4)
    nfx(5) = n2 - trois/quatre*(p5*c5*c5 + p4*c4*c4)
    nfx(6) = -trois/quatre*(p5*c5*s5 + p4*c4*s4)
!
    nfx(7) = trois/deux*(p6*c6/l6 - p5*c5/l5)
    nfx(8) = n3 - trois/quatre*(p6*c6*c6 + p5*c5*c5)
    nfx(9) = -trois/quatre*(p6*c6*s6 + p5*c5*s5)
!
    nfy(1) = trois/deux*(p4*s4/l4 - p6*s6/l6)
    nfy(2) = nfx(3)
    nfy(3) = n1 - trois/quatre*(p4*s4*s4 + p6*s6*s6)
!
    nfy(4) = trois/deux*(p5*s5/l5 - p4*s4/l4)
    nfy(5) = nfx(6)
    nfy(6) = n2 - trois/quatre*(p5*s5*s5 + p4*s4*s4)
!
    nfy(7) = trois/deux*(p6*s6/l6 - p5*s5/l5)
    nfy(8) = nfx(9)
    nfy(9) = n3 - trois/quatre*(p6*s6*s6 + p5*s5*s5)
!
end subroutine
