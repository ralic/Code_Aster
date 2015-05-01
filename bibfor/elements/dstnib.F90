subroutine dstnib(qsi, eta, carat3, an, am,&
                  nfx, nfy, nmx, nmy)
    implicit  none
    real(kind=8) :: qsi, eta, carat3(*), an(3, 9), am(3, 6)
    real(kind=8) :: nfx(9), nfy(9), nmx(6), nmy(6)
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
!
!  DSTNIB -- DETERMINATION DES FONCTIONS DE FORME DES ROTATIONS
!            SUR LES DEPLACEMENTS DE MEMBRANE ET DE FLEXION
!            POUR LES ELEMENTS DE PLAQUE DST :
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
!            ON RAPPELLE QUE POUR UNE PLAQUE NON EXCENTREE
!                ALPHA = AN*UN
!            ET QUE DANS LE CAS EXCENTRE
!                ALPHA = AN*UN + AM*UM
!
!            UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)
!            UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)
!
!   ARGUMENT        E/S  TYPE         ROLE
!    INT            IN    I       INDICE DU POINT D'INTEGRATION
!    R(*)           IN    R       TABLEAU DE CARACTERISTIQUES
!                                 GEOMETRIQUES DE L'ELEMENT :
!                                 COS ET SIN DES ANGLES, LONGUEUR
!                                 DES COTES ,...
!    AN(3,9)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
!                                 AUX INCONNUES DE FLEXION UN
!    AM(3,6)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
!                                 AUX INCONNUES DE MEMBRANE UM
!    NFX(9)         OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_X =  NFXI*WI + NFXJ*BETAXJ
!                                         (+NMX*UM)
!    NFY(9)         OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_Y =  NFYI*WI + NFYJ*BETAYJ
!                                         (+NMY*UM)
!    NMX(6)         OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_X =  NMX*UM
!                                        (+ NFXI*WI + NFXJ*BETAXJ)
!    NMY(6)         OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_Y =  NMY*UM
!                                        (+ NFYI*WI + NFYJ*BETAYJ)
!     ------------------------------------------------------------------
    real(kind=8) :: c4, c5, c6, s4, s5, s6, n1, n2, n3, p4, p5, p6, un, quatre
!     ------------------------------------------------------------------
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    un = 1.0d0
    quatre = 4.0d0
!
    c4 = carat3(16)
    c5 = carat3(17)
    c6 = carat3(18)
    s4 = carat3(19)
    s5 = carat3(20)
    s6 = carat3(21)
!
    n1 = un - qsi - eta
    n2 = qsi
    n3 = eta
!
    p4 = quatre*qsi*(un-qsi-eta)
    p5 = quatre*qsi*eta
    p6 = quatre*eta*(un-qsi-eta)
!
!==============================================================
! --- FONCTIONS D'INTERPOLATIONS DES ROTATIONS POUR LES DDLS  =
! --- DE FLEXION :                                            =
!==============================================================
!
    nfx(1) = p4*c4*an(1,1) + p5*c5*an(2,1) + p6*c6*an(3,1)
    nfx(2) = n1 + p4*c4*an(1,2) + p5*c5*an(2,2) + p6*c6*an(3,2)
    nfx(3) = p4*c4*an(1,3) + p5*c5*an(2,3) + p6*c6*an(3,3)
!
    nfx(4) = p4*c4*an(1,4) + p5*c5*an(2,4) + p6*c6*an(3,4)
    nfx(5) = n2 + p4*c4*an(1,5) + p5*c5*an(2,5) + p6*c6*an(3,5)
    nfx(6) = p4*c4*an(1,6) + p5*c5*an(2,6) + p6*c6*an(3,6)
!
    nfx(7) = p4*c4*an(1,7) + p5*c5*an(2,7) + p6*c6*an(3,7)
    nfx(8) = n3 + p4*c4*an(1,8) + p5*c5*an(2,8) + p6*c6*an(3,8)
    nfx(9) = p4*c4*an(1,9) + p5*c5*an(2,9) + p6*c6*an(3,9)
!
    nfy(1) = p4*s4*an(1,1) + p5*s5*an(2,1) + p6*s6*an(3,1)
    nfy(2) = p4*s4*an(1,2) + p5*s5*an(2,2) + p6*s6*an(3,2)
    nfy(3) = n1 + p4*s4*an(1,3) + p5*s5*an(2,3) + p6*s6*an(3,3)
!
    nfy(4) = p4*s4*an(1,4) + p5*s5*an(2,4) + p6*s6*an(3,4)
    nfy(5) = p4*s4*an(1,5) + p5*s5*an(2,5) + p6*s6*an(3,5)
    nfy(6) = n2 + p4*s4*an(1,6) + p5*s5*an(2,6) + p6*s6*an(3,6)
!
    nfy(7) = p4*s4*an(1,7) + p5*s5*an(2,7) + p6*s6*an(3,7)
    nfy(8) = p4*s4*an(1,8) + p5*s5*an(2,8) + p6*s6*an(3,8)
    nfy(9) = n3 + p4*s4*an(1,9) + p5*s5*an(2,9) + p6*s6*an(3,9)
!
!==============================================================
! --- FONCTIONS D'INTERPOLATIONS DES ROTATIONS POUR LES DDLS  =
! --- DE MEMBRANE :                                           =
!==============================================================
!
    nmx(1) = p4*c4*am(1,1) + p5*c5*am(2,1) + p6*c6*am(3,1)
    nmx(2) = p4*c4*am(1,2) + p5*c5*am(2,2) + p6*c6*am(3,2)
    nmx(3) = p4*c4*am(1,3) + p5*c5*am(2,3) + p6*c6*am(3,3)
    nmx(4) = p4*c4*am(1,4) + p5*c5*am(2,4) + p6*c6*am(3,4)
    nmx(5) = p4*c4*am(1,5) + p5*c5*am(2,5) + p6*c6*am(3,5)
    nmx(6) = p4*c4*am(1,6) + p5*c5*am(2,6) + p6*c6*am(3,6)
!
    nmy(1) = p4*s4*am(1,1) + p5*s5*am(2,1) + p6*s6*am(3,1)
    nmy(2) = p4*s4*am(1,2) + p5*s5*am(2,2) + p6*s6*am(3,2)
    nmy(3) = p4*s4*am(1,3) + p5*s5*am(2,3) + p6*s6*am(3,3)
    nmy(4) = p4*s4*am(1,4) + p5*s5*am(2,4) + p6*s6*am(3,4)
    nmy(5) = p4*s4*am(1,5) + p5*s5*am(2,5) + p6*s6*am(3,5)
    nmy(6) = p4*s4*am(1,6) + p5*s5*am(2,6) + p6*s6*am(3,6)
!
end subroutine
