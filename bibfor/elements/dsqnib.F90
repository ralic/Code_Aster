subroutine dsqnib(qsi, eta, caraq4, an, am,&
                  nfx, nfy, nmx, nmy)
    implicit  none
    real(kind=8) :: qsi, eta, caraq4(*)
    real(kind=8) :: an(4, 12), am(4, 8), nfx(12), nfy(12), nmx(8), nmy(8)
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
!  DSQNIB -- DETERMINATION DES FONCTIONS DE FORME DES ROTATIONS
!            SUR LES DEPLACEMENTS DE MEMBRANE ET DE FLEXION
!            POUR LES ELEMENTS DE PLAQUE DSQ :
!            ON A L'INTERPOLATION SUIVANTE
!            BETA_X = NI*BETA_XI + PK*CK*ALPHA_K
!            BETA_Y = NI*BETA_YI + PK*SK*ALPHA_K
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
!            COTES DU QUADRANGLE
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
!    AN(4,12)       IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
!                                 AUX INCONNUES DE FLEXION UN
!    AM(4,8)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
!                                 AUX INCONNUES DE MEMBRANE UM
!    NFX(12)        OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_X =  NFXI*WI + NFXJ*BETAXJ
!                                         (+NMX*UM)
!    NFY(12)        OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_Y =  NFYI*WI + NFYJ*BETAYJ
!                                         (+NMY*UM)
!    NMX(8)         OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_X =  NMX*UM
!                                        (+ NFXI*WI + NFXJ*BETAXJ)
!    NMY(8)         OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 BETA_Y =  NMY*UM
!                                        (+ NFYI*WI + NFYJ*BETAYJ)
!     ------------------------------------------------------------------
    real(kind=8) :: c5, c6, c7, c8, s5, s6, s7, s8
    real(kind=8) :: n1, n2, n3, n4, p5, p6, p7, p8
    real(kind=8) :: unquar, undemi, un
!     ------------------------------------------------------------------
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    unquar = 0.25d0
    undemi = 0.5d0
    un = 1.0d0
!
    c5 = caraq4(13)
    c6 = caraq4(14)
    c7 = caraq4(15)
    c8 = caraq4(16)
    s5 = caraq4(17)
    s6 = caraq4(18)
    s7 = caraq4(19)
    s8 = caraq4(20)
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
!==============================================================
! --- FONCTIONS D'INTERPOLATIONS DES ROTATIONS POUR LES DDLS  =
! --- DE FLEXION :                                            =
!==============================================================
!
    nfx(1) = p5*c5*an(1,1) + p6*c6*an(2,1) + p7*c7*an(3,1) + p8*c8*an(4,1)
    nfx(2) = n1 + p5*c5*an(1,2) + p6*c6*an(2,2) + p7*c7*an(3,2) + p8*c8*an(4,2)
    nfx(3) = p5*c5*an(1,3) + p6*c6*an(2,3) + p7*c7*an(3,3) + p8*c8*an(4,3)
!
    nfx(4) = p5*c5*an(1,4) + p6*c6*an(2,4) + p7*c7*an(3,4) + p8*c8*an(4,4)
    nfx(5) = n2 + p5*c5*an(1,5) + p6*c6*an(2,5) + p7*c7*an(3,5) + p8*c8*an(4,5)
    nfx(6) = p5*c5*an(1,6) + p6*c6*an(2,6) + p7*c7*an(3,6) + p8*c8*an(4,6)
!
    nfx(7) = p5*c5*an(1,7) + p6*c6*an(2,7) + p7*c7*an(3,7) + p8*c8*an(4,7)
    nfx(8) = n3 + p5*c5*an(1,8) + p6*c6*an(2,8) + p7*c7*an(3,8) + p8*c8*an(4,8)
    nfx(9) = p5*c5*an(1,9) + p6*c6*an(2,9) + p7*c7*an(3,9) + p8*c8*an(4,9)
!
    nfx(10) = p5*c5*an(1,10) + p6*c6*an(2,10) + p7*c7*an(3,10) + p8*c8*an(4,10)
    nfx(11) = n4 + p5*c5*an(1,11) + p6*c6*an(2,11) + p7*c7*an(3,11) + p8*c8*an(4,11)
    nfx(12) = p5*c5*an(1,12) + p6*c6*an(2,12) + p7*c7*an(3,12) + p8*c8*an(4,12)
!
    nfy(1) = p5*s5*an(1,1) + p6*s6*an(2,1) + p7*s7*an(3,1) + p8*s8*an(4,1)
    nfy(2) = p5*s5*an(1,2) + p6*s6*an(2,2) + p7*s7*an(3,2) + p8*s8*an(4,2)
    nfy(3) = n1 + p5*s5*an(1,3) + p6*s6*an(2,3) + p7*s7*an(3,3) + p8*s8*an(4,3)
!
    nfy(4) = p5*s5*an(1,4) + p6*s6*an(2,4) + p7*s7*an(3,4) + p8*s8*an(4,4)
    nfy(5) = p5*s5*an(1,5) + p6*s6*an(2,5) + p7*s7*an(3,5) + p8*s8*an(4,5)
    nfy(6) = n2 + p5*s5*an(1,6) + p6*s6*an(2,6) + p7*s7*an(3,6) + p8*s8*an(4,6)
!
    nfy(7) = p5*s5*an(1,7) + p6*s6*an(2,7) + p7*s7*an(3,7) + p8*s8*an(4,7)
    nfy(8) = p5*s5*an(1,8) + p6*s6*an(2,8) + p7*s7*an(3,8) + p8*s8*an(4,8)
    nfy(9) = n3 + p5*s5*an(1,9) + p6*s6*an(2,9) + p7*s7*an(3,9) + p8*s8*an(4,9)
!
    nfy(10) = p5*s5*an(1,10) + p6*s6*an(2,10) + p7*s7*an(3,10) + p8*s8*an(4,10)
    nfy(11) = p5*s5*an(1,11) + p6*s6*an(2,11) + p7*s7*an(3,11) + p8*s8*an(4,11)
    nfy(12) = n4 + p5*s5*an(1,12) + p6*s6*an(2,12) + p7*s7*an(3,12) + p8*s8*an(4,12)
!
!==============================================================
! --- FONCTIONS D'INTERPOLATIONS DES ROTATIONS POUR LES DDLS  =
! --- DE MEMBRANE :                                           =
!==============================================================
!
    nmx(1) = p5*c5*am(1,1) + p6*c6*am(2,1) + p7*c7*am(3,1) + p8*c8*am(4,1)
    nmx(2) = p5*c5*am(1,2) + p6*c6*am(2,2) + p7*c7*am(3,2) + p8*c8*am(4,2)
    nmx(3) = p5*c5*am(1,3) + p6*c6*am(2,3) + p7*c7*am(3,3) + p8*c8*am(4,3)
    nmx(4) = p5*c5*am(1,4) + p6*c6*am(2,4) + p7*c7*am(3,4) + p8*c8*am(4,4)
    nmx(5) = p5*c5*am(1,5) + p6*c6*am(2,5) + p7*c7*am(3,5) + p8*c8*am(4,5)
    nmx(6) = p5*c5*am(1,6) + p6*c6*am(2,6) + p7*c7*am(3,6) + p8*c8*am(4,6)
    nmx(7) = p5*c5*am(1,7) + p6*c6*am(2,7) + p7*c7*am(3,7) + p8*c8*am(4,7)
    nmx(8) = p5*c5*am(1,8) + p6*c6*am(2,8) + p7*c7*am(3,8) + p8*c8*am(4,8)
!
    nmy(1) = p5*s5*am(1,1) + p6*s6*am(2,1) + p7*s7*am(3,1) + p8*s8*am(4,1)
    nmy(2) = p5*s5*am(1,2) + p6*s6*am(2,2) + p7*s7*am(3,2) + p8*s8*am(4,2)
    nmy(3) = p5*s5*am(1,3) + p6*s6*am(2,3) + p7*s7*am(3,3) + p8*s8*am(4,3)
    nmy(4) = p5*s5*am(1,4) + p6*s6*am(2,4) + p7*s7*am(3,4) + p8*s8*am(4,4)
    nmy(5) = p5*s5*am(1,5) + p6*s6*am(2,5) + p7*s7*am(3,5) + p8*s8*am(4,5)
    nmy(6) = p5*s5*am(1,6) + p6*s6*am(2,6) + p7*s7*am(3,6) + p8*s8*am(4,6)
    nmy(7) = p5*s5*am(1,7) + p6*s6*am(2,7) + p7*s7*am(3,7) + p8*s8*am(4,7)
    nmy(8) = p5*s5*am(1,8) + p6*s6*am(2,8) + p7*s7*am(3,8) + p8*s8*am(4,8)
!
end subroutine
