      SUBROUTINE DSTNIB  ( INT, R, AN, AM, NFX, NFY, NMX, NMY )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2000   AUTEUR CIBHHGB G.BERTRAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C  
C  DSTNIB -- DETERMINATION DES FONCTIONS DE FORME DES ROTATIONS
C            SUR LES DEPLACEMENTS DE MEMBRANE ET DE FLEXION 
C            POUR LES ELEMENTS DE PLAQUE DST :
C            ON A L'INTERPOLATION SUIVANTE
C            BETA_X = NI*BETA_XI + PK*CK*ALPHA_K  
C            BETA_Y = NI*BETA_YI + PK*SK*ALPHA_K 
C            LES NI SONT LES FONCTIONS DE FORME CLASSIQUES DU TRIANGLE
C            A 3 NOEUDS I.E. :
C              N1 = 1 - QSI - ETA
C              N2 =     QSI
C              N3 =           ETA
C
C            LES PK SONT QUADRATIQUES :
C              P4 = 4*QSI*(1-QSI-ETA)
C              P5 = 4*QSI*ETA
C              P6 = 4*ETA*(1-QSI-ETA)
C
C            LES ALPHA DESIGNENT DES ROTATIONS SITUEES AU MILIEU DES
C            COTES DU TRIANGLE
C            ON RAPPELLE QUE POUR UNE PLAQUE NON EXCENTREE 
C                ALPHA = AN*UN 
C            ET QUE DANS LE CAS EXCENTRE 
C                ALPHA = AN*UN + AM*UM
C
C            UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)  
C            UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)
C                                     
C   ARGUMENT        E/S  TYPE         ROLE
C    INT            IN    I       INDICE DU POINT D'INTEGRATION
C    R(*)           IN    R       TABLEAU DE CARACTERISTIQUES
C                                 GEOMETRIQUES DE L'ELEMENT :
C                                 COS ET SIN DES ANGLES, LONGUEUR
C                                 DES COTES ,...
C    AN(3,9)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE FLEXION UN 
C    AM(3,6)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE MEMBRANE UM
C    NFX(9)         OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_X =  NFXI*WI + NFXJ*BETAXJ
C                                         (+NMX*UM)
C    NFY(9)         OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_Y =  NFYI*WI + NFYJ*BETAYJ
C                                         (+NMY*UM)
C    NMX(6)         OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_X =  NMX*UM
C                                        (+ NFXI*WI + NFXJ*BETAXJ)
C    NMY(6)         OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_Y =  NMY*UM
C                                        (+ NFYI*WI + NFYJ*BETAYJ)
C
C -----  ARGUMENTS
      INTEGER   INT
      REAL*8    R(*)
      REAL*8    AN(3,9), AM(3,6)
      REAL*8    NFX(9), NFY(9), NMX(6), NMY(6)
C -----  VARIABLES LOCALES
      REAL*8  QSI , ETA 
      REAL*8  C4,C5,C6 , S4,S5,S6
      REAL*8  N1, N2, N3
      REAL*8  P4, P5, P6
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN
               PARAMETER (NPG   = 3)
               PARAMETER (NNO   = 3)
               PARAMETER (NC    = 3)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI  + NPG + NNO )
               PARAMETER (LWGT  = LETA  + NPG + NNO )
               PARAMETER (LXYC  = LWGT  + NPG)
               PARAMETER (LCOTE = LXYC  + 2*NC)
               PARAMETER (LCOS  = LCOTE + NC)
               PARAMETER (LSIN  = LCOS  + NC)
C     ------------------------------------------------------------------
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      UN     = 1.0D0
      QUATRE = 4.0D0
C
      QSI = R(LQSI+INT-1)
      ETA = R(LETA+INT-1)
C
      C4   = R(LCOS)
      C5   = R(LCOS+1)
      C6   = R(LCOS+2)
      S4   = R(LSIN)
      S5   = R(LSIN+1)
      S6   = R(LSIN+2)
C
      N1 = UN - QSI - ETA
      N2 =      QSI
      N3 =            ETA
C
      P4 = QUATRE*QSI*(UN-QSI-ETA)
      P5 = QUATRE*QSI*ETA
      P6 = QUATRE*ETA*(UN-QSI-ETA)
C
C==============================================================
C --- FONCTIONS D'INTERPOLATIONS DES ROTATIONS POUR LES DDLS  =
C --- DE FLEXION :                                            =
C==============================================================
C
      NFX(1) =  P4*C4*AN(1,1) + P5*C5*AN(2,1) + P6*C6*AN(3,1)
      NFX(2) =  N1 + P4*C4*AN(1,2) + P5*C5*AN(2,2) + P6*C6*AN(3,2)
      NFX(3) =  P4*C4*AN(1,3) + P5*C5*AN(2,3) + P6*C6*AN(3,3)
C
      NFX(4) =  P4*C4*AN(1,4) + P5*C5*AN(2,4) + P6*C6*AN(3,4)
      NFX(5) =  N2 + P4*C4*AN(1,5) + P5*C5*AN(2,5) + P6*C6*AN(3,5)
      NFX(6) =  P4*C4*AN(1,6) + P5*C5*AN(2,6) + P6*C6*AN(3,6)
C
      NFX(7) =  P4*C4*AN(1,7) + P5*C5*AN(2,7) + P6*C6*AN(3,7)
      NFX(8) =  N3 + P4*C4*AN(1,8) + P5*C5*AN(2,8) + P6*C6*AN(3,8)
      NFX(9) =  P4*C4*AN(1,9) + P5*C5*AN(2,9) + P6*C6*AN(3,9)
C
      NFY(1) =  P4*S4*AN(1,1) + P5*S5*AN(2,1) + P6*S6*AN(3,1)
      NFY(2) =  P4*S4*AN(1,2) + P5*S5*AN(2,2) + P6*S6*AN(3,2)
      NFY(3) =  N1 + P4*S4*AN(1,3) + P5*S5*AN(2,3) + P6*S6*AN(3,3)
C
      NFY(4) =  P4*S4*AN(1,4) + P5*S5*AN(2,4) + P6*S6*AN(3,4)
      NFY(5) =  P4*S4*AN(1,5) + P5*S5*AN(2,5) + P6*S6*AN(3,5)
      NFY(6) =  N2 + P4*S4*AN(1,6) + P5*S5*AN(2,6) + P6*S6*AN(3,6)
C
      NFY(7) =  P4*S4*AN(1,7) + P5*S5*AN(2,7) + P6*S6*AN(3,7)
      NFY(8) =  P4*S4*AN(1,8) + P5*S5*AN(2,8) + P6*S6*AN(3,8)
      NFY(9) =  N3 + P4*S4*AN(1,9) + P5*S5*AN(2,9) + P6*S6*AN(3,9)
C
C==============================================================
C --- FONCTIONS D'INTERPOLATIONS DES ROTATIONS POUR LES DDLS  =
C --- DE MEMBRANE :                                           =
C==============================================================
C
      NMX(1) =  P4*C4*AM(1,1) + P5*C5*AM(2,1) + P6*C6*AM(3,1)
      NMX(2) =  P4*C4*AM(1,2) + P5*C5*AM(2,2) + P6*C6*AM(3,2)
      NMX(3) =  P4*C4*AM(1,3) + P5*C5*AM(2,3) + P6*C6*AM(3,3)
      NMX(4) =  P4*C4*AM(1,4) + P5*C5*AM(2,4) + P6*C6*AM(3,4)
      NMX(5) =  P4*C4*AM(1,5) + P5*C5*AM(2,5) + P6*C6*AM(3,5)
      NMX(6) =  P4*C4*AM(1,6) + P5*C5*AM(2,6) + P6*C6*AM(3,6)
C
      NMY(1) =  P4*S4*AM(1,1) + P5*S5*AM(2,1) + P6*S6*AM(3,1)
      NMY(2) =  P4*S4*AM(1,2) + P5*S5*AM(2,2) + P6*S6*AM(3,2)
      NMY(3) =  P4*S4*AM(1,3) + P5*S5*AM(2,3) + P6*S6*AM(3,3)
      NMY(4) =  P4*S4*AM(1,4) + P5*S5*AM(2,4) + P6*S6*AM(3,4)
      NMY(5) =  P4*S4*AM(1,5) + P5*S5*AM(2,5) + P6*S6*AM(3,5)
      NMY(6) =  P4*S4*AM(1,6) + P5*S5*AM(2,6) + P6*S6*AM(3,6)
C
      END
