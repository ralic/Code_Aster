      SUBROUTINE DKQNIB  ( INT , R , NFX, NFY )
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
C  DKQNIB -- DETERMINATION DES FONCTIONS DE FORME DES ROTATIONS
C            SUR LES DEPLACEMENTS DE MEMBRANE ET DE FLEXION 
C            POUR LES ELEMENTS DE PLAQUE DKQ :
C            ON A L'INTERPOLATION SUIVANTE
C            BETA_X = NI*BETA_XI + PK*CK*ALPHA_K  
C            BETA_Y = NI*BETA_YI + PK*SK*ALPHA_K 
C
C            LES NI SONT LES FONCTIONS DE FORME CLASSIQUES DU 
C            QUADRANGLE A 4 NOEUDS I.E. :
C              N1 = 1/4*(1-QSI)*(1-ETA)
C              N2 = 1/4*(1+QSI)*(1-ETA)
C              N3 = 1/4*(1+QSI)*(1+ETA)
C              N4 = 1/4*(1-QSI)*(1+ETA)
C
C            LES PK SONT QUADRATIQUES :
C              P5 = 1/2*(1-QSI**2)*(1-ETA)
C              P6 = 1/2*(1-ETA**2)*(1+QSI)
C              P7 = 1/2*(1-QSI**2)*(1+ETA)
C              P8 = 1/2*(1-ETA**2)*(1-QSI)
C
C            LES ALPHA DESIGNENT DES ROTATIONS SITUEES AU MILIEU DES
C            COTES DU TRIANGLE
C            ON RAPPELLE QUE ALPHA = AN*UN
C            LA MATRICE AN A UNE EXPRESSION PLUS SIMPLE QUE POUR LE DSQ 
C            ELLE EST OBTENUE EN ECRIVANT QUE LES DEFORMATIONS DE
C            CISAILLEMENT SONT NULLES EN MOYENNE LE LONG DES
C            COTES DE L'ELEMENT
C
C            UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)  
C                                     
C   ARGUMENT        E/S  TYPE         ROLE
C    INT            IN    I       INDICE DU POINT D'INTEGRATION
C    R(*)           IN    R       TABLEAU DE CARACTERISTIQUES
C                                 GEOMETRIQUES DE L'ELEMENT :
C                                 COS ET SIN DES ANGLES, LONGUEUR
C                                 DES COTES ,...
C    NFX(12)        OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_X =  NFXI*WI + NFXJ*BETAXJ
C    NFY(12)        OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_Y =  NFYI*WI + NFYJ*BETAYJ
C
C -----  ARGUMENTS
      INTEGER  INT
      REAL*8   R(*)
      REAL*8   NFX(12), NFY(12)
C -----  VARIABLES LOCALES
      REAL*8  QSI , ETA 
      REAL*8  L5,L6 , L7, L8, C5, C6, C7, C8, S5, S6, S7, S8
      REAL*8  N1, N2, N3, N4
      REAL*8  P5, P6, P7, P8
C
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN
               PARAMETER (NPG   = 4)
               PARAMETER (NNO   = 4)
               PARAMETER (NC    = 4)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI + NPG + NNO + 2*NC)
               PARAMETER (LWGT  = LETA + NPG + NNO + 2*NC)
               PARAMETER (LXYC  = LWGT + NPG)
               PARAMETER (LCOTE = LXYC + 2*NC)
               PARAMETER (LCOS  = LCOTE + NC)
               PARAMETER (LSIN  = LCOS + NC)
C     ------------------------------------------------------------------
      UNQUAR = 0.25D0
      UNDEMI = 0.50D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      QUATRE = 4.0D0
C
      QSI = R(LQSI+INT-1)
      ETA = R(LETA+INT-1)
C
      C5   = R(LCOS)
      C6   = R(LCOS+1)
      C7   = R(LCOS+2)
      C8   = R(LCOS+3)
      S5   = R(LSIN)
      S6   = R(LSIN+1)
      S7   = R(LSIN+2)
      S8   = R(LSIN+3)
      L5   = R(LCOTE)
      L6   = R(LCOTE+1)
      L7   = R(LCOTE+2)
      L8   = R(LCOTE+3)
C
      N1 = UNQUAR*(UN-QSI)*(UN-ETA)
      N2 = UNQUAR*(UN+QSI)*(UN-ETA)     
      N3 = UNQUAR*(UN+QSI)*(UN+ETA)                
      N4 = UNQUAR*(UN-QSI)*(UN+ETA)                
C
      P5 = UNDEMI*(UN-QSI*QSI)*(UN-ETA)
      P6 = UNDEMI*(UN-ETA*ETA)*(UN+QSI)
      P7 = UNDEMI*(UN-QSI*QSI)*(UN+ETA)
      P8 = UNDEMI*(UN-ETA*ETA)*(UN-QSI)
C
      NFX(1)  =  TROIS/DEUX*(P5*C5/L5 - P8*C8/L8)
      NFX(2)  =  N1 - TROIS/QUATRE*(P5*C5*C5 + P8*C8*C8)
      NFX(3)  = -TROIS/QUATRE*(P5*C5*S5 + P8*C8*S8)
C
      NFX(4)  =  TROIS/DEUX*(P6*C6/L6 - P5*C5/L5)
      NFX(5)  =  N2 - TROIS/QUATRE*(P6*C6*C6 + P5*C5*C5)
      NFX(6)  = -TROIS/QUATRE*(P6*C6*S6 + P5*C5*S5)
C
      NFX(7)  =  TROIS/DEUX*(P7*C7/L7 - P6*C6/L6)
      NFX(8)  =  N3 - TROIS/QUATRE*(P7*C7*C7 + P6*C6*C6)
      NFX(9)  = -TROIS/QUATRE*(P7*C7*S7 + P6*C6*S6)
C
      NFX(10) =  TROIS/DEUX*(P8*C8/L8 - P7*C7/L7)
      NFX(11) =  N4 - TROIS/QUATRE*(P8*C8*C8 + P7*C7*C7)
      NFX(12) = -TROIS/QUATRE*(P8*C8*S8 + P7*C7*S7)
C
      NFY(1)  =  TROIS/DEUX*(P5*S5/L5 - P8*S8/L8)
      NFY(2)  =  NFX(3)
      NFY(3)  =  N1 - TROIS/QUATRE*(P5*S5*S5 + P8*S8*S8)
C
      NFY(4)  =  TROIS/DEUX*(P6*S6/L6 - P5*S5/L5)
      NFY(5)  =  NFX(6)
      NFY(6)  =  N2 - TROIS/QUATRE*(P6*S6*S6 + P5*S5*S5)
C
      NFY(7)  =  TROIS/DEUX*(P7*S7/L7 - P6*S6/L6)
      NFY(8)  =  NFX(9)
      NFY(9)  =  N3 - TROIS/QUATRE*(P7*S7*S7 + P6*S6*S6)
C
      NFY(10) =  TROIS/DEUX*(P8*S8/L8 - P7*S7/L7)
      NFY(11) =  NFX(12)
      NFY(12) =  N4 - TROIS/QUATRE*(P8*S8*S8 + P7*S7*S7)
C
      END
