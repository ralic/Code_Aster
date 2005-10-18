      SUBROUTINE DKTNIB  ( QSI, ETA, CARAT3, NFX, NFY )
      IMPLICIT  NONE
      REAL*8    QSI, ETA, CARAT3(*), NFX(9), NFY(9)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2005   AUTEUR CIBHHLV L.VIVAN 
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
C  DKTNIB -- DETERMINATION DES FONCTIONS DE FORME DES ROTATIONS
C            SUR LES DEPLACEMENTS DE MEMBRANE ET DE FLEXION 
C            POUR LES ELEMENTS DE PLAQUE DKT :
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
C            ON RAPPELLE QUE ALPHA = AN*UN
C            LA MATRICE AN A UNE EXPRESSION PLUS SIMPLE QUE POUR LE DST 
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
C    NFX(9)         OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_X =  NFXI*WI + NFXJ*BETAXJ
C    NFY(9)         OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_Y =  NFYI*WI + NFYJ*BETAYJ
C     ------------------------------------------------------------------
      REAL*8  L4,L5,L6 , C4,C5,C6 , S4,S5,S6 , N1,N2,N3 , P4,P5,P6
      REAL*8  UN, DEUX, TROIS, QUATRE
C     ------------------------------------------------------------------
C
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      QUATRE = 4.0D0
C
      C4   = CARAT3(16)
      C5   = CARAT3(17)
      C6   = CARAT3(18)
      S4   = CARAT3(19)
      S5   = CARAT3(20)
      S6   = CARAT3(21)
      L4   = CARAT3(13)
      L5   = CARAT3(14)
      L6   = CARAT3(15)
C
      N1 = UN - QSI - ETA
      N2 =      QSI
      N3 =            ETA
C
      P4 = QUATRE*QSI*(UN-QSI-ETA)
      P5 = QUATRE*QSI*ETA
      P6 = QUATRE*ETA*(UN-QSI-ETA)
C
      NFX(1) =  TROIS/DEUX*(P4*C4/L4 - P6*C6/L6)
      NFX(2) =  N1 - TROIS/QUATRE*(P4*C4*C4 + P6*C6*C6)
      NFX(3) = -TROIS/QUATRE*(P4*C4*S4 + P6*C6*S6)
C
      NFX(4) =  TROIS/DEUX*(P5*C5/L5 - P4*C4/L4)
      NFX(5) =  N2 - TROIS/QUATRE*(P5*C5*C5 + P4*C4*C4)
      NFX(6) = -TROIS/QUATRE*(P5*C5*S5 + P4*C4*S4)
C
      NFX(7) =  TROIS/DEUX*(P6*C6/L6 - P5*C5/L5)
      NFX(8) =  N3 - TROIS/QUATRE*(P6*C6*C6 + P5*C5*C5)
      NFX(9) = -TROIS/QUATRE*(P6*C6*S6 + P5*C5*S5)
C
      NFY(1) =  TROIS/DEUX*(P4*S4/L4 - P6*S6/L6)
      NFY(2) =  NFX(3)
      NFY(3) =  N1 - TROIS/QUATRE*(P4*S4*S4 + P6*S6*S6)
C
      NFY(4) =  TROIS/DEUX*(P5*S5/L5 - P4*S4/L4)
      NFY(5) =  NFX(6)
      NFY(6) =  N2 - TROIS/QUATRE*(P5*S5*S5 + P4*S4*S4)
C
      NFY(7) =  TROIS/DEUX*(P6*S6/L6 - P5*S5/L5)
      NFY(8) =  NFX(9)
      NFY(9) =  N3 - TROIS/QUATRE*(P6*S6*S6 + P5*S5*S5)
C
      END
