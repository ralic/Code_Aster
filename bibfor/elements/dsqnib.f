      SUBROUTINE DSQNIB ( QSI, ETA, CARAQ4, AN, AM, NFX, NFY, NMX, NMY )
      IMPLICIT  NONE
      REAL*8    QSI, ETA, CARAQ4(*)
      REAL*8    AN(4,12), AM(4,8), NFX(12), NFY(12), NMX(8), NMY(8)
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
C  
C  DSQNIB -- DETERMINATION DES FONCTIONS DE FORME DES ROTATIONS
C            SUR LES DEPLACEMENTS DE MEMBRANE ET DE FLEXION 
C            POUR LES ELEMENTS DE PLAQUE DSQ :
C            ON A L'INTERPOLATION SUIVANTE
C            BETA_X = NI*BETA_XI + PK*CK*ALPHA_K  
C            BETA_Y = NI*BETA_YI + PK*SK*ALPHA_K 
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
C            COTES DU QUADRANGLE
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
C    AN(4,12)       IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE FLEXION UN 
C    AM(4,8)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE MEMBRANE UM
C    NFX(12)        OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_X =  NFXI*WI + NFXJ*BETAXJ
C                                         (+NMX*UM)
C    NFY(12)        OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_Y =  NFYI*WI + NFYJ*BETAYJ
C                                         (+NMY*UM)
C    NMX(8)         OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_X =  NMX*UM
C                                        (+ NFXI*WI + NFXJ*BETAXJ)
C    NMY(8)         OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 BETA_Y =  NMY*UM
C                                        (+ NFYI*WI + NFYJ*BETAYJ)
C     ------------------------------------------------------------------
      REAL*8  C5,C6,C7,C8,S5,S6,S7,S8
      REAL*8  N1, N2, N3, N4, P5, P6, P7, P8
      REAL*8  UNQUAR, UNDEMI, UN
C     ------------------------------------------------------------------
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      UNQUAR = 0.25D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
C
      C5   = CARAQ4(13)
      C6   = CARAQ4(14)
      C7   = CARAQ4(15)
      C8   = CARAQ4(16)
      S5   = CARAQ4(17)
      S6   = CARAQ4(18)
      S7   = CARAQ4(19)
      S8   = CARAQ4(20)
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
C==============================================================
C --- FONCTIONS D'INTERPOLATIONS DES ROTATIONS POUR LES DDLS  =
C --- DE FLEXION :                                            =
C==============================================================
C
      NFX(1) =  P5*C5*AN(1,1) + P6*C6*AN(2,1) + P7*C7*AN(3,1)
     +        + P8*C8*AN(4,1)
      NFX(2) =  N1 + P5*C5*AN(1,2) + P6*C6*AN(2,2) + P7*C7*AN(3,2)
     +        + P8*C8*AN(4,2)
      NFX(3) =  P5*C5*AN(1,3) + P6*C6*AN(2,3) + P7*C7*AN(3,3)
     +        + P8*C8*AN(4,3)
C
      NFX(4) =  P5*C5*AN(1,4) + P6*C6*AN(2,4) + P7*C7*AN(3,4)
     +        + P8*C8*AN(4,4)
      NFX(5) =  N2 + P5*C5*AN(1,5) + P6*C6*AN(2,5) + P7*C7*AN(3,5)
     +        + P8*C8*AN(4,5)
      NFX(6) =  P5*C5*AN(1,6) + P6*C6*AN(2,6) + P7*C7*AN(3,6)
     +        + P8*C8*AN(4,6)
C
      NFX(7) =  P5*C5*AN(1,7) + P6*C6*AN(2,7) + P7*C7*AN(3,7)
     +        + P8*C8*AN(4,7)
      NFX(8) =  N3 + P5*C5*AN(1,8) + P6*C6*AN(2,8) + P7*C7*AN(3,8)
     +        + P8*C8*AN(4,8)
      NFX(9) =  P5*C5*AN(1,9) + P6*C6*AN(2,9) + P7*C7*AN(3,9)
     +        + P8*C8*AN(4,9)
C
      NFX(10) =  P5*C5*AN(1,10) + P6*C6*AN(2,10) + P7*C7*AN(3,10)
     +         + P8*C8*AN(4,10)
      NFX(11) =  N4 + P5*C5*AN(1,11) + P6*C6*AN(2,11) + P7*C7*AN(3,11)
     +         + P8*C8*AN(4,11)
      NFX(12) =  P5*C5*AN(1,12) + P6*C6*AN(2,12) + P7*C7*AN(3,12)
     +         + P8*C8*AN(4,12)
C
      NFY(1) =  P5*S5*AN(1,1) + P6*S6*AN(2,1) + P7*S7*AN(3,1)
     +        + P8*S8*AN(4,1)
      NFY(2) =  P5*S5*AN(1,2) + P6*S6*AN(2,2) + P7*S7*AN(3,2)
     +        + P8*S8*AN(4,2)
      NFY(3) =  N1 + P5*S5*AN(1,3) + P6*S6*AN(2,3) + P7*S7*AN(3,3)
     +        + P8*S8*AN(4,3)
C
      NFY(4) =  P5*S5*AN(1,4) + P6*S6*AN(2,4) + P7*S7*AN(3,4)
     +        + P8*S8*AN(4,4)
      NFY(5) =  P5*S5*AN(1,5) + P6*S6*AN(2,5) + P7*S7*AN(3,5)
     +        + P8*S8*AN(4,5)
      NFY(6) =  N2 + P5*S5*AN(1,6) + P6*S6*AN(2,6) + P7*S7*AN(3,6)
     +        + P8*S8*AN(4,6)
C
      NFY(7) =  P5*S5*AN(1,7) + P6*S6*AN(2,7) + P7*S7*AN(3,7)
     +        + P8*S8*AN(4,7)
      NFY(8) =  P5*S5*AN(1,8) + P6*S6*AN(2,8) + P7*S7*AN(3,8)
     +        + P8*S8*AN(4,8)
      NFY(9) =  N3 + P5*S5*AN(1,9) + P6*S6*AN(2,9) + P7*S7*AN(3,9)
     +        + P8*S8*AN(4,9)
C
      NFY(10) =  P5*S5*AN(1,10) + P6*S6*AN(2,10) + P7*S7*AN(3,10)
     +         + P8*S8*AN(4,10)
      NFY(11) =  P5*S5*AN(1,11) + P6*S6*AN(2,11) + P7*S7*AN(3,11)
     +         + P8*S8*AN(4,11)
      NFY(12) =  N4 + P5*S5*AN(1,12) + P6*S6*AN(2,12) + P7*S7*AN(3,12)
     +         + P8*S8*AN(4,12)
C
C==============================================================
C --- FONCTIONS D'INTERPOLATIONS DES ROTATIONS POUR LES DDLS  =
C --- DE MEMBRANE :                                           =
C==============================================================
C
      NMX(1) =  P5*C5*AM(1,1) + P6*C6*AM(2,1) + P7*C7*AM(3,1)
     +        + P8*C8*AM(4,1)
      NMX(2) =  P5*C5*AM(1,2) + P6*C6*AM(2,2) + P7*C7*AM(3,2)
     +        + P8*C8*AM(4,2)
      NMX(3) =  P5*C5*AM(1,3) + P6*C6*AM(2,3) + P7*C7*AM(3,3)
     +        + P8*C8*AM(4,3)
      NMX(4) =  P5*C5*AM(1,4) + P6*C6*AM(2,4) + P7*C7*AM(3,4)
     +        + P8*C8*AM(4,4)
      NMX(5) =  P5*C5*AM(1,5) + P6*C6*AM(2,5) + P7*C7*AM(3,5)
     +        + P8*C8*AM(4,5)
      NMX(6) =  P5*C5*AM(1,6) + P6*C6*AM(2,6) + P7*C7*AM(3,6)
     +        + P8*C8*AM(4,6)
      NMX(7) =  P5*C5*AM(1,7) + P6*C6*AM(2,7) + P7*C7*AM(3,7)
     +        + P8*C8*AM(4,7)
      NMX(8) =  P5*C5*AM(1,8) + P6*C6*AM(2,8) + P7*C7*AM(3,8)
     +        + P8*C8*AM(4,8)
C
      NMY(1) =  P5*S5*AM(1,1) + P6*S6*AM(2,1) + P7*S7*AM(3,1)
     +        + P8*S8*AM(4,1)
      NMY(2) =  P5*S5*AM(1,2) + P6*S6*AM(2,2) + P7*S7*AM(3,2)
     +        + P8*S8*AM(4,2)
      NMY(3) =  P5*S5*AM(1,3) + P6*S6*AM(2,3) + P7*S7*AM(3,3)
     +        + P8*S8*AM(4,3)
      NMY(4) =  P5*S5*AM(1,4) + P6*S6*AM(2,4) + P7*S7*AM(3,4)
     +        + P8*S8*AM(4,4)
      NMY(5) =  P5*S5*AM(1,5) + P6*S6*AM(2,5) + P7*S7*AM(3,5)
     +        + P8*S8*AM(4,5)
      NMY(6) =  P5*S5*AM(1,6) + P6*S6*AM(2,6) + P7*S7*AM(3,6)
     +        + P8*S8*AM(4,6)
      NMY(7) =  P5*S5*AM(1,7) + P6*S6*AM(2,7) + P7*S7*AM(3,7)
     +        + P8*S8*AM(4,7)
      NMY(8) =  P5*S5*AM(1,8) + P6*S6*AM(2,8) + P7*S7*AM(3,8)
     +        + P8*S8*AM(4,8)
C
      END
