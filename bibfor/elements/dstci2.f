      SUBROUTINE DSTCI2 ( DCI , R , HFT2 , DFC, DMC, BCA , AN, AM )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/06/2005   AUTEUR REZETTE C.REZETTE 
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
C  DSTCI2 -- DETERMINATION DES MATRICES AN ET AM QUI SONT TELLES QUE
C            ALPHA = AN*UN + AM*UM
C            POUR OBTENIR CETTE EXPRESSION ON PASSE PAR LA RELATION :
C            AA*ALPHA = (AW + AB)*UN + AL*UM
C            UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)  
C            UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)    
C            FORMELLEMENT :  
C                                       
C            |L4 0  0|   |L4C4 L4S4|                        
C   AA = 2/3*|0 L5  0| - |L5C5 L5S5|*DCI*(BCA - DFC_T*BFA)  
C            |0  0 L6|   |L6C6 L6S6|                        
C                       
C           |L4C4 L4S4|                                      
C    AB = - |L5C5 L5S5|*DCI*DFC_T*BFB                        
C           |L6C6 L6S6|
C                                      
C           |L4C4 L4S4|                                      
C    AL = - |L5C5 L5S5|*DCI*DMC_T*BM                         
C           |L6C6 L6S6|                                      
C                                    
C              |-2  L4C4 L4S4   2  L4C4 L4S4   0     0      0|  
C    AW = -1/2*| 0     0    0  -2  L5C5 L5S5   2  L5C5   L5S5|  
C              | 2  L6C6 L6S6   0     0    0  -2  L6C6   L6S6|  
C                                     
C
C   ARGUMENT        E/S  TYPE         ROLE
C    DCI(2,2)       IN    R       INVERSE DE LA MATRICE DE CISAILLEMENT
C                                 DE HOOKE
C    R(*)           IN    R       TABLEAU DE CARACTERISTIQUES
C                                 GEOMETRIQUES DE L'ELEMENT :
C                                 COS ET SIN DES ANGLES, LONGUEUR
C                                 DES COTES ,...
C    HFT2(2,6)      IN    R       MATRICE DEFINIE PAR T = [HF.T2] 
C                                 T = [HF.T2] BETA,QSI2
C                                 OU T EST LE VECTEUR DES EFFORTS
C                                 TRANCHANTS ET BETA,QSI2 DESIGNE LE
C                                 VECTEUR DES DERIVEES SECONDES DES
C                                 ROTATIONS BETA PAR RAPPORT AUX
C                                 COORDONNEES DE L'ELEMENT DE REFERENCE
C    DF(3,3)        IN    R       MATRICE DE FLEXION DE HOOKE
C    DFC(3,2)       IN    R       MATRICE DE COUPLAGE 
C                                 FLEXION-CISAILLEMENT DE HOOKE
C    DMC(3,2)       IN    R       MATRICE DE COUPLAGE 
C                                 MEMBRANE-CISAILLEMENT DE HOOKE
C    BCA(2,3)       OUT   R       MATRICE RELIANT LES EFFORTS TRANCHANTS
C                                 AUX INCONNUES ALPHA : T = [BCA]*ALPHA
C                                 PLUS PRECISEMMENT  :
C                                 T = [HFT.T2]*[TA]*ALPHA
C    AN(3,9)        OUT   R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE FLEXION UN 
C    AM(3,6)        OUT   R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE MEMBRANE UM
C
C -----  ARGUMENTS
      REAL*8   DCI(2,2)
      REAL*8   R(*)
      REAL*8   HFT2(2,6), DMC(3,2), DFC(3,2)
      REAL*8   BCA(2,3)
      REAL*8   AN(3,9), AM(3,6)
C -----  VARIABLES LOCALES
      REAL*8  L(3) , C(3) , S(3)
      REAL*8  X(3) , Y(3)
      REAL*8  QSI(2)
      REAL*8  TA(6,3)
      REAL*8  DB(2,3)
      REAL*8  AA(3,3)
      REAL*8  AAI(3,3)
      REAL*8  AW(3,9)
      REAL*8  AB(3,9), AL(3,6), DFCBFB(2,9), DFCBFA(2,3), BFA(3,3)
      REAL*8  BM(3,6), BFB(3,9), DMCTBM(3,6), DCIDMC(2,6), DCIDFB(2,9)
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
      CHARACTER*1 TRANS,KSTOP
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      QUATRE = 4.0D0
      HUIT   = 8.0D0
      RAC3   = SQRT(TROIS)
      QSI(1) = UN/RAC3
      QSI(2) = -QSI(1)
      ETA    =  ZERO
C
      DO 10 I = 1, 3
        DO 10 J = 1, 6
          AM(I,J)     = ZERO
          DMCTBM(I,J) = ZERO
  10  CONTINUE
C
      DO 20 I = 1, 3
        DO 20 J = 1, 9
          AN(I,J)  = ZERO
          AW(I,J)  = ZERO
  20  CONTINUE
C
      DO 30 I = 1, 2
        DO 30 J = 1, 9
          DFCBFB(I,J) = ZERO
  30  CONTINUE
C
      DO 40 I = 1, 2
        DO 40 J = 1, 3
          BCA(I,J)    = ZERO
  40  CONTINUE
C
      DO 50 I = 1, 6
        DO 50 J = 1, 3
          TA(I,J) = ZERO
  50  CONTINUE
C      
      C(1) = R(LCOS)
      C(2) = R(LCOS+1)
      C(3) = R(LCOS+2)
      S(1) = R(LSIN)
      S(2) = R(LSIN+1)
      S(3) = R(LSIN+2)
      L(1) = R(LCOTE)
      L(2) = R(LCOTE+1)
      L(3) = R(LCOTE+2)
      X(1) = R(LXYC)
      X(2) = R(LXYC+1)
      X(3) = R(LXYC+2)
      Y(1) = R(LXYC+3)
      Y(2) = R(LXYC+4)
      Y(3) = R(LXYC+5)
C
      TA(1,1) =   - HUIT * C(1)
      TA(2,3) =   - HUIT * C(3)
      TA(3,1) = - QUATRE * C(1)
      TA(3,2) =   QUATRE * C(2)
      TA(3,3) = - QUATRE * C(3)
      TA(4,1) =   - HUIT * S(1)
      TA(5,3) =   - HUIT * S(3)
      TA(6,1) = - QUATRE * S(1)
      TA(6,2) =   QUATRE * S(2)
      TA(6,3) = - QUATRE * S(3)
C
C --- CALCUL DU PRODUIT HFT2.TA :
C     -------------------------
      DO 60 J = 1, 3
         DO 60 K = 1, 6
            BCA(1,J) = BCA(1,J) + HFT2(1,K) * TA(K,J)
            BCA(2,J) = BCA(2,J) + HFT2(2,K) * TA(K,J)
  60  CONTINUE
C
C================================================================
C --- DETERMINATION DE LA MATRICE AA QUI EST TELLE QUE          = 
C --- AA*ALPHA = (AW + AB)*UN + AL*UM                           =
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)    =
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)           =
C ---  FORMELLEMENT                                             =
C ---            |L4 0  0|   |L4C4 L4S4|                        =
C ---   AA = 2/3*|0 L5  0| - |L5C5 L5S5|*DCI*(BCA - DFC_T*BFA)  =
C ---            |0  0 L6|   |L6C6 L6S6|                        =
C================================================================
C
C --- BOUCLE SUR LES COTES DU TRIANGLE :
C      --------------------------------
      DO 70 IC = 1, 3
C
C ---     INITIALISATION DE DFCBFA :
C         ------------------------
          DO 80 I = 1, 2
            DO 80 J = 1, 3
            DFCBFA(I,J) = ZERO
  80      CONTINUE
C
C ---   BOUCLE SUR LES POINTS D'INTEGRATION DU COTE COURANT :
C       ---------------------------------------------------
        DO 90 INT = 1, 2
C 
C ---     CALCUL DE LA MATRICE BFA AU POINT D'INTEGRATION COURANT 
C ---     RELIANT LES COURBURES AUX INCONNUES ALPHA
C ---     I.E. X = BFB*UN + BFA*ALPHA :
C         ---------------------------
          CALL DSTBFA(QSI(INT), ETA, R, BFA)
C
C ---     CALCUL DU PRODUIT DFC_T*BFA :
C         ---------------------------
          DO 100 J = 1, 3
            DO 100 K = 1, 3
              DFCBFA(1,J) = DFCBFA(1,J) + DFC(K,1)*BFA(K,J)
              DFCBFA(2,J) = DFCBFA(2,J) + DFC(K,2)*BFA(K,J)
 100      CONTINUE
C
          DO 110 I = 1, 2
            DO 110 J = 1, 3
              DFCBFA(I,J) = UNDEMI*DFCBFA(I,J)
 110      CONTINUE
C
  90   CONTINUE
C     -------------------------------------------------------------
C --  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION DU COTE COURANT
C     -------------------------------------------------------------
C
C ---   CALCUL DU PRODUIT DCI*(BCA - DFC_T*BFA) :
C       --------------------------------------
        DO 120 J = 1, 3
          DB(1,J) =  DCI(1,1) * (BCA(1,J)-DFCBFA(1,J))
     +             + DCI(1,2) * (BCA(2,J)-DFCBFA(2,J))
          DB(2,J) =  DCI(2,1) * (BCA(1,J)-DFCBFA(1,J))
     +             + DCI(2,2) * (BCA(2,J)-DFCBFA(2,J))
 120      CONTINUE
C
C ---               |L4 0  0|   |L4C4 L4S4|
C --- CALCUL DE 2/3*|0 L5  0| - |L5C5 L5S5|*DCI*(BCA - DFC_T*BFA) :
C ---               |0  0 L6|   |L6C6 L6S6|
C --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y :
C     -------------------------------------------
         DO 130 J = 1, 3
            AA(IC,J) = - (X(IC) * DB(1,J) + Y(IC) * DB(2,J))
 130     CONTINUE
         AA(IC,IC) = AA(IC,IC) + DEUX/TROIS * L(IC)
C
  70  CONTINUE
C     -------------------------------------------
C --  FIN DE LA BOUCLE SUR LES COTES DE L'ELEMENT
C     -------------------------------------------
C
C================================================================
C --- DETERMINATION DE LA MATRICE AB QUI EST TELLE QUE          = 
C --- AA*ALPHA = (AW + AB)*UN + AL*UM                           =
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)    =
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)           =
C ---  FORMELLEMENT                                             =
C ---          |L4C4 L4S4|                                      =
C ---   AB = - |L5C5 L5S5|*DCI*DFC_T*BFB                        =
C ---          |L6C6 L6S6|                                      =
C================================================================
C
C --- CALCUL DE LA MATRICE BFB RELIANT LES COURBURES AUX INCONNUES
C --- DE FLEXION UN (X = BFB*UN+BFA*ALPHA) :
C     ------------------------------------
      CALL DSTBFB(R, BFB)
C
C --- CALCUL DU PRODUIT DFC_T*BFB :
C     ---------------------------
      DO 140 J = 1, 9
        DO 140 K = 1, 3
          DFCBFB(1,J) = DFCBFB(1,J) + DFC(K,1)*BFB(K,J)
          DFCBFB(2,J) = DFCBFB(2,J) + DFC(K,2)*BFB(K,J)
 140  CONTINUE
C
C --- CALCUL DU PRODUIT DCI*DFC_T*BFB :
C     -------------------------------
      DO 150 J = 1, 9
         DCIDFB(1,J) = DCI(1,1)*DFCBFB(1,J) + DCI(1,2)*DFCBFB(2,J)
         DCIDFB(2,J) = DCI(2,1)*DFCBFB(1,J) + DCI(2,2)*DFCBFB(2,J)
 150  CONTINUE
C
C ---  CALCUL DE :                                            
C ---          |L4C4 L4S4|                                     
C ---   AB = - |L5C5 L5S5|*DCI*DFC_T*BFB                       
C ---          |L6C6 L6S6|                                     
C --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y :
C     -------------------------------------------
      DO 160 I = 1 , 3
         DO 160 J = 1, 9
           AB(I,J) = - (X(I)*DCIDFB(1,J) + Y(I)*DCIDFB(2,J))
 160  CONTINUE
C
C================================================================
C --- DETERMINATION DE LA MATRICE AL QUI EST TELLE QUE          = 
C --- AA*ALPHA = (AW + AB)*UN + AL*UM                           =
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)    =
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)           =
C ---  FORMELLEMENT                                             =
C ---          |L4C4 L4S4|                                      =
C ---   AL = - |L5C5 L5S5|*DCI*DMC_T*BM                         =
C ---          |L6C6 L6S6|                                      =
C================================================================
C
C --- CALCUL DE LA MATRICE BM RELIANT LES DEFORMATIONS MEMBRANAIRES
C --- AUX DEPLACEMENTS DE MEMBRANE UM (I.E. (UX,UY) ) :
C     -----------------------------------------------
      CALL DXTBM(R, BM)
C
C --- CALCUL DU PRODUIT DMC_T*BM :
C     --------------------------
      DO 170 J = 1, 6
        DO 170 K = 1, 3
          DMCTBM(1,J) = DMCTBM(1,J) + DMC(K,1)*BM(K,J)
          DMCTBM(2,J) = DMCTBM(2,J) + DMC(K,2)*BM(K,J)
 170  CONTINUE
C
C --- CALCUL DU PRODUIT DCI*DMC_T*BM :
C     ------------------------------
      DO 180 J = 1, 6
         DCIDMC(1,J) = DCI(1,1)*DMCTBM(1,J) + DCI(1,2)*DMCTBM(2,J)
         DCIDMC(2,J) = DCI(2,1)*DMCTBM(1,J) + DCI(2,2)*DMCTBM(2,J)
 180  CONTINUE
C
C ---  CALCUL DE :                                            
C ---          |L4C4 L4S4|                                     
C ---   AL = - |L5C5 L5S5|*DCI*DMC_T*BM                       
C ---          |L6C6 L6S6|                                     
C --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y :
C     -------------------------------------------
      DO 190 I = 1 , 3
         DO 190 J = 1, 6
           AL(I,J) = - (X(I)*DCIDMC(1,J) + Y(I)*DCIDMC(2,J))
 190  CONTINUE
C
C=================================
C --- INVERSION DE LA MATRICE AA =
C=================================
C
      DO 200 I = 1, 3
        DO 200 J = 1, 3
         AAI(I,J) = ZERO
 200  CONTINUE
      DO 210 I = 1, 3
         AAI(I,I) = UN
 210  CONTINUE
      TRANS=' '
      KSTOP='S'
      CALL MGAUSS (TRANS,KSTOP,AA , AAI , 3 , 3 , 3, DET, IRET )
C
C===================================================================
C --- DETERMINATION DE LA MATRICE AW QUI EST TELLE QUE             = 
C --- AA*ALPHA = (AW + AB)*UN + AL*UM                              =
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)       =
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)              =
C ---  FORMELLEMENT                                                =
C ---             |-2  L4C4 L4S4   2  L4C4 L4S4   0     0      0|  =
C ---   AW = -1/2*| 0     0    0  -2  L5C5 L5S5   2  L5C5   L5S5|  =
C ---             | 2  L6C6 L6S6   0     0    0  -2  L6C6   L6S6|  =
C===================================================================
C
      AW(1,1) =   UN
      AW(1,2) = - UNDEMI*X(1)
      AW(1,3) = - UNDEMI*Y(1)
      AW(1,4) = - UN
      AW(1,5) = - UNDEMI*X(1)
      AW(1,6) = - UNDEMI*Y(1)
      AW(2,4) =   UN
      AW(2,5) = - UNDEMI*X(2)
      AW(2,6) = - UNDEMI*Y(2)
      AW(2,7) = - UN
      AW(2,8) = - UNDEMI*X(2)
      AW(2,9) = - UNDEMI*Y(2)
      AW(3,1) = - UN
      AW(3,2) = - UNDEMI*X(3)
      AW(3,3) = - UNDEMI*Y(3)
      AW(3,7) =   UN
      AW(3,8) = - UNDEMI*X(3)
      AW(3,9) = - UNDEMI*Y(3)
C
C===================================================================
C --- DETERMINATION DE LA MATRICE AN QUI EST TELLE QUE             = 
C --- ALPHA = AN*UN + AM*UM                                        =
C --- SOIT AN = AAI * (AW + AB)                                    =
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)       =
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)              =
C===================================================================
C
      DO 220 I = 1, 3
         DO 220 K = 1, 3
            DO 220 J = 1, 9
               AN(I,J) = AN(I,J) + AAI(I,K) * (AW(K,J)+AB(K,J))
 220  CONTINUE
C
C===================================================================
C --- DETERMINATION DE LA MATRICE AM QUI EST TELLE QUE             = 
C --- ALPHA = AN*UN + AM*UM                                        =
C --- SOIT AM = AAI*AL                                             =
C===================================================================
C
      DO 230 I = 1, 3
         DO 230 K = 1, 3
            DO 230 J = 1, 6
               AM(I,J) = AM(I,J) + AAI(I,K) * AL(K,J)
 230  CONTINUE
C
      END
