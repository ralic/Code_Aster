      SUBROUTINE DSQDI2 ( NOMTE, XYZL, DF, DCI, DMF, DFC, DMC, AN, AM )
      IMPLICIT  NONE
      REAL*8    XYZL(3,*), DF(3,3), DMC(3,2), DFC(3,2), DCI(2,2),
     +          DMF(3,3), AN(4,12), AM(4,8)
      CHARACTER*16  NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2004   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE CRP_20
C.======================================================================
C  
C  DSQDI2 -- DETERMINATION DES MATRICES AN ET AM QUI SONT TELLES QUE
C            ALPHA = AN*UN + AM*UM   
C            POUR OBTENIR CETTE EXPRESSION ON PASSE PAR LA RELATION:  
C            AA*ALPHA = (AW + AB)*UN + AL*UM  
C            UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)
C            UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY) 
C            FORMELLEMENT :  
C                                       
C             |L5 0  0  0|   |L5C5 L5S5|                        
C    AA = 2/3*|0 L6  0  0| - |L6C6 L6S6|*DCI*(BCA - DFC_T*BFA)  
C             |0  0 L7  0|   |L7C7 L7S7|                        
C             |0  0  0 L8|   |L8C8 L8S8| 
C                       
C           |L5C5 L5S5|                                      
C    AB = - |L6C6 L6S6|*DCI*DFC_T*BFB                        
C           |L7C7 L7S7|                                      
C           |L8C8 L8S8|  
C                                    
C           |L5C5 L5S5|                                      
C    AL = - |L6C6 L6S6|*DCI*DMC_T*BM                         
C           |L7C7 L7S7|                                      
C           |L8C8 L8S8| 
C
C         |-2  L5C5 L5S5   2  L5C5 L5S5   0     0      0   0     0    0|
C AW=-0.5*| 0     0    0  -2  L6C6 L6S6   2  L6C6   L6S6   0     0    0|
C         | 0     0    0   0     0     0 -2  L7C7   L7S7   2 L7C7  L7S7|
C         | 2  L8C8 L8S8   0     0     0  0     0      0  -2 L8C8  L8S8|
C
C         |L5C5 L5S5|
C       + |L6C6 L6S6|*DCI*BCB 
C         |L7C7 L7S7|
C         |L8C8 L8S8|
C
C
C   ARGUMENT        E/S  TYPE         ROLE
C    XYZL(3,*)      IN    R       COORDONNEES DES NOEUDS DU DSQ
C    DF(3,3)        IN    R       MATRICE DE FLEXION DE HOOKE
C    DCI(2,2)       IN    R       INVERSE DE LA MATRICE DE CISAILLEMENT
C                                 DE HOOKE
C    DMF(3,3)       IN    R       MATRICE DE COUPLAGE 
C                                 MEMBRANE-FLEXION DE HOOKE
C    DFC(3,2)       IN    R       MATRICE DE COUPLAGE 
C                                 FLEXION-CISAILLEMENT DE HOOKE
C    DMC(3,2)       IN    R       MATRICE DE COUPLAGE 
C                                 MEMBRANE-CISAILLEMENT DE HOOKE
C    AN(4,12)       OUT   R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE FLEXION UN 
C    AM(4,8)        OUT   R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE MEMBRANE UM
C
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C -----  VARIABLES LOCALES
      INTEGER           LZR,I, J, K, IC, INT
          REAL*8        QSI,ETA,ZERO,UNDEMI,UN,DEUX,TROIS
          REAL*8        L(4)
          REAL*8        X(4) , Y(4)
          REAL*8        HFT2(2,6), DFCBFA(2,4), HMFT2(2,6)
          REAL*8        DFCBFB(2,12), DCIDFB(2,12), BFA(3,4) 
          REAL*8        DMCTBM(2,8), AB(4,12), AW(4,12), DCIDMC(2,8) 
          REAL*8        BFB(3,12), BM(3,8), AL(4,8)
          REAL*8        BCB(2,12), BCA(2,4), BCM(2,8)
          REAL*8        DB(2,4), DCB(2,12)
          REAL*8        AA(4,4), AAI(4,4)
          LOGICAL       FAUX
C
C     ------------------ PARAMETRAGE QUADRANGLE ------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE
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
C     ------------------------------------------------------------------
C.========================= DEBUT DU CODE EXECUTABLE ==================
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ===============
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      FAUX   = .FALSE.
C
      DO 10 I = 1, 4
        DO 10 J = 1, 8
          AM(I,J) = ZERO
          AL(I,J) = ZERO
  10  CONTINUE
C
      DO 20 I =1, 4
        DO 20 J = 1, 12
          AN(I,J) = ZERO
          AW(I,J) = ZERO
  20  CONTINUE
C
      CALL JEVETE( '&INEL.'//NOMTE(1:8)//'.DESR' ,' ',LZR )
C
      CALL GQUAD4 (XYZL , ZR(LZR))
      L(1) = ZR(LZR-1+LCOTE)
      L(2) = ZR(LZR-1+LCOTE+1)
      L(3) = ZR(LZR-1+LCOTE+2)
      L(4) = ZR(LZR-1+LCOTE+3)
      X(1) = ZR(LZR-1+LXYC)
      X(2) = ZR(LZR-1+LXYC+1)
      X(3) = ZR(LZR-1+LXYC+2)
      X(4) = ZR(LZR-1+LXYC+3)
      Y(1) = ZR(LZR-1+LXYC+4)
      Y(2) = ZR(LZR-1+LXYC+5)
      Y(3) = ZR(LZR-1+LXYC+6)
      Y(4) = ZR(LZR-1+LXYC+7)
C
C===================================================================
C --- DETERMINATION DES MATRICES AN ET AM QUI SONT TELLES QUE      = 
C --- ALPHA = AN*UN + AM*UM                                        = 
C --- POUR OBTENIR CETTE EXPRESSION ON PASSE PAR LA RELATION :     =
C --- AA*ALPHA = (AW + AB)*UN + AL*UM                              =
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)       =
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)              =
C ---  FORMELLEMENT                                                =
C ---            |L5 0  0  0|   |L5C5 L5S5|                        =
C ---   AA = 2/3*|0 L6  0  0| - |L6C6 L6S6|*DCI*(BCA - DFC_T*BFA)  =
C ---            |0  0 L7  0|   |L7C7 L7S7|                        =
C ---            |0  0  0 L8|   |L8C8 L8S8|                        =
C===================================================================
C
C --- BOUCLE SUR LES COTES DU QUADRILATERE :
C     ------------------------------------

      DO 30 IC = 1, NC
C
        DO 40 I = 1, 2
          DO 40 J = 1, 4
            DB(I,J) = ZERO
  40    CONTINUE
C
        DO 50 I = 1, 2
          DO 50 J = 1, 12
            DCIDFB(I,J) = ZERO
            DCB(I,J)    = ZERO
  50    CONTINUE
C
        DO 60 I = 1, 2
          DO 60 J = 1, 8
            DCIDMC(I,J) = ZERO
  60    CONTINUE
C
C ---   INTEGRATION SUR LE COTE COURANT :
C       -------------------------------
         DO 70 INT = 1, 2
C
C ---       INITIALISATIONS :
C           ---------------
            DO 80 I = 1, 2
              DO 80 J = 1, 4
                DFCBFA(I,J) = ZERO
  80        CONTINUE
C
            DO 90 I = 1, 2
              DO 90 J = 1, 8
                DMCTBM(I,J) = ZERO
 90         CONTINUE
C
            DO 100 I = 1, 2
              DO 100 J = 1, 12
                DFCBFB(I,J) = ZERO
 100        CONTINUE
C
            CALL JQUAD4 (NPG+NNO+2*(IC-1)+INT , XYZL , ZR(LZR))
C
C ---       CALCUL DE LA MATRICE HFT2 :
C           -------------------------
            CALL DSXHFT (DF, ZR(LZR), HFT2)
C
C ---       CALCUL DU PRODUIT HMF.T2 :
C           ------------------------
            CALL DXHMFT(DMF,ZR(LZR),HMFT2)
C
C ---       COORDONNEES DU POINT D'INTEGRATION COURANT :
C           ------------------------------------------
            QSI  = ZR(LZR-1+LQSI+NPG+NNO+2*(IC-1)+INT-1)
            ETA  = ZR(LZR-1+LETA+NPG+NNO+2*(IC-1)+INT-1)
C 
C ---       CALCUL DES MATRICES  [BCB] ET [BCA] QUI SONT TELLES QUE
C ---       D (BETA)/(DQSI*DQSI) = [TB]*BETA + [TA]*ALPHA :
C           ---------------------------------------------
            CALL DSQCI2(QSI, ETA, ZR(LZR), HFT2, HMFT2, BCB, BCA, BCM)
C
C ---      CALCUL DE LA MATRICE BFA AU POINT D'INTEGRATION COURANT  
C ---       RELIANT LES COURBURES AUX INCONNUES ALPHA 
C ---       (X = BFB*UN + BFA*ALPHA) :
C           -----------------------
            CALL DSQBFA(QSI, ETA, ZR(LZR), BFA)
C
C ---       CALCUL DU PRODUIT DFC_T*BFA :
C           ---------------------------
            DO 110 J = 1, 4
              DO 110 K = 1, 3
                DFCBFA(1,J) = DFCBFA(1,J) + DFC(K,1)*BFA(K,J)
                DFCBFA(2,J) = DFCBFA(2,J) + DFC(K,2)*BFA(K,J)
 110        CONTINUE
C
C ---       CALCUL DU PRODUIT DCI*(BCA - DFC_T*BFA) :
C           --------------------------------------
            DO 120 J = 1, 4
              DB(1,J) = DB(1,J) + DCI(1,1) * (BCA(1,J)-DFCBFA(1,J))
     +                          + DCI(1,2) * (BCA(2,J)-DFCBFA(2,J))
              DB(2,J) = DB(2,J) + DCI(2,1) * (BCA(1,J)-DFCBFA(1,J))
     +                          + DCI(2,2) * (BCA(2,J)-DFCBFA(2,J))
 120        CONTINUE
C
C================================================================
C --- DETERMINATION DE LA MATRICE AB QUI EST TELLE QUE          = 
C --- AA*ALPHA = (AW + AB)*UN + AL*UM                           =
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)    =
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)           =
C ---  FORMELLEMENT                                             =
C ---          |L5C5 L5S5|                                      =
C ---   AB = - |L6C6 L6S6|*DCI*DFC_T*BFB                        =
C ---          |L7C7 L7S7|                                      =
C ---          |L8C8 L8S8|                                      =
C================================================================
C
C ---      CALCUL DE LA MATRICE BFB RELIANT LES COURBURES AUX 
C ---      INCONNUES DE FLEXION UN (X = BFB*UN+BFA*ALPHA) :
C          ----------------------------------------------
           CALL DSQBFB(INT, ZR(LZR), BFB)
C
C ---      CALCUL DU PRODUIT DFC_T*BFB :
C          ---------------------------
           DO 130 J = 1, 12
             DO 130 K = 1, 3
               DFCBFB(1,J) = DFCBFB(1,J) + DFC(K,1)*BFB(K,J)
               DFCBFB(2,J) = DFCBFB(2,J) + DFC(K,2)*BFB(K,J)
 130       CONTINUE
C
C ---      CALCUL DU PRODUIT DCI*DFC_T*BFB :
C          -------------------------------
           DO 140 J = 1, 12
             DCIDFB(1,J) = DCIDFB(1,J) + 
     +                     DCI(1,1)*DFCBFB(1,J) + DCI(1,2)*DFCBFB(2,J)
             DCIDFB(2,J) = DCIDFB(2,J) + 
     +                     DCI(2,1)*DFCBFB(1,J) + DCI(2,2)*DFCBFB(2,J)
 140      CONTINUE
C
C===================================================================
C ---      DETERMINATION DE LA MATRICE AL QUI EST TELLE QUE        = 
C ---      AA*ALPHA = (AW + AB)*UN + AL*UM                         =
C ---      UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)  =
C ---      UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)         =
C ---      FORMELLEMENT                                            =
C ---           |L5C5 L5S5|                                        =
C ---      AL = |L6C6 L6S6|*DCI*(BCM-DMC_T*BM)                     =
C ---           |L7C7 L7S7|                                        =
C ---           |L8C8 L8S8|                                        =
C===================================================================
C
C ---     CALCUL DE LA MATRICE BM RELIANT LES DEFORMATIONS MEMBRANAIRES
C ---     AUX DEPLACEMENTS DE MEMBRANE UM (I.E. (UX,UY) ) :
C         -----------------------------------------------
          CALL DXQBM(INT, ZR(LZR), BM)
C
C ---     CALCUL DU TERME BCM-DMC_T*BM :
C         ----------------------------
          DO 150 J = 1, 8
              DMCTBM(1,J) = DMCTBM(1,J) + BCM(1,J)
              DMCTBM(2,J) = DMCTBM(2,J) + BCM(2,J)
            DO 150 K = 1, 3
              DMCTBM(1,J) = DMCTBM(1,J) - DMC(K,1)*BM(K,J)
              DMCTBM(2,J) = DMCTBM(2,J) - DMC(K,2)*BM(K,J)
 150      CONTINUE
C
C ---     CALCUL DU PRODUIT DCI*(BCM-DMC_T*BM) :
C         ------------------------------------
          DO 160 J = 1, 8
             DCIDMC(1,J) = DCIDMC(1,J) + 
     +                     DCI(1,1)*DMCTBM(1,J) + DCI(1,2)*DMCTBM(2,J)
             DCIDMC(2,J) = DCIDMC(2,J) +
     +                     DCI(2,1)*DMCTBM(1,J) + DCI(2,2)*DMCTBM(2,J)
 160      CONTINUE
C
C======================================================================
C --- DETERMINATION DE LA MATRICE AW QUI EST TELLE QUE              
C --- AA*ALPHA = (AW + AB)*UN + AL*UM                              
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)       
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)              
C ---  FORMELLEMENT                                                
C         |-2  L5C5 L5S5   2  L5C5 L5S5   0     0      0   0     0    0|
C AW=-0.5*| 0     0    0  -2  L6C6 L6S6   2  L6C6   L6S6   0     0    0|
C         | 0     0    0   0     0     0 -2  L7C7   L7S7   2 L7C7  L7S7|
C         | 2  L8C8 L8S8   0     0     0  0     0      0  -2 L8C8  L8S8|
C
C         |L5C5 L5S5|
C       + |L6C6 L6S6|*DCI*BCB 
C         |L7C7 L7S7|
C         |L8C8 L8S8|
C
C --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y :
C
C=======================================================================
C
            DO 170 J = 1, 12
             DCB(1,J) = DCB(1,J) + DCI(1,1)*BCB(1,J) + DCI(1,2)*BCB(2,J)
             DCB(2,J) = DCB(2,J) + DCI(2,1)*BCB(1,J) + DCI(2,2)*BCB(2,J)
 170        CONTINUE
C
  70    CONTINUE
C     -------------------------------------------------------------
C --  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION DU COTE COURANT
C     -------------------------------------------------------------
C
C===================================================================
C --- DETERMINATION DE LA MATRICE AA QUI EST TELLE QUE             = 
C --- AA*ALPHA = (AW + AB)*UN + AL*UM                              =
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)       =
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)              =
C ---  FORMELLEMENT                                                =
C ---            |L5 0  0  0|   |L5C5 L5S5|                        =
C ---   AA = 2/3*|0 L6  0  0| - |L6C6 L6S6|*DCI*(BCA - DFC_T*BFA)  =
C ---            |0  0 L7  0|   |L7C7 L7S7|                        =
C ---            |0  0  0 L8|   |L8C8 L8S8|                        =
C                                                                  =
C --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y                  =
C===================================================================
C
        DO 180 J = 1, 4
          AA(IC,J) = - (X(IC) * DB(1,J) + Y(IC) * DB(2,J))*UNDEMI
 180    CONTINUE
        AA(IC,IC) = AA(IC,IC) + DEUX/TROIS * L(IC)
C
C================================================================
C --- DETERMINATION DE LA MATRICE AB QUI EST TELLE QUE          = 
C --- AA*ALPHA = (AW + AB)*UN + AL*UM                           =
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)    =
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)           =
C ---  FORMELLEMENT                                             =
C ---          |L5C5 L5S5|                                      =
C ---   AB = - |L6C6 L6S6|*DCI*DFC_T*BFB                        =
C ---          |L7C7 L7S7|                                      =
C ---          |L8C8 L8S8|                                      =
C                                                               =
C --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y               =
C================================================================
C
        DO 190 J = 1, 12
          AB(IC,J) = - (X(IC)*DCIDFB(1,J) + Y(IC)*DCIDFB(2,J))*UNDEMI
 190  CONTINUE
C
C===================================================================
C ---      DETERMINATION DE LA MATRICE AL QUI EST TELLE QUE        = 
C ---      AA*ALPHA = (AW + AB)*UN + AL*UM                         =
C ---      UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)  =
C ---      UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)         =
C ---      FORMELLEMENT                                            =
C ---             |L5C5 L5S5|                                      =
C ---      AL = - |L6C6 L6S6|*DCI*DMC_T*BM                         =
C ---             |L7C7 L7S7|                                      =
C ---             |L8C8 L8S8|                                      =
C                                                                  =
C --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y                  =
C===================================================================
C
            DO 200 J = 1, 8
              AL(IC,J) = (X(IC)*DCIDMC(1,J) + Y(IC)*DCIDMC(2,J))*UNDEMI
 200     CONTINUE
C
C======================================================================
C --- DETERMINATION DE LA MATRICE AW QUI EST TELLE QUE              
C --- AA*ALPHA = (AW + AB)*UN + AL*UM                              
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)       
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)              
C ---  FORMELLEMENT                                                
C         |-2  L5C5 L5S5   2  L5C5 L5S5   0     0      0   0     0    0|
C AW=-0.5*| 0     0    0  -2  L6C6 L6S6   2  L6C6   L6S6   0     0    0|
C         | 0     0    0   0     0     0 -2  L7C7   L7S7   2 L7C7  L7S7|
C         | 2  L8C8 L8S8   0     0     0  0     0      0  -2 L8C8  L8S8|
C
C         |L5C5 L5S5|
C       + |L6C6 L6S6|*DCI*BCB 
C         |L7C7 L7S7|
C         |L8C8 L8S8|
C
C --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y :
C
C=======================================================================
C
          DO 210 J = 1, 12
             AW(IC,J) =   (X(IC)*DCB(1,J) + Y(IC)*DCB(2,J))*UNDEMI
 210      CONTINUE
C
  30  CONTINUE
C     -------------------------------------------
C --  FIN DE LA BOUCLE SUR LES COTES DE L'ELEMENT
C     -------------------------------------------
C
          AW(1,1)  = AW(1,1)  + UN
          AW(1,2)  = AW(1,2)  - X(1)/DEUX
          AW(1,3)  = AW(1,3)  - Y(1)/DEUX
          AW(1,4)  = AW(1,4)  - UN
          AW(1,5)  = AW(1,5)  - X(1)/DEUX
          AW(1,6)  = AW(1,6)  - Y(1)/DEUX
          AW(2,4)  = AW(2,4)  + UN
          AW(2,5)  = AW(2,5)  - X(2)/DEUX
          AW(2,6)  = AW(2,6)  - Y(2)/DEUX
          AW(2,7)  = AW(2,7)  - UN
          AW(2,8)  = AW(2,8)  - X(2)/DEUX
          AW(2,9)  = AW(2,9)  - Y(2)/DEUX
          AW(3,7)  = AW(3,7)  + UN
          AW(3,8)  = AW(3,8)  - X(3)/DEUX
          AW(3,9)  = AW(3,9)  - Y(3)/DEUX
          AW(3,10) = AW(3,10) - UN
          AW(3,11) = AW(3,11) - X(3)/DEUX
          AW(3,12) = AW(3,12) - Y(3)/DEUX
          AW(4,1)  = AW(4,1)  - UN
          AW(4,2)  = AW(4,2)  - X(4)/DEUX
          AW(4,3)  = AW(4,3)  - Y(4)/DEUX
          AW(4,10) = AW(4,10) + UN
          AW(4,11) = AW(4,11) - X(4)/DEUX
          AW(4,12) = AW(4,12) - Y(4)/DEUX
C
C====================================
C ---    INVERSION DE LA MATRICE AA =
C====================================
C
      DO 220 I = 1, 4
        DO 220 J = 1, 4
           AAI(I,J) = ZERO
 220  CONTINUE
      DO 230 I = 1, 4
         AAI(I,I) = UN
 230  CONTINUE
      CALL MGAUSS ( AA , AAI , 4 , 4 , 4, ZERO, FAUX )
C
C===================================================================
C --- DETERMINATION DE LA MATRICE AN QUI EST TELLE QUE             = 
C --- ALPHA = AN*UN + AM*UM                                        =
C --- SOIT AN = AAI * (AW + AB)                                    =
C --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)       =
C --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)              =
C===================================================================
C
      DO 240 I = 1, 4
         DO 240 K = 1, 4
            DO 240 J = 1, 12
               AN(I,J) = AN(I,J) + AAI(I,K) * (AW(K,J)+AB(K,J))
 240  CONTINUE
C
C===================================================================
C --- DETERMINATION DE LA MATRICE AM QUI EST TELLE QUE             = 
C --- ALPHA = AN*UN + AM*UM                                        =
C --- SOIT AM = AAI*AL                                             =
C===================================================================
C
      DO 250 I = 1, 4
         DO 250 K = 1, 4
            DO 250 J = 1, 8
               AM(I,J) = AM(I,J) + AAI(I,K) * AL(K,J)
 250  CONTINUE
C
      CALL JEDEMA()
      END
