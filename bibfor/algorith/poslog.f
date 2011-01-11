      SUBROUTINE POSLOG(RESI,RIGI,TN,TP,VFF,DFF,FM,JM,POIDS,LGPG,VIP,
     & NNO,NDIM,FP,JP,PES,AXI,G,R,DTDE,MATSYM,SIGM,
     & GN,FETA,XI,ME, SIGP,FINT,MATUU )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/01/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C TOLE CRP_21
C TOLE CRS_1404
C ----------------------------------------------------------------------
C     BUT:  POST TRAITEMENT GRANDES DEFORMATIONS 2D ET 3D LOG
C     SUIVANT ARTICLE MIEHE APEL LAMBRECHT CMAME 2002
C     CONFIGURATION LAGRANGIENNE
C ----------------------------------------------------------------------
C IN  RESI    : .TRUE. SI FULL_MECA/RAPH_MECA .FALSE. SI RIGI_MECA_TANG
C IN  RIGI    : .TRUE. SI FULL_MECA/RIGI_MECA_TANG
C IN  TN      : CONTRAINTES ASSOCIEES AUX DEF. LOGARITHMIQUES EN T-
C IN  TP      : CONTRAINTES ASSOCIEES AUX DEF. LOGARITHMIQUES EN T+
C IN  VFF     : VALEUR  DES FONCTIONS DE FORME
C IN  DFF     : DERIVEE DES FONCTIONS DE FORME
C IN  FM      : GRADIENT TRANSFORMATION EN T-
C IN  JM      : DET DU GRADIENT TRANSFORMATION EN T-
C IN  POIDS   : POIDS PT GAUSS
C IN  LGPG    : DIMENSION DU VECTEUR DES VAR. INTERNES POUR 1 PT GAUSS
C VAR VIP     : VARIABLES INTERNES EN T+
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  FP      : GRADIENT TRANSFORMATION EN T+
C IN  JP      : DET DU GRADIENT TRANSFORMATION EN T+
C IN  PES     : OPERATEUR DE TRANSFORMATION TN (OU TP) EN PK2
C IN  AXI     : .TRUE. SI AXIS
C IN  G       : NUMERO DU POINTS DE GAUSS
C IN  R       : RAYON DU POINT DE GAUSS COURANT (EN AXI)
C IN  DTDE    : OPERATEUR TANGENT ISSU DE NMCOMP (6,6)
C IN  MATSYM  : .TRUE. SI MATRICE SYMETRIQUE
C IN  SIGM    : CONTRAINTES DE CAUCHY EN T-
C IN  GN      : TERMES UTILES AU CALCUL DE TL DANS POSLOG
C IN  FETA    : TERMES UTILES AU CALCUL DE TL DANS POSLOG
C IN  XI      : TERMES UTILES AU CALCUL DE TL DANS POSLOG
C IN  ME      : TERMES UTILES AU CALCUL DE TL DANS POSLOG
C OUT SIGP    : CONTRAINTES DE CAUCHY EN T+
C OUT FINT    : FORCES INTERIEURES (RAPH_MECA ET FULL_MECA_*)
C OUT MATUU    : MATR. DE RIGIDITE NON SYM. (RIGI_MECA_* ET FULL_MECA_*)
C
      IMPLICIT NONE
      INTEGER NDIM,NNO,N,NMAX,KK,I,J,K,M,KL,J1,KKD,G,LGPG,IVTN
      REAL*8  DFF(NNO,*),VFF(NNO),DTDE(6,6),TRAV(6,6),TRAV2(6,6)
      REAL*8  PES(6,6),SIGM(2*NDIM),SIGP(2*NDIM),TP2(6),TN(6)
      REAL*8  MATUU(*), FINT(*), TMP2,TMP1, FR(3,3),VIP(LGPG)
      REAL*8  JP,R,POIDS,DSIDEP(6,6),RAC2
      REAL*8  PK2(6),JM,TP(6),FP(3,3),FM(3,3),SIG(6),EPSB(6)
      REAL*8  PE(3,3,3,3),TL(3,3,3,3),TLS(6,6)
      REAL*8  GN(3,3),FETA(4),XI(3,3),ME(3,3,3,3)
C     TABLEAUX AUTOMATIQUES. DIM MAXI : DEF(6,27,3),PFF(6,27,27)
      REAL*8  DEF(2*NDIM,NNO,NDIM),PFF(6,NNO,NNO)
      LOGICAL AXI, RESI, RIGI, MATSYM
C ---------------------------------------------------------------------
C********************CONTRAINTE ET FORCES INTERIEURES******************
        
      RAC2=SQRT(2.D0)


C     CALCUL DES PRODUITS SYMETR. DE F PAR N
      IF (RESI) THEN
        CALL DCOPY(9,FP,1,FR,1)
      ELSE
        CALL DCOPY(9,FM,1,FR,1)
      ENDIF    
      
C     CONFIG LAGRANGIENNE : COMME GROT_GDEP (ANCIENNEMENT GREEN)
C     CF. NMGR2D, NMGR3D       
      IF (NDIM.EQ.3) THEN
      
          DO 40 N=1,NNO
             DO 30 I=1,3
              DEF(1,N,I) =  FR(I,1)*DFF(N,1)
              DEF(2,N,I) =  FR(I,2)*DFF(N,2)
              DEF(3,N,I) =  FR(I,3)*DFF(N,3)
              DEF(4,N,I) = (FR(I,1)*DFF(N,2) + FR(I,2)*DFF(N,1))/RAC2
              DEF(5,N,I) = (FR(I,1)*DFF(N,3) + FR(I,3)*DFF(N,1))/RAC2
              DEF(6,N,I) = (FR(I,2)*DFF(N,3) + FR(I,3)*DFF(N,2))/RAC2
 30          CONTINUE
 40       CONTINUE

          IF (RIGI) THEN
             DO 125 N=1,NNO
                IF(MATSYM) THEN
                 NMAX = N
                ELSE
                  NMAX = NNO
                ENDIF
                DO 126 M=1,NMAX
                 PFF(1,N,M) =  DFF(N,1)*DFF(M,1)
                 PFF(2,N,M) =  DFF(N,2)*DFF(M,2)
                 PFF(3,N,M) =  DFF(N,3)*DFF(M,3)
                 PFF(4,N,M) =(DFF(N,1)*DFF(M,2)+DFF(N,2)*DFF(M,1))/RAC2
                 PFF(5,N,M) =(DFF(N,1)*DFF(M,3)+DFF(N,3)*DFF(M,1))/RAC2
                 PFF(6,N,M) =(DFF(N,2)*DFF(M,3)+DFF(N,3)*DFF(M,2))/RAC2
 126            CONTINUE
 125         CONTINUE
          ENDIF

      ELSEIF (NDIM.EQ.2) THEN
      
         CALL R8INIR(NNO*18,0.D0,DEF,1)
         DO 41 N=1,NNO
            DO 31 I=1,2
               DEF(1,N,I) =  FR(I,1)*DFF(N,1)
               DEF(2,N,I) =  FR(I,2)*DFF(N,2)
               DEF(3,N,I) =  0.D0
               DEF(4,N,I) = (FR(I,1)*DFF(N,2) + FR(I,2)*DFF(N,1))/RAC2
 31         CONTINUE
 41      CONTINUE
C 5.2.5 - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
         IF (AXI) THEN
            DO 50 N=1,NNO
               DEF(3,N,1) = FR(3,3)*VFF(N+(G-1)*NNO)/R
 50         CONTINUE
         ENDIF
         IF (RIGI) THEN
            DO 135 N=1,NNO
              IF(MATSYM) THEN
               NMAX = N
              ELSE
               NMAX = NNO
              ENDIF
              DO 136 M=1,NMAX
               PFF(1,N,M) =  DFF(N,1)*DFF(M,1)
               PFF(2,N,M) =  DFF(N,2)*DFF(M,2)
               PFF(3,N,M) = 0.D0
               PFF(4,N,M) =(DFF(N,1)*DFF(M,2)+DFF(N,2)*DFF(M,1))/RAC2
 136          CONTINUE
 135        CONTINUE
          ENDIF

      ENDIF

      IF (RESI) THEN

C        TRANSFORMATION DU TENSEUR T EN PK2          
         CALL R8INIR(6,0.D0,PK2,1)
         DO 51 I=1,6
            DO 52 J=1,6
               PK2(I)=PK2(I)+TP(J)*PES(J,I)
 52         CONTINUE
 51      CONTINUE
         
         DO 230 N=1,NNO
            DO 220 I=1,NDIM
               DO 210 K=1,2*NDIM
                  FINT(NDIM*(N-1)+I)=
     &            FINT(NDIM*(N-1)+I)+DEF(K,N,I)*PK2(K)*POIDS
 210           CONTINUE
 220        CONTINUE
 230     CONTINUE

         DO 53 I=4,2*NDIM
            PK2(I)=PK2(I)/RAC2
 53      CONTINUE
         
C        CALCUL DES CONTRAINTES DE CAUCHY, CONVERSION LAGRANGE -> CAUCHY
         CALL PK2SIG(NDIM,FP,JP,PK2,SIGP,1)

CC     --------------------------------
CC   pour gagner tu temps : stocker TP comme variable interne   
CC     --------------------------------
         IVTN=LGPG-6+1
         CALL DCOPY(2*NDIM,TP,1,VIP(IVTN),1)

      END IF

C *********************MATRICE TANGENTE(SYMETRIQUE)********************
      IF (RIGI) THEN
      
C        POUR LA RIGIDITE GEOMETRIQUE : CALCUL AVEC LES PK2
         IF (.NOT.RESI) THEN
            CALL PK2SIG(NDIM,FM,JM,PK2,SIGM,-1)
            DO 54 KL=4,2*NDIM
               PK2(KL)=PK2(KL)*RAC2
 54         CONTINUE
            DO 55 I=1,3
               TP2(I)=TN(I)
 55         CONTINUE
            DO 56 I=4,2*NDIM
               TP2(I)=TN(I)/RAC2
 56         CONTINUE
         ELSE
            DO 57 I=1,3
               TP2(I)=TP(I)
 57         CONTINUE
            DO 58 I=4,2*NDIM
               TP2(I)=TP(I)/RAC2
 58         CONTINUE
            TP2(5)=0.D0
            TP2(6)=0.D0
         ENDIF
 
         CALL DEFLOG(NDIM,FP,EPSB,PE,GN,FETA,XI,ME,1,TP2,TL )
         CALL SYMT46(TL,TLS)
         CALL DAXPY(36,1.D0,TLS,1,DSIDEP,1)
 
         CALL LCTR2M(6, PES , TRAV )
         CALL PMAT(6,TRAV,DTDE,TRAV2)
         CALL PMAT(6,TRAV2,PES,DSIDEP)
 
         IF (RESI) THEN
            CALL DAXPY(36,1.D0,TLS,1,DSIDEP,1)
         ENDIF
 
         DO 160 N=1,NNO
            DO 150 I=1,NDIM
            
               DO 151,KL=1,2*NDIM
                  SIG(KL)=0.D0
                  DO 152 K=1,2*NDIM
                     SIG(KL)=SIG(KL)+DEF(K,N,I)*DSIDEP(K,KL)
152               CONTINUE
151            CONTINUE
               IF(MATSYM) THEN
                  NMAX = N
               ELSE
                  NMAX = NNO
               ENDIF
               DO 140 J=1,NDIM
                  DO 130 M=1,NMAX
                     TMP1 = 0.D0
                     IF (I.EQ.J) THEN
                        DO 153 KL=1,2*NDIM
                           TMP1 = TMP1 + PFF(KL,N,M)*PK2(KL)
153                     CONTINUE
C                       TERME DE CORRECTION AXISYMETRIQUE
                        IF (AXI .AND. I.EQ.1) THEN
                           TMP1=TMP1+VFF(N+(G-1)*NNO)*
     &                     VFF(M+(G-1)*NNO)/(R*R)*PK2(3)
                        END IF
                     ENDIF

C     -              RIGIDITE DE COMPORTEMENT
                     TMP2=0.D0
                     DO 154 KL=1,2*NDIM
                        TMP2=TMP2+SIG(KL)*DEF(KL,M,J)
154                  CONTINUE
 
                     IF(MATSYM) THEN
C                       STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                        IF (M.EQ.N) THEN
                           J1 = I
                        ELSE
                           J1 = NDIM
                        ENDIF
                        IF (J.LE.J1) THEN
                           KKD = (NDIM*(N-1)+I-1) * (NDIM*(N-1)+I) /2
                           KK = KKD + NDIM*(M-1)+J
                           MATUU(KK) = MATUU(KK) + (TMP1+TMP2)*POIDS
                        END IF
                     ELSE
C                       STOCKAGE SANS SYMETRIE
                        KK = NDIM*NNO*(NDIM*(N-1)+I-1) + NDIM*(M-1)+J
                        MATUU(KK) = MATUU(KK) + (TMP1+TMP2)*POIDS
                     ENDIF
 130              CONTINUE
 140           CONTINUE
 150        CONTINUE
 160     CONTINUE

      END IF
      END
