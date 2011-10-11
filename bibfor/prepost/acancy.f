      SUBROUTINE ACANCY(NBORDR, KWORK, SOMPGW, JRWORK, TSPAQ, IPG, C1,
     &                   C2, ORDINI, ORDFIN, NBCYAD, CYFERM, EPSPAC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 10/10/2011   AUTEUR TRAN V-X.TRAN 
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
C TOLE CRS_512 CRS_1404
      IMPLICIT     NONE
      INTEGER      NBORDR, KWORK, SOMPGW, JRWORK, TSPAQ, IPG 
      INTEGER      ORDINI, ORDFIN, NBCYAD
      LOGICAL      CYFERM
      REAL*8       C1, C2, EPSPAC(NBORDR+1)


C ---------------------------------------------------------------------
C BUT: POUR AMPLITUDE CONSTANTE, ON ANALYSE LE CYCLE AFIN DE DECOMPOSER
C      LE CHARGEMENT MONOTONIQUE (POUR RAMEMER A UNE VALEUR MOYENNE)
C      ET LE CHARGEMENT CYCLIQUE
C
C REMARQUE: CETTE SUBROUTINE EST POUR CHARGEMENT PROPORTIONEL
C ---------------------------------------------------------------------
C ARGUMENTS:
C    NBORDR : IN  : NOMBRE DE NUMEROS D'ORDRE.
C    KWORK  : IN  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET DE
C                             MAILLES ;
C                   KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
C                             MAILLES.
C    SOMPGW : IN  : SOMME DES POINTS DE GAUSS DES N MAILLES PRECEDANT 
C                   LA MAILLE COURANTE.
C    JRWORK : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT 
C                   L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                   ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                   DU <<PAQUET>> DE MAILLES.
C    TSPAQ  : IN  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                   COURANT.
C    IPG    : IN  : IEME POINT DE GAUSS.
C    C1     : IN : COEEFS D'ELASTICITE
C    C2     : IN : COEEFS D'ELASTICITE
C    ORNINI : OUT : ORDRE INITIAL POUR LE CHARGEMENT CYCLIQUE
C    ORDFIN : OUT : ORDRE FINAL POUR LE CHARGEMENT CYCLIQUE
C    NBCYAD : OUT : NOMBRE DE CYCLE STABILISE
C    CYFERM : OUT : 'TRUE' -->CYCLE FERME, 'FALSE' SINON
C    EPSPAC : OUT : DEFORMATION PLASTIQUE ACCUMULE SUR UN CYCLE
C---------------------------------------------------------------------
C---- COMMUNS NORMALISES  JEVEUX
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
      INTEGER    NPERM,ITYPE,IORDRE, NVP, NITJAC
      INTEGER    I,J,K, DECAL, ADR
      REAL*8     SIG(6), EPS(6),EPSE(6),EPSP(6), AR(6), BR(6)
      REAL*8     SIGINI(6),SIGFIN(6), TOL, TOLDYN
      REAL*8     VECPRO(3,3),VALPRO(3),JACAUX(3)
      REAL*8     SIGPR1(NBORDR+1),SEPFIN, SEPINI
      
C----------------------------------------------------------------------
C INITIALISATION
C
C     
      ORDINI = 1
      ORDFIN = 1
      NBCYAD = 0
      
      DO 35 I = 1, 6
         SIG(I) = 0.0D0
         EPS(I) = 0.0D0
         EPSE(I)= 0.0D0
         EPSP(I)= 0.0D0
35    CONTINUE 
      DO 45 I = 1, NBORDR+1
         EPSPAC(I) = 0.D0
         SIGPR1(I) = 0.D0
45    CONTINUE 

      CYFERM = .TRUE.
      
C PARAMETRES POUR CALCULER LES CONTRAINTES PRINCIPLAES   
      NVP = 3
      NPERM = 12
      TOL = 1.D-10
      TOLDYN = 1.D-2
      ITYPE = 0
      IORDRE = 1
                  
      DO 10 J=1, NBORDR
         DECAL = 12
C         ADR = (J-1)*TSPAQ+KWORK*SOMPGW*6+(IPG-1)*6

         ADR = (J-1)*TSPAQ+KWORK*SOMPGW*DECAL+(IPG-1)*DECAL
         
         CALL TENEPS( JRWORK,ADR, C1, C2, SIG, EPS, EPSE, EPSP)
         
C CALCULER LA DEFORMATION PLASTIQUE ACUUMULE         
         DO 15 K=1,6
            EPSPAC(J) = EPSPAC(J) + ABS(EPSP(K))         
15       CONTINUE

         AR(1) = SIG(1)
         AR(2) = SIG(4)
         AR(3) = SIG(5)
         AR(4) = SIG(2)
         AR(5) = SIG(6)
         AR(6) = SIG(3)
         BR(1) = 1.D0
         BR(2) = 0.D0
         BR(3) = 0.D0
         BR(4) = 1.D0
         BR(5) = 0.D0
         BR(6) = 1.D0      
        
         CALL JACOBI(NVP,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,
     &                    JACAUX,NITJAC,ITYPE,IORDRE)
         SIGPR1(J) = VALPRO(1)

10    CONTINUE       
   
      DO 20 J=1, (NBORDR-1)   
          
         IF (ABS(SIGPR1(J+1)) .LT. ABS(SIGPR1(J))) THEN
            ORDINI = J
            SEPINI = EPSPAC(J) 
            GOTO 25    
         ENDIF 
20    CONTINUE 

25    CONTINUE 

C  AJOUTER UN ORDRE OVERLAP
      SIGPR1(NBORDR+1) = SIGPR1(ORDINI+1)
      EPSPAC(NBORDR+1) = EPSPAC(ORDINI+1)
            
      DO 30 J=ORDINI, NBORDR  
         DECAL = 12
C         ADR = (J-1)*TSPAQ+KWORK*SOMPGW*6+(IPG-1)*6

         ADR = (J-1)*TSPAQ+KWORK*SOMPGW*DECAL+(IPG-1)*DECAL
         
         CALL TENEPS( JRWORK,ADR, C1, C2, SIG, EPS, EPSE, EPSP)
         
         IF (J .EQ. ORDINI) THEN
            DO 22 I = 1, 6
               SIGINI(I) = SIG(I)
22          CONTINUE 
         ENDIF
                   
C          IF (ABS(SIG_1(J+1)) .GT. ABS(SIG_1(J))) THEN
C             SEPITE = EPSPAC(J)          
C          ENDIF 
      
         IF (ABS(SIGPR1(J+1)) .LT. ABS(SIGPR1(J))) THEN
             ORDFIN = J
             SEPFIN = EPSPAC(J)
             NBCYAD = NBCYAD + 1  
             DO 32 I = 1, 6
                SIGFIN(I) = SIG(I)
32           CONTINUE     
         ENDIF         
       
30    CONTINUE 
    
C ALMARME ET ERROR
      
C       IF ((ORDFIN .NE. NBORDR) .AND. (NBCYAD .NE.1)) THEN
C          CALL U2MESS('F','FATIGUE1_95')
C       ENDIF
      
      TOL = 1.D-6
      TOL = TOL*ABS(SIGINI(I))
      
      DO 55 I = 1, 6
         IF  ((ABS(SIGINI(I) - SIGFIN(I)) .GT. TOL) 
     &    .AND. (SEPFIN .GT. 1.D-6) )  THEN
            CYFERM = .FALSE.
            CALL U2MESS('A','FATIGUE1_96')
         ENDIF     
55    CONTINUE       
             
      END
