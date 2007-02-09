      SUBROUTINE MMMAB1(NDIM,NNE,NNM,
     &                  INDM,INI1,INI2,INI3,CMP,
     &                  HPG,FFPC,FFPR,JACOBI,
     &                  LAMBDA,TYALGF,COEFFA,COEFFS,COEFFP,
     &                  COEFFF,TAU1,TAU2,MPROJ,MMAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2007   AUTEUR TORKHANI M.TORKHANI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      IMPLICIT NONE
      INTEGER  NDIM,NNE,NNM
      REAL*8   HPG,FFPC(9),FFPR(9),JACOBI  
      INTEGER  INDM,INI1,INI2,INI3,CMP,TYALGF 
      REAL*8   LAMBDA,COEFFF,COEFFA,COEFFS,COEFFP
      REAL*8   TAU1(3),TAU2(3)     
      REAL*8   MMAT(81,81)  
      REAL*8   MPROJ(3,3)
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0364
C ----------------------------------------------------------------------
C
C CALCUL DE B ET DE BT POUR LE CONTACT METHODE CONTINUE
C AVEC ADHERENCE
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  INDM   : NOMBRE DE NOEUDS EXCLUS PAR SANS GROUP_NO 
C IN  INI1   : NUMERO DU PREMIER NOEUD A EXCLURE PAR SANS_GROUP_NO
C IN  INI2   : NUMERO DU DEUXIEME NOEUD A EXCLURE PAR SANS_GROUP_NO
C IN  INI3   : NUMERO DU TROISIEME NOEUD A EXCLURE PAR SANS_GROUP_NO
C IN  CMP    : NUMERO DE LA TANGENTE A ELIMINER PAR SANS_GROUP_NO
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFPC   : FONCTIONS DE FORME DU POINT DE CONTACT
C IN  FFPR   : FONCTIONS DE FORME DE LA PROJECTION DU POINT DE CONTACT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  LAMBDA : VALEUR DU SEUIL_INIT
C IN  COEFFA : COEF_REGU_FROT
C IN  COEFFS : COEF_STAB_FROT
C IN  COEFFP : COEF_PENA_FROT
C IN  TYALGF : TYPE D'ALGORITHME DE FROTTEMENT
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C IN  TAU1   : PREMIERE TANGENTE
C IN  TAU2   : SECONDE TANGENTE
C IN  MPROJ  : MATRICE DE L'OPERATEUR DE PROJECTION
C I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
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
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER I,J,K,L,II,JJ
      REAL*8  E(3,3),A(3,3)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      DO 300 I = 1,3
        DO 290 J = 1,3
          A(I,J)  = 0.D00
          E(I,J)  = 0.D00
  290   CONTINUE
  300 CONTINUE         
C
C --- E : C.C
C
      DO 360 I = 1,NDIM
        DO 350 J = 1,NDIM
          DO 340 K = 1,NDIM
            E(I,J) = MPROJ(K,I)*MPROJ(K,J) + E(I,J)
  340     CONTINUE
  350   CONTINUE
  360 CONTINUE
C
C --- A : T.C
C
      DO 4  I = 1,NDIM
        DO 5  K = 1,NDIM
          A(1,I) = TAU1(K)*MPROJ(K,I) + A(1,I)
  5     CONTINUE
  4   CONTINUE
      DO 6  I = 1,NDIM
        DO 7  K = 1,NDIM
          A(2,I) = TAU2(K)*MPROJ(K,I) + A(2,I)
  7     CONTINUE
  6   CONTINUE
C      
C --- CALCUL DE B ET DE BT
C
      DO 361 I = 1,NNE
        DO 362 J = 1,NNE
          DO 363 L = 1,NDIM-1
            DO 364 K = 1,NDIM
              II = (2*NDIM)*(I-1)+NDIM+1+L
              JJ = (J-1)*(2*NDIM)+K  
              IF ((INDM.EQ.3).AND.(L.EQ.1).AND.(K.EQ.CMP))THEN
                IF (((I.EQ.INI1).OR.(I.EQ.INI2).OR.(I.EQ.INI3)).AND.
     &             (I.EQ.J))THEN
                  MMAT(II,JJ) = 0.D0
                  MMAT(JJ,II) = 0.D0
                ELSE               
                  MMAT(II,JJ) =
     &             -1.D0*LAMBDA*COEFFF*HPG*FFPC(I)*FFPC(J)*JACOBI*A(L,K)
                  MMAT(JJ,II) = MMAT(II,JJ) 
                END IF
              ELSEIF ((INDM.EQ.2).AND.(L.EQ.1).AND.(K.EQ.CMP))THEN
                IF (((I.EQ.INI1).OR.(I.EQ.INI2)).AND.(I.EQ.J)) THEN 
                  MMAT(II,JJ) = 0.D0
                  MMAT(JJ,II) = 0.D0
                ELSE
                  MMAT(II,JJ) =
     &             -1.D0*LAMBDA*COEFFF*HPG*FFPC(I)*FFPC(J)*JACOBI*A(L,K)
                  MMAT(JJ,II) = MMAT(II,JJ)
                END IF
              ELSEIF ((INDM.EQ.1).AND.(L.EQ.1).AND.(K.EQ.CMP))THEN
                IF ((I.EQ.INI1).AND.(I.EQ.J)) THEN 
                  MMAT(II,JJ) = 0.D0
                  MMAT(JJ,II) = 0.D0
                ELSE
                  MMAT(II,JJ) =
     &             -1.D0*LAMBDA*COEFFF*HPG*FFPC(I)*FFPC(J)*JACOBI*A(L,K)
                  MMAT(JJ,II) = MMAT(II,JJ)
                END IF
              ELSE
                MMAT(II,JJ) =
     &           -1.D0*LAMBDA*COEFFF*HPG*FFPC(I)*FFPC(J)*JACOBI*A(L,K)
                MMAT(JJ,II) = MMAT(II,JJ)
              END IF 
 364        CONTINUE
 363      CONTINUE
 362    CONTINUE
 361  CONTINUE
C
C --- CALCUL DE B (SECOND BLOC) ESCLAVE - MAITRE ET SA TRANSPOSEE
C 
      DO 365 I = 1,NNE
        DO 366 J = 1,NNM
          DO 367 L = 1,NDIM-1
            DO 368 K = 1,NDIM
              II = (2*NDIM)*(I-1)+NDIM+1+L
              JJ = 2*NDIM*NNE+(J-1)*NDIM+K             
              MMAT(II,JJ)=
     &          LAMBDA*COEFFF*HPG*FFPC(I)*FFPR(J)*JACOBI*A(L,K)
              MMAT(JJ,II) = MMAT(II,JJ)
 368        CONTINUE
 367      CONTINUE
 366    CONTINUE
 365  CONTINUE
C
C --- ON CALCULE LA MATRICE B_U (1ER BLOC E-E)
C
      DO 400 I = 1,NNE
        DO 390 J = 1,NNE
          DO 380 L = 1,NDIM
            DO 370 K = 1,NDIM
              II = (2*NDIM)*(I-1)+L
              JJ = (J-1)*(2*NDIM)+K           
              IF (TYALGF .EQ. 1) THEN 
                MMAT(II,JJ) =
     &          -COEFFA*COEFFF*HPG*LAMBDA*FFPC(I)*FFPC(J)*JACOBI*E(L,K)
              ELSEIF (TYALGF .EQ. 2) THEN
                MMAT(II,JJ) = 0.D0
              ELSEIF (TYALGF .EQ. 3) THEN
                MMAT(II,JJ) =
     &          -COEFFP*COEFFF*HPG*LAMBDA*FFPC(I)*FFPC(J)*JACOBI*E(L,K)
              END IF
 370        CONTINUE
 380      CONTINUE
 390    CONTINUE
 400  CONTINUE
C
C --- ON CALCULE LE SECOND BLOC DE B_U (E-M)
C
      DO 440 I = 1,NNE
        DO 430 J = 1,NNM
          DO 420 L = 1,NDIM
            DO 410 K = 1,NDIM
              II = (2*NDIM)*(I-1)+L
              JJ = (2*NDIM)*NNE+(J-1)*NDIM+K             
              IF (TYALGF .EQ. 1) THEN
                MMAT(II,JJ) =
     &          COEFFA * COEFFF * LAMBDA * HPG * FFPC(I) * FFPR(J) *
     &          JACOBI * E(L,K)
              ELSEIF (TYALGF .EQ. 2) THEN
                MMAT(II,JJ) = 0.D0
              ELSEIF (TYALGF .EQ. 3) THEN
                MMAT(II,JJ) =
     &          COEFFP * COEFFF * LAMBDA * HPG * FFPC(I) * FFPR(J) *
     &          JACOBI * E(L,K)
              END IF
 410        CONTINUE
 420      CONTINUE
 430    CONTINUE
 440  CONTINUE
C
C --- ON CALCULE LE TROISIEME  BLOC DE B_U (M-E)
C   
      DO 480 I = 1,NNM
        DO 470 J = 1,NNE
          DO 460 L = 1,NDIM
            DO 450 K = 1,NDIM
              II = (2*NDIM)*NNE+NDIM*(I-1)+L
              JJ = (2*NDIM)*(J-1)+K              
              IF (TYALGF .EQ. 1) THEN
                MMAT(II,JJ) =
     &          COEFFA * COEFFF * LAMBDA * HPG * FFPR(I) * FFPC(J) *
     &          JACOBI * E(L,K)
              ELSEIF (TYALGF .EQ. 2) THEN
                MMAT(II,JJ) = 0.D0
              ELSEIF (TYALGF .EQ. 3) THEN
                MMAT(II,JJ) =
     &          COEFFP * COEFFF * LAMBDA * HPG * FFPR(I) * FFPC(J) *
     &          JACOBI * E(L,K)
              END IF
 450        CONTINUE
 460      CONTINUE
 470    CONTINUE
 480  CONTINUE
C
C --- ON CALCULE LE QUATRIEME  BLOC DE B_U (M-M)
C 

      DO 520 I = 1,NNM
        DO 510 J = 1,NNM
          DO 500 L = 1,NDIM
            DO 490 K = 1,NDIM
              II = NNE*(2*NDIM)+NDIM*(I-1)+L
              JJ = NNE*(2*NDIM)+NDIM*(J-1)+K
              IF (TYALGF .EQ. 1) THEN
                MMAT(II,JJ) =
     &          -COEFFA*COEFFF*LAMBDA*HPG*FFPR(I)*FFPR(J)*JACOBI*E(L,K)
              ELSEIF (TYALGF .EQ. 2) THEN
                MMAT(II,JJ) = 0.D0
              ELSEIF (TYALGF .EQ. 3) THEN
                MMAT(II,JJ) =
     &          -COEFFP*COEFFF*LAMBDA*HPG*FFPR(I)*FFPR(J)*JACOBI*E(L,K)
              END IF
 490        CONTINUE
 500      CONTINUE
 510    CONTINUE
 520  CONTINUE
C
C --- POUR TRAITEMENT DE LA REDONDANCE CONDITIONS DE FROTTEMENT/SYMETRIE
C 
      IF (INDM.EQ.2) THEN
        DO 931 I = 1,NNE
          DO 932 J = 1,NNE
            DO 933 L = 1,NDIM-1
              DO 934 K = 1,NDIM-1
                II = 2*NDIM*(I-1)+NDIM+1+L
                JJ = 2*NDIM*(J-1)+NDIM+1+K
                IF ((K.EQ.L).AND.(I.EQ.J).AND.(K.EQ.1)) THEN
                  IF ((I.EQ.INI1).OR.(I.EQ.INI2)) THEN
                    MMAT(II,JJ) = 1.D0
                  END IF
                END IF
 934          CONTINUE
 933        CONTINUE
 932      CONTINUE
 931    CONTINUE
      ELSEIF (INDM.EQ.3) THEN
        DO 941 I = 1,NNE
          DO 942 J = 1,NNE
            DO 943 L = 1,NDIM-1
              DO 944 K = 1,NDIM-1
                II = 2*NDIM*(I-1)+NDIM+1+L
                JJ = 2*NDIM*(J-1)+NDIM+1+K
                IF((K.EQ.L).AND.(I.EQ.J).AND.(K.EQ.1))THEN
                  IF((I.EQ.INI1).OR.(I.EQ.INI2).OR.(I.EQ.INI3))THEN
                    MMAT(II,JJ) = 1.D0
                  END IF
                END IF
 944          CONTINUE
 943        CONTINUE
 942      CONTINUE
 941    CONTINUE
      ELSEIF (INDM.EQ.1) THEN
        DO 935 I = 1,NNE
          DO 936 J = 1,NNE
            DO 937 L = 1,NDIM-1
              DO 938 K = 1,NDIM-1
                II = 2*NDIM*(I-1)+NDIM+1+L
                JJ = 2*NDIM*(J-1)+NDIM+1+K
                IF((K.EQ.L).AND.(I.EQ.J).AND.(K.EQ.1)) THEN
                  IF(I.EQ.INI1) THEN
                    MMAT(II,JJ) = 1.D0
                  END IF
                END IF
 938          CONTINUE
 937        CONTINUE
 936      CONTINUE
 935    CONTINUE  
      END IF
C
      CALL JEDEMA()      
      END
