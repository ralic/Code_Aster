      SUBROUTINE XMMCO2(NDIM  ,NNO   ,NNOS  ,NNOL  ,DDLS  ,
     &                  DDLM  ,DSIDEP,P     ,R     ,NFH   ,
     &                  JAC   ,FFP   ,FFC   ,PLA   ,SINGU ,
     &                  NFISS ,JHEAFA,JFISNO,IFA   ,NCOMPH,
     &                  IFISS ,RR    ,MMAT)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER     NDIM,NNO,NFH,DDLS,SINGU
      REAL*8      MMAT(216,216),DSIDEP(6,6)
      REAL*8      FFP(27),JAC
      REAL*8      P(3,3),RR
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
C
C --- CALCUL DES MATRICES DE COHESION
C ELEMENT COHESIF MIXTE
C
C ----------------------------------------------------------------------
C
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
C IN  DSIDEP : MATRICE TANGENTE BASE LOCALE
C IN  PP     :
C IN  P      : MATRICE PROJECTION PLAN TANGENT
C IN  ND     : DIRECTION NORMALE
C IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
C IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
C IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
C IN  FFP    : FONCTIONS DE FORME DE L'ELEMENT PARENT
C IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
C IN  RR     : DISTANCE AU FOND DE FISSURE
C IN  TAU1   : PREMIERE DIRECTION TANGENTE
C IN  AM     :
C I/O MMAT   : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT
C
C
      INTEGER I,J,DDLM,IFA,IFH,IFISS,IN,JFH,JFISNO,JHEAFA,JN
      INTEGER K,L,NCOMPH,NFISS,NNOL,NNOS,PLA(27),PLI,PLJ
      REAL*8  AU(3,3),COEFI,COEFJ,DSIDE2(3,3),FFC(8),PDOTAL(3,3)
      REAL*8  FFI,FFJ,R,TEMP(3,3),UNITY(3,3),PTR(3,3),ALOCAL(3,3)
      LOGICAL LMULTC
      
C
C ----------------------------------------------------------------------
C
C     INITIALISATIONS
C
      CALL MATINI(3,3,0.D0,UNITY)
      CALL MATINI(3,3,0.D0,ALOCAL)
      CALL MATINI(3,3,0.D0,PTR)
      CALL MATINI(3,3,0.D0,PDOTAL)
      CALL MATINI(3,3,0.D0,AU)
      CALL MATINI(3,3,0.D0,DSIDE2)
      CALL MATINI(3,3,0.D0,TEMP)
C
C   MATRICE -ID+R DSIDEP
C
      DO 3 I=1,NDIM
         UNITY(I,I) = 1.D0
3     CONTINUE
C
      DO 1 I=1,NDIM
         DO 2 J=1,NDIM
           DSIDE2(I,J) = DSIDEP(I,J)
           ALOCAL(I,J) = -UNITY(I,J) + R*DSIDE2(I,J)
2        CONTINUE
1     CONTINUE
C
C MATRICE [P]T[ALOCAL]
C
      CALL TRANSP(P,3,NDIM,NDIM,PTR,3)
C
      CALL PROMAT(PTR,3,NDIM,NDIM,
     &            ALOCAL,3,NDIM,NDIM,PDOTAL)
C
C MATRICE TANGENTE EN BASE FIXE [P]T [DSIDEP] [P]
C
      CALL PROMAT(PTR,3,NDIM,NDIM,
     &            ALOCAL,3,NDIM,NDIM,TEMP)
      CALL PROMAT(TEMP,3,NDIM,NDIM,
     &            P,3,NDIM,NDIM,AU)
C
C ON STOCKE DANS LA MATRICE ELEMENTAIRE
C
      COEFI = 2
      COEFJ = 2
      LMULTC = NFISS.GT.1
C
      DO 10 I = 1,NNOL
C
        PLI=PLA(I)
        FFI=FFC(I)
C
        DO 11 K=1,NDIM
C
          DO 20 J = 1,NNO
            CALL INDENT(J,DDLS,DDLM,NNOS,JN)
            DO 21 JFH = 1,NFH
              IF (LMULTC) THEN
                COEFJ = ZI(JHEAFA-1+NCOMPH*(NFISS*(IFISS-1)
     &                  +ZI(JFISNO-1+NFH*(J-1)+JFH)-1)+2*IFA)
     &              - ZI(JHEAFA-1+NCOMPH*(NFISS*(IFISS-1)
     &                  +ZI(JFISNO-1+NFH*(J-1)+JFH)-1)+2*IFA-1)
              ENDIF
            DO 22 L = 1,NDIM
C
C INDICES INVERSES MATRICE INTERFACE
C
              MMAT(PLI-1+K,JN+NDIM*JFH+L)=MMAT(PLI-1+K,JN+NDIM*JFH+L)
     &        - COEFJ * FFI * FFP(J) * PDOTAL(L,K) * JAC
C
C INDICES MEME ORDRE MATRICE EQUILIBRE
C
              MMAT(JN+NDIM*JFH+L,PLI-1+K)=MMAT(JN+NDIM*JFH+L,PLI-1+K)
     &        - COEFJ * FFI * FFP(J) * PDOTAL(L,K) * JAC
 22        CONTINUE
C
 21      CONTINUE
          DO 23 L = 1,SINGU*NDIM
            MMAT(PLI-1+K,JN+NDIM*(1+NFH)+L) = 
     &        MMAT(PLI-1+K,JN+NDIM*(1+NFH)+L)
     &      - COEFJ * FFI * FFP(J) * RR * PDOTAL(L,K) * JAC
C
            MMAT(JN+NDIM*(1+NFH)+L,PLI-1+K)= 
     &       MMAT(JN+NDIM*(1+NFH)+L,PLI-1+K)
     &      - COEFJ * FFI * FFP(J) * RR * PDOTAL(L,K) * JAC
 23      CONTINUE

 20    CONTINUE
 11    CONTINUE
 10    CONTINUE
C
C -- MATRICE VENANT S AJOUTER A LA RAIDEUR
C
      DO 140 I = 1,NNO
        CALL INDENT(I,DDLS,DDLM,NNOS,IN)
        DO 141 J = 1,NNO
          CALL INDENT(J,DDLS,DDLM,NNOS,JN)
          DO 148 IFH = 1,NFH
            IF (LMULTC) THEN
              COEFI = ZI(JHEAFA-1+NCOMPH*(NFISS*(IFISS-1)
     &                  +ZI(JFISNO-1+NFH*(I-1)+IFH)-1)+2*IFA)
     &              - ZI(JHEAFA-1+NCOMPH*(NFISS*(IFISS-1)
     &                  +ZI(JFISNO-1+NFH*(I-1)+IFH)-1)+2*IFA-1)
            ENDIF
            DO 149 JFH = 1,NFH
              IF (LMULTC) THEN
                COEFJ = ZI(JHEAFA-1+NCOMPH*(NFISS*(IFISS-1)
     &                    +ZI(JFISNO-1+NFH*(J-1)+JFH)-1)+2*IFA)
     &                - ZI(JHEAFA-1+NCOMPH*(NFISS*(IFISS-1)
     &                    +ZI(JFISNO-1+NFH*(J-1)+JFH)-1)+2*IFA-1)
              ENDIF
              DO 142 K = 1,NDIM
                DO 143 L = 1,NDIM
                  MMAT(IN+NDIM*IFH+K,JN+NDIM*JFH+L) =
     &            MMAT(IN+NDIM*IFH+K,JN+NDIM*JFH+L) -
     &            COEFI*COEFJ*R*AU(K,L)*FFP(I)*FFP(J)*JAC
 143            CONTINUE
C
                DO 144 L = 1,SINGU*NDIM
                  MMAT(IN+NDIM+K,JN+NDIM*(1+NFH)+L) =
     &            MMAT(IN+NDIM+K,JN+NDIM*(1+NFH)+L) -
     &            4.D0*FFP(I)*FFP(J)*RR*R*AU(K,L)*JAC
 144            CONTINUE
 142          CONTINUE
 149        CONTINUE
 148      CONTINUE

          DO 145 K = 1,SINGU*NDIM
            DO 146 L = 1,NFH*NDIM
              MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM+L) =
     &        MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM+L) -
     &        4.D0*FFP(I)*FFP(J)*RR*R*AU(K,L)*JAC
 146        CONTINUE
C
            DO 147 L = 1,SINGU*NDIM
              MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM*(1+NFH)+L) =
     &        MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM*(1+NFH)+L) -
     &        4.D0*FFP(I)*FFP(J)*RR*RR*R*AU(K,L)
     &        *JAC
 147        CONTINUE
 145      CONTINUE

 141    CONTINUE
 140   CONTINUE
C
C -- MATRICE D INTERFACE : EXPRESSION DIRECTE
C
      DO 30 I = 1,NNOL
C
          PLI=PLA(I)
          FFI=FFC(I)
          DO 31 K=1,NDIM
C
          DO 40 J = 1,NNOL
C
            PLJ=PLA(J)
            FFJ=FFC(J)
            DO 41 L=1,NDIM
C
            MMAT(PLI-1+K,PLJ-1+L) = MMAT(PLI-1+K,PLJ-1+L)
     &               - FFJ * DSIDE2(K,L)*FFI * JAC
 41         CONTINUE
 40        CONTINUE
 31       CONTINUE
 30      CONTINUE
      END
