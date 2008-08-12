      SUBROUTINE SH6FOR(XETEMP,PARA,XIDEPM,SIGMA,XIVECT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/08/2008   AUTEUR DESROCHES X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C               ELEMENT SHB6
C
      IMPLICIT REAL *8(A-H,O-Z)
      INTEGER LAG
      REAL*8 EYG(5),ENU(5),PARA(*)
      REAL*8 XIVECT(*),XETEMP(*)
      REAL*8 XE(18),XIDEPM(*),SIGMA(*),XE1(3,6),XE2(3,6)
      REAL*8 XCOQ(3,3),BKSIP(3,6,5),SIGMAG(6)
      REAL*8 XCENT(3),PPP(3,3),PPPT(3,3),B(3,6)
      REAL*8 XL(3,3),XXX(3),YYY(3),XELOC(18)
      REAL*8 TMPKE(18,18),TMPKE2(18,18)
      REAL*8 XXG5(5),PXG5(5),FTEMP(18),BLOC(6,18),BLOCAL(3,6)
      REAL*8 SIGLOC(6),SITMP1(6,6),SITMP2(6,6),XMODIF(18)
      REAL*8 F(3,6),FQ(18),FLOC(3,6),FGLOB(3,6)
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

C INFOS:
C XE EST RANGE COMME CA: (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2
C... ZNOEUD2)      
C DANS SHB15_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C ON DEFINI LES POINTS GAUSS ET LES POIDS
C
      XXG5(1) = -0.906179845938664D0
      XXG5(2) = -0.538469310105683D0
      XXG5(3) =  0.D0
      XXG5(4) =  0.538469310105683D0
      XXG5(5) =  0.906179845938664D0
C
      PXG5(1) =  0.236926885056189D0
      PXG5(2) =  0.478628670499366D0
      PXG5(3) =  0.568888888888889D0
      PXG5(4) =  0.478628670499366D0
      PXG5(5) =  0.236926885056189D0
C
C     ON FAIT UNE COPIE DE XETEMP DANS XE
      DO 10 I = 1,18
         XE(I) = XETEMP(I)
   10 CONTINUE
C
      LAG = PARA(6)
      CALL R8INIR(36,0.D0,SITMP2,1)
      DO 30 J=1,6
         DO 20 I=1,3
           F(I,J) = 0.D0
 20      CONTINUE
 30   CONTINUE
CC
CC CALCUL DE BKSIP(3,15,IP) DANS REPERE DE REFERENCE
CC      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
CC      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
CC      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
CC
      CALL SH6KSI(5,XXG5,BKSIP)
C
      DO 170 IP=1,5
CC
CC RECHERCHE DE SIGMA DU POINT DE GAUSS GLOBAL
CC
         DO 40 I=1,6
            SIGLOC(I)=SIGMA((IP-1)*6+I)
  40     CONTINUE
         ZETA  = XXG5(IP)
         ZLAMB = 0.5D0*(1.D0-ZETA)    
         DO 60 I=1,3
            DO 50 J=1,3
               XCOQ(J,I) = ZLAMB*XE((I-1)*3+J) 
     &             + (1.D0-ZLAMB)*XE(3*I+6+J)
  50        CONTINUE
  60     CONTINUE
         CALL RLOSH6(XCOQ,XCENT,PPP,XL,XXX,YYY,RBID)
CC
CC PASSAGE DES CONTRAINTES AU REPERE GLOBAL
CC	 
         DO 80 I = 1,3
            DO 70 J = 1,3
              PPPT(J,I) = PPP(I,J)
  70        CONTINUE
  80     CONTINUE
C Passer les coordonnées globaux XE dans le repère local :
         DO 81 I = 1,6
              XE1(1,I) = XE(3*(I-1)+1)
              XE1(2,I) = XE(3*(I-1)+2)
              XE1(3,I) = XE(3*(I-1)+3)
  81     CONTINUE
         CALL MULMAT(3,3,6,PPPT,XE1,XE2)
         DO 82 I = 1,6
              XELOC(3*(I-1)+1) = XE2(1,I)
              XELOC(3*(I-1)+2) = XE2(2,I)
              XELOC(3*(I-1)+3) = XE2(3,I)
  82     CONTINUE

C
C CALCUL DE B : U_GLOBAL ---> EPS_GLOBAL
         CALL S6CALB(BKSIP(1,1,IP),XELOC,BLOCAL,AJAC)
C
C Transformer matrice BLOCAL(3,6) dans le repère local en BLOC(6,18) 
C  dans le repère local et en tenant
C compte également des modifications sur les termes croisés ZY,ZX :
         DATA XMODIF/1.D0,0.D0,0.D0,
     &          0.D0,1.D0,0.D0,
     &          0.D0,0.D0,1.D0,
     &          1.D0,1.D0,0.D0,
     &          0.D0,0.45D0,0.45D0,
     &          0.45D0,0.D0,0.45D0/
         CALL ASSEBG(BLOC,BLOCAL,XMODIF)
CC
CC CALCUL DE B.SIGMA EN GLOBAL
CC
         POIDS = 0.5D0*PXG5(IP)*AJAC
         CALL R8INIR(18,0.D0,FTEMP,1)
         DO 140 J=1,18
            DO 130 I=1,6
              FTEMP(J)= FTEMP(J)+BLOC(I,J)*SIGLOC(I)*POIDS
 130        CONTINUE
 140     CONTINUE
         DO 150 I=1,6
           FLOC(1,I)=FTEMP(I)
           FLOC(2,I)=FTEMP(I+6)
           FLOC(3,I)=FTEMP(I+12)
 150     CONTINUE
C
C Transformer FLOC(3,6) dans le repère global :
C
         CALL MULMAT(3,3,6,PPP,FLOC,FGLOB)
         DO 160 K=1,6
            F(1,K) = F(1,K) + FGLOB(1,K)
            F(2,K) = F(2,K) + FGLOB(2,K)
            F(3,K) = F(3,K) + FGLOB(3,K)
 160     CONTINUE
 170   CONTINUE
CC
CC SI LAGRANGIEN TOTAL: AJOUT DE FQ A F
CC  
       IF (LAG.EQ.1) THEN
         CALL R8INIR(324,0.D0,TMPKE,1)
         DO 490 KK=1,3
            DO 480 I=1,6
               DO 470 J=1,6
                  TMPKE(I+(KK-1)*6,J+(KK-1)*6) = SITMP2(I,J)
  470          CONTINUE
  480       CONTINUE
  490    CONTINUE
         CALL R8INIR(324,0.D0,TMPKE2,1)
         DO 510 J=1,6
            DO 500 I=1,18
               TMPKE2(I,(J-1)*3+1)=TMPKE(I,J)
               TMPKE2(I,(J-1)*3+2)=TMPKE(I,J+6)
               TMPKE2(I,(J-1)*3+3)=TMPKE(I,J+12)
  500       CONTINUE
  510    CONTINUE
         CALL R8INIR(324,0.D0,TMPKE,1)
         DO 530 I=1,6
            DO 520 J=1,18
               TMPKE((I-1)*3+1,J)=TMPKE2(I,J)
               TMPKE((I-1)*3+2,J)=TMPKE2(I+6,J)
               TMPKE((I-1)*3+3,J)=TMPKE2(I+12,J)
  520       CONTINUE
  530    CONTINUE
         CALL MULMAT(18,18,1,TMPKE,XIDEPM,FQ)
         DO 540 K=1,6
            F(1,K) = F(1,K) + FQ((K-1)*3+1)
            F(2,K) = F(2,K) + FQ((K-1)*3+2)
            F(3,K) = F(3,K) + FQ((K-1)*3+3)
  540    CONTINUE
       END IF
CC
CC ATTENTION A L'ORDRE DE XIVECT
CC      
       DO 560 I=1,3
         DO 550 J=1,6
            XIVECT((J-1)*3+I) = F(I,J)
  550    CONTINUE
  560  CONTINUE
       END
