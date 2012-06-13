      SUBROUTINE PTFORP ( ITYPE,OPTION,NOMTE,A,A2,XL,RAD,ANGS2,IST,
     &                    NNO, NC, PGL, PGL1, PGL2, FER, FEI)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      INTEGER           ITYPE,IST,NNO,NC

      CHARACTER*(*)     OPTION,NOMTE
      REAL*8            PGL(3,3),PGL1(3,3),PGL2(3,3),FER(*),FEI(*)
      REAL*8            A,A2,XL,RAD,ANGS2
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C TOLE
C
      REAL*8       RHO,  COEF1,  COEF2, S, S2 , S4 , S3 , S5
      REAL*8       ZERO, UN, XXX, R8MIN, R8BID,G
      REAL*8       U(3), V(3), W(8), W2(3),DW(12),TET1,TET2
      REAL*8       Q(12), QQ(12), QQR(12), QQI(12),PTA(3),DIR(3)
      REAL*8       DIR1(3),DIR2(3),D1,D2,OMEG2,X1(3),X2(3),V1(3),V2(3)
      INTEGER ICODRE
      CHARACTER*8  NOMPAR(4)
      CHARACTER*16 CH16
      LOGICAL      GLOBAL, NORMAL

      INTEGER      IFCX,I,J,NNOC,NCC,LX,IORIEN,IDEPLA,IDEPLP,LMATE,LPESA
      INTEGER      LFORC,ITEMPS,NBPAR,IER,IRET,ICOER,ICOEC,IRETR,IRETC
      INTEGER      LROTA
      CHARACTER*8  NOMPAV(1)
      REAL*8       VALPAV(1),FCX,VITE2,VP(3),ANGLE(3),CASECT(6)
      LOGICAL      OKVENT
      REAL*8  DIMAG,DDOT,R8MIEM
C     ------------------------------------------------------------------
      DATA         NOMPAR/'X','Y','Z','INST'/
      DATA         NOMPAV/'VITE'/
C     ------------------------------------------------------------------
C     --- INITIALISATION  ---
C
      R8MIN = R8MIEM()
      ZERO = 0.0D0
      UN   = 1.0D0
      DO 1 I =1,12
         Q(I)   = ZERO
         QQ(I)  = ZERO
         FER(I) = ZERO
         FEI(I) = ZERO
  1   CONTINUE
      DO 2 I =1,3
         VP(I)   = ZERO
  2   CONTINUE
      NNOC = 1
      NCC  = 6
      GLOBAL = .FALSE.

C
C *********************************************************************
C
      CALL JEVECH('PGEOMER', 'L',LX)
      LX = LX-1

      CALL JEVECH('PCAORIE','L',IORIEN)

      IF ( OPTION .EQ. 'CHAR_MECA_SR1D1D' .OR.
     &     OPTION .EQ. 'CHAR_MECA_SF1D1D' ) THEN
         IF (NOMTE .EQ. 'MECA_POU_C_T') GOTO 998
         CALL JEVECH('PDEPLMR','L',IDEPLA)
         CALL JEVECH('PDEPLPR','L',IDEPLP)
         DO 8 I = 1 , 12
            DW(I)   = ZR(IDEPLA-1+I) + ZR(IDEPLP-1+I)
8        CONTINUE
         DO 10 I = 1 , 3
            W(I)   = ZR(LX+I)   + DW(I)
            W(I+4) = ZR(LX+I+3) + DW(I+6)
            W2(I)  = W(I+4) - W(I)
10       CONTINUE
         CALL ANGVX(W2,ANGLE(1),ANGLE(2))
         S=DDOT(3,W2,1,W2,1)
         XL = SQRT(S)
         TET1=DDOT(3,DW(4),1,W2,1)
         TET2=DDOT(3,DW(10),1,W2,1)
         ANGLE(3) = ZR(IORIEN+2) + (TET1 + TET2)/XL/2.0D0
         CALL MATROT ( ANGLE , PGL )
      ELSE
         DO 15 I = 1 , 3
            W(I)   = ZR(LX+I)
            W(I+4) = ZR(LX+I+3)
            W2(I)  = W(I+4) - W(I)
15       CONTINUE
      ENDIF
C
C *********************************************************************
C
      IF ( OPTION.EQ.'CHAR_MECA_PESA_R'
     &     .OR.OPTION.EQ.'CHAR_MECA_ROTA_R') THEN
C
         IF ( OPTION.EQ.'CHAR_MECA_PESA_R') THEN
           CALL JEVECH('PPESANR','L',LPESA)
           G=ZR(LPESA-1+1)
C          -- DIR EST NORME :
           DIR(1)=ZR(LPESA-1+2)
           DIR(2)=ZR(LPESA-1+3)
           DIR(3)=ZR(LPESA-1+4)
         ELSE IF ( OPTION.EQ.'CHAR_MECA_ROTA_R')  THEN
           CALL JEVECH('PROTATR','L',LROTA)
           OMEG2=ZR(LROTA-1+1)**2
           DO 778, K=1,3
             DIR(K)=ZR(LROTA-1+1+K)
             PTA(K)=ZR(LROTA-1+4+K)
             X1(K)=ZR(LX+K) - PTA(K)
             X2(K)=ZR(LX+3+K) - PTA(K)
778        CONTINUE
           CALL PROVEC(DIR,X1,V1)
           CALL PROVEC(DIR,X2,V2)
           CALL NORMEV(V1,D1)
           CALL NORMEV(V2,D2)
           CALL PROVEC(V1,DIR,DIR1)
           CALL PROVEC(V2,DIR,DIR2)
         ENDIF

         CALL JEVECH('PMATERC','L',LMATE)
         IF(NOMTE(1:13).EQ.'MECA_POU_D_EM')THEN
            IF (IST.EQ.1)THEN
              CALL PMFITX(ZI(LMATE),2,CASECT,R8BID)
            ELSE
              CALL PMFITX(ZI(LMATE),3,CASECT,R8BID)
            ENDIF
            RHO=CASECT(1)
            COEF1=1.D0
            COEF2=1.D0
         ELSE
           IF ( IST .EQ. 1 ) THEN
             CALL RCVALA(ZI(LMATE),' ','ELAS',0,' ',R8BID,1,
     &                              'RHO',RHO,ICODRE, 1)
           ELSE
             CALL RCVALA(ZI(LMATE),' ','ELAS',0,' ',R8BID,1,
     &                              'RHO',RHO,ICODRE, 0)
             IF (ICODRE.NE.0    ) RHO = ZERO
           ENDIF
C          ---A CAUSE DES CHARGEMENTS VARIABLE ---
           COEF1 = A
           COEF2 = A2
         ENDIF
C
         DO 20 I=1,3
           IF ( OPTION.EQ.'CHAR_MECA_PESA_R') THEN
             Q(I)   = RHO *  G * DIR(I)
             Q(I+6) = RHO *  G * DIR(I)
           ELSEIF ( OPTION.EQ.'CHAR_MECA_ROTA_R') THEN
             Q(I)   = RHO *  OMEG2 * D1 * DIR1(I)
             Q(I+6) = RHO *  OMEG2 * D2 * DIR2(I)
           ENDIF
20       CONTINUE
C
C        ---UN CAS DE CHARGE DE PESANTEUR SE PASSE EN REPERE GLOBAL ---
C        --- PASSAGE REPERE LOCAL DU VECTEUR FORCE ---
         IF ( NOMTE(1:12) .EQ. 'MECA_POU_C_T' ) THEN
            CALL UTPVGL ( NNOC, NCC, PGL1, Q(1), QQ(1) )
            CALL UTPVGL ( NNOC, NCC, PGL2, Q(7), QQ(7) )
         ELSE
            CALL UTPVGL ( NNO, NC, PGL, Q(1), QQ(1) )
         ENDIF
C
         GLOBAL = .TRUE.
C
         GOTO 777
      ENDIF
C
C *********************************************************************
C
      OKVENT = .FALSE.
      IF ( OPTION .EQ. 'CHAR_MECA_FR1D1D' .OR.
     &     OPTION .EQ. 'CHAR_MECA_SR1D1D' ) THEN
C     --- FORCES REPARTIES PAR VALEURS REELLES---
C        POUR LE CAS DU VENT
         CALL TECACH('NNN','PVITER',1,LFORC,IRET)
         IF ( IRET .EQ. 0 ) THEN
           IF (NOMTE .EQ. 'MECA_POU_C_T') GOTO 997
           OKVENT = .TRUE.
           NORMAL = .FALSE.
           GLOBAL = .FALSE.
           DO 30 I= 1 , 3
             Q(I)   = ZR(LFORC-1+I)
             Q(I+6) = ZR(LFORC+2+I)
30         CONTINUE
         ELSE
           CALL JEVECH ('PFR1D1D','L',LFORC)
           XXX = ABS(ZR(LFORC+6))
           GLOBAL = XXX .LT. 1.D-3
           NORMAL = XXX .GT. 1.001D0
           DO 40 I = 1, 3
             Q(I)   =  ZR(LFORC-1+I)
             Q(I+6) =  Q(I)
             XXX    =  ABS(ZR(LFORC+2+I))
             IF (XXX .GT. 1.D-20) CALL U2MESS('F','ELEMENTS2_46')
40         CONTINUE
         ENDIF
C
      ELSEIF ( OPTION .EQ. 'CHAR_MECA_FF1D1D' .OR.
     &         OPTION .EQ. 'CHAR_MECA_SF1D1D' ) THEN
C     --- FORCES REPARTIES PAR FONCTIONS ---
          CALL TECACH ('NNN', 'PTEMPSR', 1, ITEMPS,IRET )
          IF ( IRET .EQ. 0 ) THEN
             W(4) = ZR(ITEMPS)
             W(8) = ZR(ITEMPS)
             NBPAR = 4
          ELSE
             NBPAR = 3
          ENDIF
          CALL JEVECH ('PFF1D1D','L',LFORC)
          NORMAL = ZK8(LFORC+6) .EQ. 'VENT'
          GLOBAL = ZK8(LFORC+6) .EQ. 'GLOBAL'
          DO 50 I = 1, 3
             J = I + 6
             CALL FOINTE('FM',ZK8(LFORC+I-1),NBPAR,NOMPAR,W(1),Q(I),IER)
             CALL FOINTE('FM',ZK8(LFORC+I-1),NBPAR,NOMPAR,W(5),Q(J),IER)
50        CONTINUE
C
      ELSE
        CH16 = OPTION
        CALL U2MESK('F','ELEMENTS2_47',1,CH16)
      ENDIF
C
C     --- CONTROLE DE VALIDITE DE FORCES VARIANT LINEAIREMENT ---
      IF ( ITYPE .NE. 0 ) THEN
         DO 342 I=1,3
            IF ( QQ(I) .NE. QQ(I+6)  ) THEN
               IF ( ITYPE .EQ. 10 ) THEN
                  CALL U2MESS('F','ELEMENTS2_49')
               ELSE
                  CALL U2MESS('F','ELEMENTS2_50')
               ENDIF
            ENDIF
  342    CONTINUE
      ENDIF

      IF ( OKVENT ) THEN
         S=DDOT(3,W2,1,W2,1)
         S2=1.D0/S

C        CALCUL DE LA NORME DU VECTEUR A PROJETTER
         S=DDOT(3,Q(1),1,Q(1),1)
         S4 = SQRT(S)

C        CALCUL DU VECTEUR VITESSE PERPENDICULAIRE
         FCX = 0.0D0
         IF ( S4 .GT. R8MIN ) THEN
           CALL PROVEC(W2,Q(1),U)
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,VP)
C          NORME DE LA VITESSE PERPENDICULAIRE
           VITE2=DDOT(3,VP,1,VP,1)
           VALPAV(1) = SQRT( VITE2 )
           IF ( VALPAV(1) .GT. R8MIN ) THEN
C            RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
             CALL TECACH('ONN','PVENTCX',1,IFCX,IRET)
             IF ( IRET .NE. 0 ) GOTO  999
             IF ( ZK8(IFCX)(1:1) .EQ. '.' ) GOTO  999
             CALL FOINTE('FM',ZK8(IFCX),1,NOMPAV,VALPAV,FCX,IRET)
             FCX = FCX / VALPAV(1)
           ENDIF
         ENDIF
         CALL PSCVEC(3,FCX,VP,Q(1))

C        CALCUL DE LA NORME DU VECTEUR A PROJETTER
         S=DDOT(3,Q(7),1,Q(7),1)
         S4 = SQRT(S)

C        CALCUL DU VECTEUR VITESSE PERPENDICULAIRE
         FCX = 0.0D0
         IF ( S4 .GT. R8MIN ) THEN
           CALL PROVEC(W2,Q(7),U)
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,VP)
C          NORME DE LA VITESSE PERPENDICULAIRE
           VITE2=DDOT(3,VP,1,VP,1)
           VALPAV(1) = SQRT( VITE2 )
           IF ( VALPAV(1) .GT. R8MIN ) THEN
C            RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
             CALL TECACH('ONN','PVENTCX',1,IFCX,IRET)
             IF ( IRET .NE. 0 ) GOTO  999
             IF ( ZK8(IFCX)(1:1) .EQ. '.' ) GOTO  999
             CALL FOINTE('FM',ZK8(IFCX),1,NOMPAV,VALPAV,FCX,IRET)
             FCX = FCX / VALPAV(1)
           ENDIF
         ENDIF
         CALL PSCVEC(3,FCX,VP,Q(7))

      ELSEIF ( NORMAL ) THEN
         S=DDOT(3,W2,1,W2,1)
         S2=1.D0/S

         S=DDOT(3,Q(1),1,Q(1),1)
         S4 = SQRT(S)
         IF ( S4 .GT. R8MIN) THEN
           CALL PROVEC(W2,Q(1),U)
           S=DDOT(3,U,1,U,1)
           S3 = SQRT(S)
           S5 = S3*SQRT(S2)/S4
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,U)
           CALL PSCVEC(3,S5,U,Q(1))
         ENDIF

         S=DDOT(3,Q(7),1,Q(7),1)
         S4 = SQRT(S)
         IF ( S4 .GT. R8MIN) THEN
           CALL PROVEC(W2,Q(7),U)
           S=DDOT(3,U,1,U,1)
           S3 = SQRT(S)
           S5 = S3*SQRT(S2)/S4
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,U)
           CALL PSCVEC(3,S5,U,Q(7))
         ENDIF
      ENDIF
C     --- PASSAGE REPERE LOCAL DU VECTEUR FORCE  ---
      IF ( GLOBAL .OR. NORMAL .OR. OKVENT ) THEN
         IF ( NOMTE(1:12) .EQ. 'MECA_POU_C_T' ) THEN
            CALL UTPVGL ( NNOC, NCC, PGL1, Q(1), QQ(1) )
            CALL UTPVGL ( NNOC, NCC, PGL2, Q(7), QQ(7) )
         ELSE
            CALL UTPVGL ( NNO, NC, PGL, Q(1), QQ(1) )
         ENDIF
      ELSE
         DO 343 I = 1, 12
            QQ(I) = Q(I)
  343    CONTINUE
      ENDIF
C
C      ---A CAUSE DES CHARGEMENTS VARIABLES ---
      COEF1 = UN
      COEF2 = UN
C
 777  CONTINUE
C
C *********************************************************************
C
C     --- RECUPERATION DU COEF_MULT ---
C
      CALL TECACH('NNN','PCOEFFR',1,ICOER,IRETR)
      CALL TECACH('NNN','PCOEFFC',1,ICOEC,IRETC)
C
      IF ( IRETR .EQ. 0 ) THEN
         DO 400 I = 1 , 12
            QQ(I) = QQ(I) * ZR(ICOER)
 400     CONTINUE
         CALL PTFOP1 ( ITYPE, COEF1, COEF2, XL, RAD, ANGS2, GLOBAL,
     &                     QQ, FER )
C
      ELSEIF ( IRETC .EQ. 0 ) THEN
         DO 410 I = 1 , 12
            QQR(I) =   QQ(I) *  DBLE( ZC(ICOEC) )
            QQI(I) =   QQ(I) * DIMAG( ZC(ICOEC) )
 410     CONTINUE
         CALL PTFOP1 ( ITYPE, COEF1, COEF2, XL, RAD, ANGS2, GLOBAL,
     &                     QQR, FER )
         CALL PTFOP1 ( ITYPE, COEF1, COEF2, XL, RAD, ANGS2, GLOBAL,
     &                     QQI, FEI )
C
      ELSE
         CALL PTFOP1 ( ITYPE, COEF1, COEF2, XL, RAD, ANGS2, GLOBAL,
     &                     QQ, FER )
C
      ENDIF
C

      GOTO 1000

997   CONTINUE
      CALL U2MESS('F','ELEMENTS2_51')

998   CONTINUE
      CALL U2MESS('F','ELEMENTS2_52')

999   CONTINUE
      CALL U2MESS('F','ELEMENTS2_53')


 1000 CONTINUE
      END
