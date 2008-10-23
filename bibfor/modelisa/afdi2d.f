      SUBROUTINE AFDI2D (IREP,ETA,CAR,VAL,JDC,JDV,JDCNS,JDVNS,IVR,IV,
     +                   KMA,NCMP,NTP,JDCINF,JDVINF,ISYM,IFM)
      IMPLICIT   NONE
      INTEGER           IREP,JDV(3),JDC(3),IVR(*),IV,NCMP,NTP,IFM
      INTEGER           JDVNS(3),JDCNS(3),ISYM,JDCINF,JDVINF
      REAL*8                 ETA,    VAL(*)
      CHARACTER*1            KMA(3)
      CHARACTER*9            CAR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 23/10/2008   AUTEUR TORKHANI M.TORKHANI 
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
C     ------------------------------------------------------------------
C     AFFECTATION DES VALEURS DES MATRICES A TOUS LES ELEMENTS
C     DEMANDES PAR L UTILISATEUR DANS LES CARTES CORRESPONDANTES
C     LES ELEMENTS CONCERNES SONT LES ELEMENTS DISCRETS 2D
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      INTEGER      J, L
      CHARACTER*7  KI
C     ------------------------------------------------------------------
C
C     BOUCLE SUR LES TYPES DE MATRICE (K,M,A)
C
      CALL CODENT(1,'G',KI)
      ZK8(JDCINF+1-1) = 'X'//KI
      ZR (JDVINF+1-1) = ISYM
      DO 200 J = 1 , 3
         IF (CAR(1:1).EQ.KMA(J)) THEN
            NTP = J
C - T_D_N
            IF (CAR(3:7).EQ.'T_D_N') THEN
               DO 5 L = 1 , 3
                  CALL CODENT(L,'G',KI)
                  ZK8(JDC(J)+L-1) = KMA(J)//KI
                  ZR (JDV(J)+L-1) = 0.D0
 5             CONTINUE
               ZK8(JDC(J)+3) = 'REP'//KMA(J)//'    '
               ZR (JDV(J)+3) = IREP
               IF (CAR(1:1).EQ.'M')THEN
                  ZR (JDV(J)  ) = VAL(IV)
                  ZR (JDV(J)+2) = VAL(IV)
                  IV = IV + 1
               ELSE
                  ZR (JDV(J)  ) = VAL(IV)
                  ZR (JDV(J)+2) = VAL(IV+1)
                  IV = IV + 2
               ENDIF
           IF(IVR(3).EQ.1) CALL IMPMV(IFM,CAR(1:8),ZR(JDV(J)),3)
               NCMP = 4
               IF (CAR(1:1).EQ.'K')THEN
                  ZK8(JDC(J)+4) = 'ETA     '
                  ZR (JDV(J)+4) = ETA
                  NCMP = 5
               ENDIF
C - T_D_L
            ELSEIF (CAR(3:7).EQ.'T_D_L') THEN
               IF (CAR(1:1).EQ.'M') THEN
                  NCMP = 0
               ELSE
                  DO 15 L = 1 , 10
                     CALL CODENT(L,'G',KI)
                     ZK8(JDC(J)+L-1) = KMA(J)//KI
                     ZR (JDV(J)+L-1) = 0.D0
 15               CONTINUE
                  ZK8(JDC(J)+10) = 'REP'//KMA(J)//'    '
                  ZR (JDV(J)+10) = IREP
                  ZR (JDV(J))    =  VAL(IV)
                  ZR (JDV(J)+2)  =  VAL(IV+1)
                  ZR (JDV(J)+3)  = -VAL(IV)
                  ZR (JDV(J)+5)  =  VAL(IV)
                  ZR (JDV(J)+7)  = -VAL(IV+1)
                  ZR (JDV(J)+9 ) =  VAL(IV+1)
                  IV = IV + 2
           IF(IVR(3).EQ.1) CALL IMPMV(IFM,CAR(1:8),ZR(JDV(J)),10)
                  NCMP = 11
                  IF (CAR(1:1).EQ.'K') THEN
                     ZK8(JDC(J)+11) = 'ETA     '
                     ZR (JDV(J)+11) = ETA
                     NCMP = 12
                  ENDIF
               ENDIF
C - TR_D_N
            ELSEIF (CAR(3:8).EQ.'TR_D_N') THEN
               DO 20 L = 1 , 6
                  CALL CODENT(L,'G',KI)
                  ZK8(JDC(J)+L-1) = KMA(J)//KI
                  ZR (JDV(J)+L-1) = 0.D0
 20            CONTINUE
               ZK8(JDC(J)+6)  = 'REP'//KMA(J)//'    '
               ZR (JDV(J)+6)  = IREP
               IF (CAR(1:1).EQ.'M') THEN
                  ZR (JDV(J))   =  VAL(IV)
                  ZR (JDV(J)+2) =  VAL(IV)
                  ZR (JDV(J)+3) =  VAL(IV)*VAL(IV+3)
                  ZR (JDV(J)+4) = -VAL(IV)*VAL(IV+2)
                  ZR (JDV(J)+5) =  VAL(IV+1)+ 
     +                VAL(IV)*(VAL(IV+2)*VAL(IV+2)+VAL(IV+3)*VAL(IV+3))
                  IV = IV + 4
               ELSE
                  ZR (JDV(J))    = VAL(IV)
                  ZR (JDV(J)+2)  = VAL(IV+1)
                  ZR (JDV(J)+5)  = VAL(IV+2)
                  IV = IV + 3
               ENDIF
           IF (IVR(3).EQ.1) CALL IMPMV(IFM,CAR(1:8),ZR(JDV(J)),6)
               NCMP = 7
               IF (CAR(1:1).EQ.'K') THEN
                  ZK8(JDC(J)+7)  = 'ETA     '
                  ZR (JDV(J)+7)  = ETA
                  NCMP = 8
               ENDIF
C - TR_D_L
            ELSEIF (CAR(3:8).EQ.'TR_D_L') THEN
               IF (CAR(1:1).EQ.'M') THEN
                  NCMP = 0
               ELSE
                  DO 25 L = 1 , 21
                     CALL CODENT(L,'G',KI)
                     ZK8(JDC(J)+L-1) = KMA(J)//KI
                     ZR (JDV(J)+L-1) = 0.D0
 25               CONTINUE
                  ZK8(JDC(J)+21) = 'REP'//KMA(J)//'    '
                  ZR (JDV(J)+21) = IREP
                  ZR (JDV(J))    =  VAL(IV)
                  ZR (JDV(J)+2)  =  VAL(IV+1)
                  ZR (JDV(J)+5)  =  VAL(IV+2)
                  ZR (JDV(J)+6)  = -VAL(IV)
                  ZR (JDV(J)+9)  =  VAL(IV)
                  ZR (JDV(J)+11) = -VAL(IV+1)
                  ZR (JDV(J)+14) =  VAL(IV+1)
                  ZR (JDV(J)+17) = -VAL(IV+2)
                  ZR (JDV(J)+20) =  VAL(IV+2)
                  IV = IV + 3
           IF (IVR(3).EQ.1) CALL IMPMV(IFM,CAR(1:8),ZR(JDV(J)),21)
                  NCMP = 22
                  IF (CAR(1:1).EQ.'K') THEN
                     ZK8(JDC(J)+22) = 'ETA     '
                     ZR (JDV(J)+22) =  ETA
                     NCMP = 23 
                  ENDIF
               ENDIF
C - T_N
            ELSEIF (CAR(3:5).EQ.'T_N') THEN
C - T_N_NS
               IF (CAR(6:8).EQ.'_NS') THEN
                  DO 31 L = 1 , 4
                     CALL CODENT(L,'G',KI)
                     ZK8(JDCNS(J)+L-1) = KMA(J)//KI
                     ZR (JDVNS(J)+L-1) = VAL(IV)
                     IV = IV + 1
 31               CONTINUE
                  ZK8(JDCNS(J)+4) = 'REP'//KMA(J)//'    '
                  ZR (JDVNS(J)+4) = IREP
                  NCMP = 5
                  IF (CAR(1:1).EQ.'K') THEN
                     ZK8(JDCNS(J)+5) = 'ETA     '
                     ZR (JDVNS(J)+5) =  ETA
                     NCMP = 6
                  ENDIF
               ELSE
                  DO 30 L = 1 , 3
                     CALL CODENT(L,'G',KI)
                     ZK8(JDC(J)+L-1) = KMA(J)//KI
                     ZR (JDV(J)+L-1) = VAL(IV)
                     IV = IV + 1
 30               CONTINUE
                  ZK8(JDC(J)+3) = 'REP'//KMA(J)//'    '
                  ZR (JDV(J)+3) = IREP
           IF (IVR(3).EQ.1) CALL IMPMV(IFM,CAR(1:8),ZR(JDV(J)),3)
                  NCMP = 4
                  IF (CAR(1:1).EQ.'K') THEN
                     ZK8(JDC(J)+4) = 'ETA     '
                     ZR (JDV(J)+4) =  ETA
                     NCMP = 5
                  ENDIF
              ENDIF
C - T_L
            ELSEIF (CAR(3:5).EQ.'T_L') THEN
C - T_L_NS
               IF (CAR(6:8).EQ.'_NS') THEN
                  DO 36 L = 1 , 16
                     CALL CODENT(L,'G',KI)
                     ZK8(JDCNS(J)+L-1) = KMA(J)//KI
                     ZR (JDVNS(J)+L-1) = VAL(IV)
                     IV = IV + 1
 36               CONTINUE
                  ZK8(JDCNS(J)+16) = 'REP'//KMA(J)//'    '
                  ZR (JDVNS(J)+16) = IREP
                  NCMP = 17
                  IF (CAR(1:1).EQ.'K') THEN
                     ZK8(JDCNS(J)+17) = 'ETA     '
                     ZR (JDVNS(J)+17) =  ETA
                     NCMP = 18
                  ENDIF
               ELSE
                  DO 35 L = 1 , 10
                     CALL CODENT(L,'G',KI)
                     ZK8(JDC(J)+L-1) = KMA(J)//KI
                     ZR (JDV(J)+L-1) = VAL(IV)
                     IV = IV + 1
 35               CONTINUE
                  ZK8(JDC(J)+10) = 'REP'//KMA(J)//'    '
                  ZR (JDV(J)+10) = IREP
              IF (IVR(3).EQ.1) CALL IMPMV(IFM,CAR(1:8),ZR(JDV(J)),10)
                  NCMP = 11
                  IF (CAR(1:1).EQ.'K') THEN
                     ZK8(JDC(J)+11) = 'ETA     '
                     ZR (JDV(J)+11) =  ETA
                     NCMP = 12
                  ENDIF
              ENDIF
C - TR_N
            ELSEIF (CAR(3:6).EQ.'TR_N') THEN
C - TR_N_NS
               IF (CAR(7:9).EQ.'_NS') THEN
                  DO 41 L = 1 , 9
                     CALL CODENT(L,'G',KI)
                     ZK8(JDCNS(J)+L-1) = KMA(J)//KI
                     ZR (JDVNS(J)+L-1) = VAL(IV)
                     IV = IV + 1
 41               CONTINUE
                  ZK8(JDCNS(J)+9) = 'REP'//KMA(J)//'    '
                  ZR (JDVNS(J)+9) = IREP
                  NCMP = 10
                  IF (CAR(1:1).EQ.'K') THEN
                     ZK8(JDCNS(J)+10) = 'ETA     '
                     ZR (JDVNS(J)+10) =  ETA
                     NCMP = 11
                  ENDIF
               ELSE
                  DO 40 L = 1 , 6
                     CALL CODENT(L,'G',KI)
                     ZK8(JDC(J)+L-1) = KMA(J)//KI
                     ZR (JDV(J)+L-1) = VAL(IV)
                     IV = IV + 1
 40               CONTINUE
                  ZK8(JDC(J)+6) = 'REP'//KMA(J)//'    '
                  ZR (JDV(J)+6) = IREP
              IF(IVR(3).EQ.1)CALL IMPMV(IFM,CAR(1:8),ZR(JDV(J)),6)
                  NCMP = 7
                  IF (CAR(1:1).EQ.'K') THEN
                     ZK8(JDC(J)+7) = 'ETA     '
                     ZR (JDV(J)+7) =  ETA
                     NCMP = 8
                  ENDIF
               ENDIF
C - TR_L
            ELSEIF (CAR(3:6).EQ.'TR_L') THEN
C - TR_L_NS
               IF (CAR(7:9).EQ.'_NS') THEN
                  DO 46 L = 1 , 36
                     CALL CODENT(L,'G',KI)
                     ZK8(JDCNS(J)+L-1) = KMA(J)//KI
                     ZR (JDVNS(J)+L-1) = VAL(IV)
                     IV = IV + 1
 46               CONTINUE
                  ZK8(JDCNS(J)+36) = 'REP'//KMA(J)//'    '
                  ZR (JDVNS(J)+36) = IREP
                  NCMP = 37
                  IF (CAR(1:1).EQ.'K') THEN
                     ZK8(JDCNS(J)+37) = 'ETA     '
                     ZR (JDVNS(J)+37) =  ETA
                     NCMP = 38
                  ENDIF
               ELSE
                  DO 45 L = 1 , 21
                     CALL CODENT(L,'G',KI)
                     ZK8(JDC(J)+L-1) = KMA(J)//KI
                     ZR (JDV(J)+L-1) = VAL(IV)
                     IV = IV + 1
 45               CONTINUE
                  ZK8(JDC(J)+21) = 'REP'//KMA(J)//'    '
                  ZR (JDV(J)+21) = IREP
              IF (IVR(3).EQ.1) CALL IMPMV(IFM,CAR(1:8),ZR(JDV(J)),21)
                  NCMP = 22
                  IF (CAR(1:1).EQ.'K') THEN
                     ZK8(JDC(J)+22) = 'ETA     '
                     ZR (JDV(J)+22) =  ETA
                     NCMP = 23
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
 200  CONTINUE
C
      END
