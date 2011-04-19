      SUBROUTINE AFDI3D(IREP,ETA,CAR,VAL,JDC,JDV,IVR,IV,
     &                  KMA,NCMP,NTP,JDCINF,JDVINF,ISYM,IFM)
      IMPLICIT       NONE
      INTEGER        IREP,JDV(3),JDC(3),IVR(*),IV,NCMP,NTP,IFM
      INTEGER        ISYM,JDCINF,JDVINF
      REAL*8         ETA,VAL(*)
      CHARACTER*1    KMA(3)
      CHARACTER*(*)  CAR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C --- ---------------------------------------------------------------
C     AFFECTATION DES VALEURS DES MATRICES A TOUS LES ELEMENTS
C     DEMANDES PAR L UTILISATEUR DANS LES CARTES CORRESPONDANTES
C     LES ELEMENTS CONCERNES SONT LES ELEMENTS DISCRETS 3D
C --- ---------------------------------------------------------------
C
C --- -- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C --- --  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C --- ---------------------------------------------------------------
      INTEGER      J, L,DIMMAT,IBID
      CHARACTER*7  KI
      REAL*8       R8BID
      LOGICAL      NONSYM
      CHARACTER*8  K8BID
C --- ---------------------------------------------------------------
C

      CALL INFDIS('DMXM',DIMMAT,R8BID,K8BID)

C     BOUCLE SUR LES TYPES DE MATRICE (K,M,A)
C
      DO 200 J = 1 , 3
         IF (CAR(1:1).NE.KMA(J)) GOTO 200
C        REPERE GLOBAL OU REPERE LOCAL)
         ZK8(JDCINF+J-1) = 'REP'//KMA(J)//'    '
         ZR (JDVINF+J-1) = IREP
C        MATRICE SYMETRIQUE OU PAS
         ZK8(JDCINF+J+2) = 'SYM'//KMA(J)//'    '
         ZR (JDVINF+J+2) = ISYM
C        TYPE DE LA MATRICE
         ZK8(JDCINF+J+5) = 'DIS'//KMA(J)//'    '
         ZR (JDVINF+J+5) = 1.0D0
C        COEFFICIENT AMORTISSEMENT HYSTERETIQUE
         IF (CAR(1:1).EQ.'K')THEN
            ZK8(JDCINF+9) = 'ETAK    '
            ZR (JDVINF+9) = ETA
         ENDIF
         ZK8(JDCINF+10)   = 'TYDI    '
         CALL INFDIS('CODE',IBID,ZR(JDVINF+10),CAR)
C
         NONSYM = ( ISYM .EQ. 2 )
         NTP = J
C
C        NCMP : NOMBRE DE COMPOSANTES DE LA MATRICE NON-SYMETRIQUE
C
         DO 100 L = 1 , DIMMAT
            CALL CODENT(L,'G',KI)
            ZK8(JDC(J)+L-1) = KMA(J)//KI
            ZR (JDV(J)+L-1) = 0.D0
100      CONTINUE
C --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_T_D_N
         IF (CAR(3:7).EQ.'T_D_N') THEN
            CALL ASSERT( ISYM .EQ. 1 )
            NCMP = 9
            IF (CAR(1:1).EQ.'M')THEN
               ZR (JDV(J)  ) = VAL(IV)
               ZR (JDV(J)+2) = VAL(IV)
               ZR (JDV(J)+5) = VAL(IV)
               IV = IV + 1
            ELSE
               ZR (JDV(J)  ) = VAL(IV)
               ZR (JDV(J)+2) = VAL(IV+1)
               ZR (JDV(J)+5) = VAL(IV+2)
               IV = IV + 3
            ENDIF
C --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_T_D_L
         ELSEIF (CAR(3:7).EQ.'T_D_L') THEN
            CALL ASSERT( ISYM .EQ. 1 )
            IF (CAR(1:1).EQ.'M') THEN
               NCMP = 0
            ELSE
               NCMP = 36
               ZR (JDV(J))    =  VAL(IV)
               ZR (JDV(J)+2)  =  VAL(IV+1)
               ZR (JDV(J)+5)  =  VAL(IV+2)
               ZR (JDV(J)+9)  =  VAL(IV)
               ZR (JDV(J)+14) =  VAL(IV+1)
               ZR (JDV(J)+20) =  VAL(IV+2)
               ZR (JDV(J)+6)  = -VAL(IV)
               ZR (JDV(J)+11) = -VAL(IV+1)
               ZR (JDV(J)+17) = -VAL(IV+2)
               IV = IV + 3
            ENDIF
C --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_TR_D_N
         ELSEIF (CAR(3:8).EQ.'TR_D_N') THEN
            CALL ASSERT( ISYM .EQ. 1 )
            NCMP = 36
            IF (CAR(1:1).EQ.'M') THEN
               ZR(JDV(J))   =  VAL(IV)
               ZR(JDV(J)+2) =  VAL(IV)
               ZR(JDV(J)+5) =  VAL(IV)
               ZR(JDV(J)+7) = -VAL(IV) * VAL(IV+9)
               ZR(JDV(J)+8) =  VAL(IV) * VAL(IV+8)
               ZR(JDV(J)+10)=  VAL(IV) * VAL(IV+9)
               ZR(JDV(J)+12)= -VAL(IV) * VAL(IV+7)
               ZR(JDV(J)+15)= -VAL(IV) * VAL(IV+8)
               ZR(JDV(J)+16)=  VAL(IV) * VAL(IV+7)
               ZR(JDV(J)+9) =  VAL(IV+1) +
     &             VAL(IV)*(VAL(IV+9)*VAL(IV+9)+VAL(IV+8)*VAL(IV+8))
               ZR(JDV(J)+14)=  VAL(IV+2) +
     &             VAL(IV)*(VAL(IV+7)*VAL(IV+7)+VAL(IV+9)*VAL(IV+9))
               ZR(JDV(J)+20)=  VAL(IV+3) +
     &             VAL(IV)*(VAL(IV+8)*VAL(IV+8)+VAL(IV+7)*VAL(IV+7))
               ZR(JDV(J)+13)= VAL(IV+4) - VAL(IV)*VAL(IV+7)*VAL(IV+8)
               ZR(JDV(J)+19)= VAL(IV+5) - VAL(IV)*VAL(IV+8)*VAL(IV+9)
               ZR(JDV(J)+18)= VAL(IV+6) - VAL(IV)*VAL(IV+7)*VAL(IV+9)
               IV = IV + 10
            ELSE
               ZR(JDV(J))   = VAL(IV)
               ZR(JDV(J)+2) = VAL(IV+1)
               ZR(JDV(J)+5) = VAL(IV+2)
               ZR(JDV(J)+9) = VAL(IV+3)
               ZR(JDV(J)+14)= VAL(IV+4)
               ZR(JDV(J)+20)= VAL(IV+5)
               IV = IV + 6
            ENDIF
C --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_TR_D_L
         ELSEIF (CAR(3:8).EQ.'TR_D_L') THEN
            CALL ASSERT( ISYM .EQ. 1 )
            IF (CAR(1:1).EQ.'M') THEN
               NCMP = 0
            ELSE
               NCMP = 144
               ZR (JDV(J)   ) =  VAL(IV)
               ZR (JDV(J)+ 2) =  VAL(IV+1)
               ZR (JDV(J)+ 5) =  VAL(IV+2)
               ZR (JDV(J)+ 9) =  VAL(IV+3)
               ZR (JDV(J)+14) =  VAL(IV+4)
               ZR (JDV(J)+20) =  VAL(IV+5)
               ZR (JDV(J)+27) =  VAL(IV)
               ZR (JDV(J)+35) =  VAL(IV+1)
               ZR (JDV(J)+44) =  VAL(IV+2)
               ZR (JDV(J)+54) =  VAL(IV+3)
               ZR (JDV(J)+65) =  VAL(IV+4)
               ZR (JDV(J)+77) =  VAL(IV+5)
               ZR (JDV(J)+21) = -VAL(IV)
               ZR (JDV(J)+29) = -VAL(IV+1)
               ZR (JDV(J)+38) = -VAL(IV+2)
               ZR (JDV(J)+48) = -VAL(IV+3)
               ZR (JDV(J)+59) = -VAL(IV+4)
               ZR (JDV(J)+71) = -VAL(IV+5)
               IV = IV + 6
            ENDIF
C --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_T_N
         ELSEIF (CAR(3:5).EQ.'T_N') THEN
            IF ( NONSYM ) THEN
               NCMP = 9
            ELSE
               NCMP = 6
            ENDIF
            DO 30 L = 1 , NCMP
               CALL CODENT(L,'G',KI)
               ZK8(JDC(J)+L-1) = KMA(J)//KI
               ZR (JDV(J)+L-1) = VAL(IV)
               IV = IV + 1
30          CONTINUE
            NCMP = 9
C --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_T_L
         ELSEIF (CAR(3:5).EQ.'T_L') THEN
            IF ( NONSYM ) THEN
               NCMP = 36
            ELSE
               NCMP = 21
            ENDIF
            DO 35 L = 1 , NCMP
               CALL CODENT(L,'G',KI)
               ZK8(JDC(J)+L-1) = KMA(J)//KI
               ZR (JDV(J)+L-1) = VAL(IV)
               IV = IV + 1
35          CONTINUE
            NCMP = 36
C --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_TR_N
         ELSEIF (CAR(3:6).EQ.'TR_N') THEN
            IF ( NONSYM ) THEN
               NCMP = 36
            ELSE
               NCMP = 21
            ENDIF
            DO 40 L = 1 , NCMP
               CALL CODENT(L,'G',KI)
               ZK8(JDC(J)+L-1) = KMA(J)//KI
               ZR (JDV(J)+L-1) = VAL(IV)
               IV = IV + 1
40          CONTINUE
            NCMP = 36
C --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_TR_L
         ELSEIF (CAR(3:6).EQ.'TR_L') THEN
            IF ( NONSYM ) THEN
               NCMP = 144
            ELSE
               NCMP = 78
            ENDIF
            DO 45 L = 1 , NCMP
               CALL CODENT(L,'G',KI)
               ZK8(JDC(J)+L-1) = KMA(J)//KI
               ZR (JDV(J)+L-1) = VAL(IV)
               IV = IV + 1
45          CONTINUE
            NCMP = 144
         ENDIF
C
         IF(IVR(3).EQ.1) CALL IMPMV(IFM,CAR(1:8),ZR(JDV(J)),NCMP,ISYM)
200   CONTINUE
C
      END
