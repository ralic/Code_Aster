      SUBROUTINE CALFIG ( GUIDAG, RESU, OBST, NO, DIMOBS, DIMTUB,
     +                    OBSUSE, TUBUSE, NOMTAB )
      IMPLICIT   NONE
      INTEGER             NO, DIMOBS, DIMTUB
      REAL*8              OBSUSE(*), TUBUSE(*)
      CHARACTER*8         OBST, GUIDAG
      CHARACTER*19        RESU, NOMTAB
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 15/01/2002   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE CRP_6
C
C   CALCULE LES FIGURES DE JEU A PARTIR DES TUBES ET OBSTACLES USES 
C ----------------------------------------------------------------------
C     ---- DEBUT DES COMMUNS JEVEUX ------------------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ---- FIN DES COMMUNS JEVEUX --------------------------------------
      INTEGER    I, J, K, N, P, Q, R, K1, N1, CONTAC, NDIM, NV
      INTEGER    IBID, ICOMP1, ICOMP2, ICOMP3, LVAL, LPRO
      INTEGER    IJEU, IJPRM, ITUB, IOBS, IDRAY, IDTHE, IDREFE
      PARAMETER  ( NDIM = 3000 )
      COMPLEX*16    C16B
      REAL*8        R8B, C, D, E, F, ANG2, ANGMIN, ANGMAX, RHO2
      REAL*8        X, Y, XTUBE, YTUBE, RTUBE2
      REAL*8        PAS, PAS1, PAS2, PAS3, THET, TETDEG, ESPACE
      REAL*8        ANG2BI, RHO2BI, XA, XABIS, YA, YABIS, A1, B1, GAME
      REAL*8        RAD, R8DGRD, DEG, R8RDDG, PI, R8PI, R8PREM
      CHARACTER*8   K8B
      CHARACTER*16  NOPARA(2)
      CHARACTER*19  NOMFON
      CHARACTER*24  TABK(2)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      ESPACE = 0.D0
      RAD = R8DGRD( )
      DEG = R8RDDG( )
      PI  = R8PI( )
C
      CALL WKVECT ( '&&CALFIG.JEU'   , 'V V R', 2*NDIM, IJEU  )
      CALL WKVECT ( '&&CALFIG.JEUPRM', 'V V R', 2*NDIM, IJPRM )
      CALL WKVECT ( '&&CALFIG.TUB'   , 'V V R', 2*NDIM, ITUB  )
      CALL WKVECT ( '&&CALFIG.OBS'   , 'V V R', 2*NDIM, IOBS  )
C
      CALL JEVEUO ( OBST//'           .VALR', 'L', IDRAY )
      CALL JEVEUO ( OBST//'           .VALT', 'L', IDTHE )
C
      NOPARA(1) = 'LIEU'
      NOPARA(2) = 'FONCTION'
C
      CALL GCNCON ( '_' , K8B )
      NOMFON = RESU(1:8)//K8B
      CALL FO0182 ( NOMFON, DIMTUB, TUBUSE )
      TABK(1) = 'TUBE_USE'
      TABK(2) = NOMFON
      CALL TBAJLI ( NOMTAB, 2, NOPARA, IBID, R8B, C16B, TABK, 0 )
C
      CALL GCNCON ( '_' , K8B )
      NOMFON = RESU(1:8)//K8B
      CALL FO0182 ( NOMFON, DIMOBS, OBSUSE )
      TABK(1) = 'OBST_USE'
      TABK(2) = NOMFON
      CALL TBAJLI ( NOMTAB, 2, NOPARA, IBID, R8B, C16B, TABK, 0 )
C
C --- REMPLISSAGE DES TABLEAUX POUR LE CALCUL :
C     ---------------------------------------
C
      IF ( GUIDAG .EQ. 'CERCLE' ) THEN
         DO 100 J = 1 , NO
            ZR(IJEU+2*J-2) = ZR(IDTHE+J-1) / RAD
            ZR(IJEU+2*J-1) = 6.5D-4
 100     CONTINUE
      ELSE
         DO 101 J = 1 , NO
            ZR(IJEU+2*J-2) = ZR(IDTHE+J-1) / RAD
            ZR(IJEU+2*J-1) = ZR(IDRAY+J-1)
 101     CONTINUE
      ENDIF
C
      CALL GCNCON ( '_' , K8B )
      NOMFON = RESU(1:8)//K8B
      CALL FO0182 ( NOMFON, NO, ZR(IJEU) )
      TABK(1) = 'JEU_INIT'
      TABK(2) = NOMFON
      CALL TBAJLI ( NOMTAB, 2, NOPARA, IBID, R8B, C16B, TABK, 0 )
C
      N1 = NO 
      DO 104 J = 1 , (N1-1)
         ZR(IJEU+2*(J+N1)-2) = ZR(IJEU+2*J) + 360.D0
         ZR(IJEU+2*(J+N1)-1) = ZR(IJEU+2*J+1)
 104  CONTINUE
      DO 106 J = 1 , DIMOBS
         ZR(IOBS+2*J-2) = OBSUSE(2*J-1)
         ZR(IOBS+2*J-1) = OBSUSE(2*J)
 106  CONTINUE
      DO 108 J = 1 , (DIMOBS-1)
         ZR(IOBS+2*(J+DIMOBS)-2) = ZR(IOBS+2*J) + 360.D0
         ZR(IOBS+2*(J+DIMOBS)-1) = ZR(IOBS+2*J+1)
 108  CONTINUE
      DO 110 J = 1 , DIMTUB
         ZR(ITUB+2*J-2) = TUBUSE(2*J-1)
         ZR(ITUB+2*J-1) = TUBUSE(2*J)
 110  CONTINUE
      DO 112 J = 1 , (DIMTUB-1)
         ZR(ITUB+2*(J+DIMTUB)-2) = ZR(ITUB+2*J) + 360.D0
         ZR(ITUB+2*(J+DIMTUB)-1) = ZR(ITUB+2*J+1)
 112  CONTINUE
C
C --- DETERMINATION DU PAS DE CALCUL :
C     ------------------------------
      PAS1 = 0.100D-3
      PAS2 = 0.010D-3
      PAS3 = 0.001D-3
C
      PAS = PAS1
      N = 1 
 200  CONTINUE
         I = 1
         ESPACE = ZR(IJEU+2*N-1)
C
C ------ ANGLE DE BALAYAGE :
C        -----------------
C
         IF ( ZR(IJEU+2*N-2) .GT. 90.D0 )  THEN
            IF ( ZR(IJEU+2*N-2) .LT. 450.D0 ) THEN
               ANGMAX = ZR(IJEU+2*N-2)+90.D0
               ANGMIN = ZR(IJEU+2*N-2)-90.D0
            ENDIF
         ENDIF
C 
         DO 202 K1 = 1,(2*DIMTUB)
            IF ( ZR(IJEU+2*N-2) .GT. 90.D0 ) THEN 
               IF ( ZR(IJEU+2*N-2) .LT. 450.D0 ) THEN
                  IF ( ZR(ITUB+2*K1-2).GT.ANGMIN )  GOTO 204
               ENDIF
            ENDIF
 202     CONTINUE
 204     CONTINUE
         CONTAC = 0
C
C ------ AVANCEE DU CRAYON, DETERMINATION DES COORDONNEES :
C        ------------------------------------------------
C
 206     CONTINUE
         IF ( PAS .EQ. PAS2 ) THEN
            ICOMP2 = ICOMP2 + 1
            ICOMP3 = 0
         ELSEIF ( PAS .EQ. PAS1 ) THEN
            ICOMP1 = I
            ICOMP2 = 0
            ICOMP3 = 0
         ELSEIF ( PAS .EQ. PAS3 ) THEN
            ICOMP3 = ICOMP3 + 1
         ENDIF
         X = ( ICOMP1*PAS1 + ICOMP2*PAS2 + ICOMP3*PAS3 + ESPACE )
     +                          * COS( ZR(IJEU+2*N-2)*RAD )
         Y = ( ICOMP1*PAS1 + ICOMP2*PAS2 + ICOMP3*PAS3 + ESPACE )
     +                          * SIN( ZR(IJEU+2*N-2)*RAD )
C
C ------ BALAYAGE DE PART ET D'AUTRE DE L'ANGLE TRAITE :
C        ---------------------------------------------
C
         IF ( ZR(IJEU+2*N-2) .GT. 90.D0 ) THEN
            IF ( ZR(IJEU+2*N-2) .LT. 450.D0 ) THEN
               DO 208 K = K1,(2*DIMTUB)
                  IF ( ZR(ITUB+2*K-2) .GT. ANGMAX )  GOTO 210
C
C --------------- AVANCEE DU CRAYON AU NIVEAU DE L'ANGLE BALAYE 
C                 DANS LA DIRECTION DE L'ANGLE TRAITE :
C                 -----------------------------------
C
                  XTUBE = ZR(ITUB+2*K-1)*COS(ZR(ITUB+2*K-2)*RAD) + X
                  YTUBE = ZR(ITUB+2*K-1)*SIN(ZR(ITUB+2*K-2)*RAD) + Y
                  RTUBE2 = XTUBE*XTUBE + YTUBE*YTUBE
                  IF ( XTUBE .EQ. 0.D0 ) THEN
                     THET = 0.D0
                  ELSE
                     IF ((XTUBE.GT.0.D0).AND.(YTUBE.GT.0.D0)) THEN
                        THET   = ATAN(YTUBE/XTUBE)
                        TETDEG = THET*DEG
                     ELSEIF ((XTUBE.LT.0.D0).AND.(YTUBE.GT.0.D0)) THEN
                        THET   = PI-ABS(ATAN(YTUBE/XTUBE))
                        TETDEG = THET*DEG
                     ELSEIF ((XTUBE.LT.0.D0).AND.(YTUBE.LT.0.D0)) THEN
                        THET   = PI+ABS(ATAN(YTUBE/XTUBE))
                        TETDEG = THET*DEG
                     ELSEIF ((XTUBE.GT.0.D0).AND.(YTUBE.LT.0.D0)) THEN
                        THET   = 2.D0*PI-ABS(ATAN(YTUBE/XTUBE))
                        TETDEG = THET*DEG
                     ENDIF
                  ENDIF
C
C --------------- DETERMINATION DE L ANGLE OBSTACLE CORRESPONDANT :
C                 -----------------------------------------------
C
                  P = 0
1458              CONTINUE
                     P = P + 1
                     RHO2 = ZR(IOBS+2*P-1)
                     ANG2 = ZR(IOBS+2*P-2)  
                     IF ( ZR(IOBS+2*P-2) .LE. TETDEG ) THEN
                        RHO2BI = RHO2
                        ANG2BI = ANG2
                        GOTO 1458
                     ENDIF
                     XA = RHO2*COS(ANG2*RAD)
                     YA = RHO2*SIN(ANG2*RAD)
                     XABIS = RHO2BI*COS(ANG2BI*RAD)
                     YABIS = RHO2BI*SIN(ANG2BI*RAD)
                     IF ( ABS(XA-XABIS) .GT.R8PREM( ) ) THEN     
                        A1   = (YA-YABIS)/(XA-XABIS)
                        B1   = YA - A1*XA
                        RHO2 = B1/(SIN(TETDEG*RAD)-A1*COS(TETDEG*RAD))
                     ELSE
                        RHO2 = XA/COS(TETDEG*RAD)
                     ENDIF
C
C ------------------ TEST DE CONTACT :
C                    ---------------
C
                     IF ( RTUBE2 .GT. (RHO2*RHO2) ) THEN
                        IF ( PAS .EQ. PAS1 ) THEN
                           I = I - 1
                           ICOMP1 = I 
                           PAS = PAS2
                           ICOMP2 = 0
                           GOTO 206
                        ELSEIF ( PAS .EQ. PAS2 ) THEN 
                           ICOMP2 = ICOMP2 - 1
                           PAS = PAS3
                           ICOMP3 = 0
                           GOTO 206
                        ELSEIF ( PAS .EQ. PAS3 ) THEN
                           CONTAC = 1
                           ICOMP3 = ICOMP3 - 1
                        ENDIF
                        GOTO 210
                     ENDIF
 208           CONTINUE
 210           CONTINUE  
               IF ( CONTAC .EQ. 0 ) THEN
                  I=I+1
                  GOTO 206
               ELSE
                  IF ( CONTAC .EQ. 1 ) THEN
                     ESPACE =ESPACE+ICOMP1*PAS1+ICOMP2*PAS2+ICOMP3*PAS3
                     PAS = PAS1
                     IF ( ZR(IJEU+2*N-2) .EQ. 360.D0 )  GAME = ESPACE
                     IF ( ZR(IJEU+2*N-2) .GE. 360.D0 ) THEN
                        ZR(IJEU+2*N-2) = ZR(IJEU+2*N-2)-360.D0
                     ENDIF
                     ZR(IJPRM+2*N-2) = ZR(IJEU+2*N-2)
                     ZR(IJPRM+2*N-1) = ESPACE
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         N = N + 1
         IF ( N .LE. (2*N1-1) )  GOTO 200
C
      DO 300 Q=1,N
         C = ZR(IJPRM+2*Q-2)
         D = ZR(IJPRM+2*Q-1)
         DO 302 R=(Q+1),N
            IF ( (ZR(IJPRM+2*R-2).LT.C   ) .AND.
     &           (ZR(IJPRM+2*R-2).GT.0.D0) ) THEN
               E = C 
               F = D
               C = ZR(IJPRM+2*R-2)
               D = ZR(IJPRM+2*R-1)
               ZR(IJPRM+2*R-2) = E
               ZR(IJPRM+2*R-1) = F
            ENDIF
 302     CONTINUE
         ZR(IJPRM+2*Q-2) = C
         ZR(IJPRM+2*Q-1) = D
 300  CONTINUE
      NV = 2
      DO 304 Q=1,N
         IF (ZR(IJPRM+2*Q-2).NE.0.D0) NV = NV +1
 304  CONTINUE
C
      IF ( RESU(1:8) .NE. OBST ) THEN
         CALL WKVECT ( RESU//'.REFE', 'G V K24',1 , IDREFE )
         ZK24(IDREFE) = 'DISCRET'
         CALL WKVECT ( RESU//'.VALR', 'G V R'  ,NV, IDRAY )
         CALL WKVECT ( RESU//'.VALT', 'G V R'  ,NV, IDTHE )
      ELSE
         CALL JEVEUO ( RESU//'.VALR', 'E', IDRAY )
         CALL JEVEUO ( RESU//'.VALT', 'E', IDTHE )
      ENDIF
C
      CALL GCNCON ( '_' , K8B )
      NOMFON = RESU(1:8)//K8B
C
      CALL WKVECT ( NOMFON//'.PROL', 'G V K8', 5, LPRO )
      ZK8(LPRO)   = 'FONCTION'
      ZK8(LPRO+1) = 'LIN LIN '
      ZK8(LPRO+2) = 'THETA   '
      ZK8(LPRO+3) = 'R       '
      ZK8(LPRO+4) = 'EE      '
C
      CALL WKVECT ( NOMFON//'.VALE', 'G V R8', 2*NV, LVAL )
C
      I = 1 
      ZR(IDTHE) = 0.D0
      ZR(IDRAY) = GAME
      ZR(LVAL+I-1)    = 0.D0
      ZR(LVAL+NV+I-1) = GAME
      DO 306 Q = 1 , N
         IF ( ZR(IJPRM+2*Q-2) .NE. 0.D0 ) THEN
            I = I + 1
            ZR(IDTHE+I-1) = ZR(IJPRM+2*Q-2) * RAD
            ZR(IDRAY+I-1) = ZR(IJPRM+2*Q-1) 
            ZR(LVAL+I-1)    = ZR(IJPRM+2*Q-2)
            ZR(LVAL+NV+I-1) = ZR(IJPRM+2*Q-1)
         ENDIF
 306  CONTINUE
      I = I + 1
      ZR(IDTHE+I-1) = 360.D0 * RAD
      ZR(IDRAY+I-1) = GAME
      ZR(LVAL+I-1)    = 360.D0
      ZR(LVAL+NV+I-1) = GAME
C
      TABK(1) = 'JEU_USE'
      TABK(2) = NOMFON
      CALL TBAJLI ( NOMTAB, 2, NOPARA, IBID, R8B, C16B, TABK, 0 )
C
      CALL JEDETR ( '&&CALFIG.JEU' )
      CALL JEDETR ( '&&CALFIG.JEUPRM' )
      CALL JEDETR ( '&&CALFIG.TUB'    )
      CALL JEDETR ( '&&CALFIG.OBS'    )
      CALL JEDEMA()  
      END
