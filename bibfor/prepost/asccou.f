      SUBROUTINE ASCCOU ( MAILLA )
      IMPLICIT   NONE         
      CHARACTER*8         MAILLA
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/03/99   AUTEUR AUBHHMB M.BONNAMY 
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
C     OPERATEUR: "MODI_MAILLAGE" , MOTCLE FACTEUR "TUBE-COUDE"
C
C     REALISE LA TRANSFORMATION TUBE-COUDE
C
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
C
C     RC        = RAYON DE CINTRAGE DU COUDE
C     ALPHA     = ANGLE DU COUDE
C
C-----------------------------------------------------------------------
C
      INTEGER       NBNO, ICOOR, IDIME, N1, INO, NDIM
      REAL*8        RC, ALPHA, XT, YT, ZT, PI, R8PI, BETA, ALPHAR,
     +              XCOU, YCOU, ZCOU
      CHARACTER*24  COORD, DIME 
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C      
      CALL GETVR8 ( 'TUBE_COUDE', 'R_CINTR'   , 1,1,1, RC      , N1 )
      CALL GETVR8 ( 'TUBE_COUDE', 'ANGLE'     , 1,1,1, ALPHA   , N1 )
C
      COORD  = MAILLA//'.COORDO    .VALE'
      DIME   = MAILLA//'.DIME           '
C      
      CALL JEVEUO (COORD,'E',ICOOR)      
      CALL JEVEUO (DIME ,'L',IDIME)
      NBNO = ZI(IDIME)
      NDIM  = ZI(IDIME+5)
      PI = R8PI()
      ALPHAR = ALPHA*PI/180.D0
C      
      DO 100 INO=1, NBNO
C      
         XT = ZR(ICOOR+NDIM*(INO-1))
         YT = ZR(ICOOR+NDIM*(INO-1)+1)
         ZT = ZR(ICOOR+NDIM*(INO-1)+2)
         IF ((ZT.GE.0.D0).AND.(ZT.LE.ALPHAR*RC)) THEN
            BETA = ZT/RC
            XCOU = XT
            YCOU = RC * (1.D0-COS(BETA)) + YT * COS(BETA)
            ZCOU = (RC  - YT ) * SIN (BETA)
         ELSE IF (ZT.GT.ALPHAR*RC) THEN
            XCOU = XT
            YCOU = RC * (1.D0-COS(ALPHAR)) 
     +           + SIN(ALPHAR) * ( ZT - ALPHAR*RC )
     +           + YT * COS(ALPHAR)
            ZCOU = RC * SIN (ALPHAR)            
     +           + COS(ALPHAR) * ( ZT - ALPHAR*RC )
     +           - YT * SIN(ALPHAR)         
         ELSE
            XCOU = XT
            YCOU = YT
            ZCOU = ZT
         END IF 
         ZR(ICOOR+NDIM*(INO-1))   =   XCOU
         ZR(ICOOR+NDIM*(INO-1)+1) =   YCOU
         ZR(ICOOR+NDIM*(INO-1)+2) =   ZCOU
C         
 100  CONTINUE
C 
      CALL JEDEMA()
C      
      END
