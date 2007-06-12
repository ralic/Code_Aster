      SUBROUTINE OP0094 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
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
C
C     COMMANDE DEFI_TRC
C
C ----------------------------------------------------------------------
C     --- DEBUT DECLARATIONS NORMALISEES JEVEUX ------------------------
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
C     --- FIN DECLARATIONS NORMALISEES JEVEUX --------------------------
C
      INTEGER      NBHIST, NBTRC, NBPARR, IBID, LONMAX, NBVAL, I, J,
     +             JVALE, NBV, IND
      PARAMETER    ( NBPARR = 19 )
      REAL*8       RBID, XNBV, VALE(6)
      CHARACTER*8  K8B, NOMTRC, TYPARR(NBPARR)
      CHARACTER*16 CONCEP, NOMCMD, NOPARR(NBPARR)
      COMPLEX*16   C16B
C
      DATA NOPARR / 'VITESSE' , 'PARA_EQ' , 'COEF_0' , 'COEF_1' ,
     +              'COEF_2' , 'COEF_3' , 'COEF_4' , 'COEF_5' ,
     +              'NB_POINT' ,
     +              'Z1' , 'Z2' , 'Z3' , 'TEMP' ,
     +              'SEUIL' , 'AKM' , 'BKM' , 'TPLM',
     +              'DREF', 'A' /
      DATA TYPARR / 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R',
     +              'R' , 'R' , 'R' , 'R' ,
     +              'R' , 'R' , 'R' , 'R',
     +              'R' , 'R' /
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES ( NOMTRC, CONCEP, NOMCMD )
C
      CALL GETFAC ( 'HIST_EXP' , NBHIST )
      CALL GETFAC ( 'TEMP_MS'  , NBTRC  )
C
C
      CALL TBCRSD ( NOMTRC, 'G' )
      CALL TBAJPA ( NOMTRC, NBPARR, NOPARR, TYPARR )
C
      LONMAX = 0
      DO 100 I = 1 , NBHIST
         CALL GETVR8 ( 'HIST_EXP', 'VALE', I,1,0, RBID, NBVAL )
         LONMAX = MAX ( LONMAX , -NBVAL )
 100  CONTINUE
      CALL WKVECT ( '&&OP0094.VALE', 'V V R', LONMAX, JVALE )
C
      DO 110 I = 1 , NBHIST
         CALL GETVR8 ( 'HIST_EXP', 'VALE', I,1,0, RBID, NBVAL )
         NBVAL = -NBVAL
         CALL GETVR8 ( 'HIST_EXP', 'VALE', I,1,NBVAL, ZR(JVALE), NBVAL )
         CALL TBAJLI ( NOMTRC, 8, NOPARR,IBID,ZR(JVALE),C16B,K8B, 0 )
         XNBV = ( NBVAL - 8 ) / 4
         CALL TBAJLI ( NOMTRC, 1, NOPARR(9),IBID,XNBV,C16B,K8B, I )
 110  CONTINUE
C
      DO 120 I = 1 , NBHIST
         CALL GETVR8 ( 'HIST_EXP', 'VALE', I,1,0, RBID, NBVAL )
         NBVAL = -NBVAL
         CALL GETVR8 ( 'HIST_EXP', 'VALE', I,1,NBVAL, ZR(JVALE), NBVAL )
         NBV = ( NBVAL - 8 ) / 4
         DO 122 J = 1 , NBV
            IND = JVALE + 8 + 4*(J-1)
            CALL TBAJLI ( NOMTRC, 4,NOPARR(10), IBID,ZR(IND),C16B,K8B,0)
 122     CONTINUE
 120  CONTINUE
C
C
C
      DO 200 I = 1 , NBTRC
         CALL GETVR8 ( 'TEMP_MS', 'SEUIL', I,1,1, VALE(1), IBID )
         CALL GETVR8 ( 'TEMP_MS', 'AKM'  , I,1,1, VALE(2), IBID )
         CALL GETVR8 ( 'TEMP_MS', 'BKM'  , I,1,1, VALE(3), IBID )
         CALL GETVR8 ( 'TEMP_MS', 'TPLM' , I,1,1, VALE(4), IBID )
         CALL GETVR8 ( 'GRAIN_AUST', 'DREF'    , I,1,1, VALE(5), IBID )
         IF (IBID .EQ. 0) VALE(5) = 0.D0
         CALL GETVR8 ( 'GRAIN_AUST', 'A', I,1,1, VALE(6), IBID )
         IF (IBID .EQ. 0) VALE(6) = 0.D0
         CALL TBAJLI ( NOMTRC, 6,NOPARR(14), IBID,VALE,C16B,K8B, 0 )
 200  CONTINUE
C
      CALL JEDEMA()
      END
