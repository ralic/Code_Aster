      SUBROUTINE RC32R1 ( NOMRES )
      IMPLICIT   NONE
      CHARACTER*8         NOMRES
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 24/05/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     STOCKAGE DES RESULTATS DANS LA TABLE DE SORTIE
C
C     ------------------------------------------------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IBID, NPAR0, IM, IG, IS, I3,
     +              NBSIGR, VALEI(2), JNUMGR, JNSITU, JNSG, JVALE,
     +              JPMPB, NBGR, IOC, NUMGR, IS1, IS2 
      PARAMETER    ( NPAR0 = 18 )
      REAL*8        UTOT, VALER(2)
      COMPLEX*16    C16B
      CHARACTER*2   K2C, K2L
      CHARACTER*4   LIEU(2)
      CHARACTER*6   K6B
      CHARACTER*8   K8B, TYPAR0(NPAR0), VALEK(3)
      CHARACTER*16  NOPAR0(NPAR0), NOPAR4(9), NOPAR5(6), NOPAR6(7)
      CHARACTER*24  K24B, K24T
C     ------------------------------------------------------------------
      DATA NOPAR0 / 'NUME_GROUPE', 'LIEU', 'NUME_SITU_1', 'NUME_SITU_2',
     +              'NUME_SITU', 'PM' , 'PB' , 'PMPB', 'SN' , 'SN*',
     +              'FACT_USAGE' , '%_FACT_USAGE', 'SM' , 'SN/3SM' ,
     +              'SN_MAX' , 'SP_MAX' , 'SALT_MAX' ,
     +              'FACT_USAGE_CUMU' /
      DATA TYPAR0 / 'I', 'K8', 'K8', 'K8','I', 'R', 'R', 'R', 'R', 
     +              'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R' /
      DATA LIEU   / 'ORIG' , 'EXTR' /
C    
      DATA NOPAR4 / 'NUME_GROUPE', 'LIEU' , 'NUME_SITU', 'PM' , 'PB' ,
     +              'PMPB', 'SN', 'FACT_USAGE', 'SN*'   /
C
      DATA NOPAR5 / 'NUME_GROUPE', 'LIEU', 'NUME_SITU_1', 
     +              'NUME_SITU_2', 'FACT_USAGE', '%_FACT_USAGE'  /

      DATA NOPAR6 / 'LIEU',  'SM' , 'SN/3SM' , 'SN_MAX' , 'SP_MAX' ,
     +              'SALT_MAX' , 'FACT_USAGE_CUMU' /
C DEB ------------------------------------------------------------------
C
      CALL JELIRA ( '&&RC3200.SITU_NUME_GROUP', 'LONMAX', NBGR, K8B )
      CALL JEVEUO ( '&&RC3200.SITU_NUME_GROUP', 'L', JNUMGR )

      CALL JEVEUO ( '&&RC3200.SITU_NUMERO', 'L', JNSITU )
C
C     -----------------------------------------------------------------
C
        CALL TBAJPA ( NOMRES, NPAR0, NOPAR0, TYPAR0 )
C
        DO 100 IG = 1 , NBGR
          NUMGR = ZI(JNUMGR+IG-1)
          VALEI(1) = NUMGR
          CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONMAX',
     +                                                    NBSIGR,K8B)
          CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)

          DO 102 IM = 1 , 2
            K24B = '&&RC3200.PMPB       '//LIEU(IM)
            CALL JEVEUO(JEXNUM(K24B,NUMGR),'L',JPMPB)
            DO 104 IS = 1 , NBSIGR
              IOC = ZI(JNSG+IS-1)
              VALEI(2) = ZI(JNSITU+IOC-1)

              CALL TBAJLI ( NOMRES, 9, NOPAR4, VALEI,
     +                      ZR(JPMPB-1+6*(IS-1)+1), C16B, LIEU(IM), 0 )
C
 104        CONTINUE
 102      CONTINUE
C
          DO 112 IM = 1 , 2
            VALEK(1) = LIEU(IM)
C
            CALL JEVEUO ( '&&RC3200.RESULTAT  .'//LIEU(IM), 'L', JVALE)
            UTOT = ZR(JVALE+5)
C
            K24T = '&&RC3200.FACT_USAGE '//LIEU(IM)
            CALL JEVEUO(JEXNUM(K24T,NUMGR),'L',JPMPB)
            DO 114 IS = 1 , 50
              I3  = INT( ZR(JPMPB-1+4*(IS-1)+1) )
              IF( I3 .EQ. 0 ) GOTO 116
              IS1 = INT( ZR(JPMPB-1+4*(IS-1)+2) )
              IS2 = INT( ZR(JPMPB-1+4*(IS-1)+3) )
              VALER(1) = ZR(JPMPB-1+4*(IS-1)+4)
              IF ( UTOT .EQ. 0.D0 ) THEN
                VALER(2) = 0.D0
              ELSE
                VALER(2) = 100.D0 * VALER(1) / UTOT
              ENDIF
              IF ( I3.EQ.1 .OR. I3.EQ.3 ) THEN
                K2L = '_A'
              ELSE
                K2L = '_B'
              ENDIF
              IF ( I3.EQ.1 .OR. I3.EQ.2 ) THEN
                K2C = '_A'
              ELSE
                K2C = '_B'
              ENDIF
              CALL CODENT ( IS1, 'D', K6B )
              VALEK(2) = K6B//K2L
              CALL CODENT ( IS2, 'D', K6B )
              VALEK(3) = K6B//K2C
              CALL TBAJLI ( NOMRES, 6,NOPAR5,VALEI,VALER,C16B,VALEK,0 )
C
 114        CONTINUE
 116        CONTINUE
 112      CONTINUE
 100    CONTINUE
C
        DO 110 IM = 1 , 2
C
          CALL JEVEUO ( '&&RC3200.RESULTAT  .'//LIEU(IM), 'L', JVALE )
C
          CALL TBAJLI ( NOMRES, 7, NOPAR6, IBID, ZR(JVALE),
     +                                              C16B, LIEU(IM), 0 )
C
 110    CONTINUE
C
      END
