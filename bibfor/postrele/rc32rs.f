      SUBROUTINE RC32RS ( PMPB, SN, FATIGU )
      IMPLICIT   NONE
      LOGICAL             PMPB, SN, FATIGU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 23/02/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER       IBID, NPAR0, NPAR1, NPAR2, NPAR3, IM, IG, IS, I3,
     +              NBSIGR, VALEI(2), JNUMGR, JNSITU, JNSG, JVALE,
     +              JPMPB, NBGR, IOC, NUMGR, IS1, IS2 
      PARAMETER    ( NPAR0 = 17 , NPAR1 = 6 , NPAR2 = 4 , NPAR3 = 7 )
      REAL*8        UTOT, VALER(2)
      COMPLEX*16    C16B
      CHARACTER*2   K2C, K2L
      CHARACTER*4   LIEU(2)
      CHARACTER*6   K6B
      CHARACTER*8   K8B, NOMRES, TYPAR0(NPAR0), TYPAR1(NPAR1),
     +              TYPAR2(NPAR2), TYPAR3(NPAR3), VALEK(3)
      CHARACTER*16  CONCEP, NOMCMD, NOPAR0(NPAR0), NOPAR1(NPAR1),
     +              NOPAR2(NPAR2), NOPAR3(NPAR3),
     +              NOPAR5(6), NOPAR4(8), NOPAR6(7)
      CHARACTER*24 K24B, K24T
C     ------------------------------------------------------------------
      DATA NOPAR0 / 'NUME_GROUPE', 'LIEU', 'NUME_SITU_1', 'NUME_SITU_2',
     +              'NUME_SITU', 'PM' , 'PB' , 'PMPB', 'SN' , 
     +              'FACT_USAGE' , '%_FACT_USAGE', 'SM' , 'SN/3SM' ,
     +              'SN_MAX' , 'SP_MAX' , 'SALT_MAX' ,
     +              'FACT_USAGE_CUMU' /
      DATA TYPAR0 / 'I', 'K8', 'K8', 'K8','I', 'R', 'R', 'R', 'R', 
     +              'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R' /
      DATA LIEU   / 'ORIG' , 'EXTR' /
C
C --- QUE DU PMPB
      DATA NOPAR1 / 'NUME_GROUPE', 'LIEU', 'NUME_SITU' , 'PM' , 'PB' ,
     +              'PMPB' /
      DATA TYPAR1 / 'I', 'K8', 'I', 'R', 'R', 'R' /
C
C --- QUE DU SN
      DATA NOPAR2 / 'NUME_GROUPE', 'LIEU', 'NUME_SITU' , 'SN' /
      DATA TYPAR2 / 'I', 'K8', 'I', 'R' /
C
C --- QUE DU PMPB ET DU SN
      DATA NOPAR3 / 'NUME_GROUPE', 'LIEU', 'NUME_SITU', 'PM', 'PB',
     +              'PMPB', 'SN'  /
      DATA TYPAR3 / 'I', 'K8', 'I', 'R', 'R', 'R', 'R'  /
C    
C --- POUR LA FATIGUE: PAR SITUATION
      DATA NOPAR4 / 'NUME_GROUPE', 'LIEU' , 'NUME_SITU', 'PM' , 'PB' ,
     +              'PMPB', 'SN', 'FACT_USAGE'   /
C
C --- POUR LA FATIGUE: FACTEUR D'USAGE PAR SITUATION
      DATA NOPAR5 / 'NUME_GROUPE', 'LIEU', 'NUME_SITU_1', 
     +              'NUME_SITU_2', 'FACT_USAGE', '%_FACT_USAGE'  /
C    
C --- POUR LA FATIGUE: 
      DATA NOPAR6 / 'LIEU',  'SM' , 'SN/3SM' , 'SN_MAX' , 'SP_MAX' ,
     +              'SALT_MAX' , 'FACT_USAGE_CUMU' /
C DEB ------------------------------------------------------------------
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
      CALL JELIRA ( '&&RC3200.SITU_NUME_GROUP', 'LONMAX', NBGR, K8B )
      CALL JEVEUO ( '&&RC3200.SITU_NUME_GROUP', 'L', JNUMGR )

      CALL JEVEUO ( '&&RC3200.SITU_NUMERO', 'L', JNSITU )
C
      CALL TBCRSD ( NOMRES, 'G' )
C
C     -----------------------------------------------------------------
C
      IF ( FATIGU ) THEN
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

              CALL TBAJLI ( NOMRES, 8, NOPAR4, VALEI,
     +                      ZR(JPMPB-1+5*(IS-1)+1), C16B, LIEU(IM), 0 )
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
C     -----------------------------------------------------------------
      ELSEIF ( PMPB .AND. SN ) THEN
C
        CALL TBAJPA ( NOMRES, NPAR3, NOPAR3, TYPAR3 )
C
        DO 200 IG = 1 , NBGR
          NUMGR = ZI(JNUMGR+IG-1)
          VALEI(1) = NUMGR
          CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONMAX',
     +                                                    NBSIGR,K8B)
          CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)

          DO 202 IS = 1 , NBSIGR
            IOC = ZI(JNSG+IS-1)
            VALEI(2) = ZI(JNSITU+IOC-1)
            DO 204 IM = 1 , 2
              K24B = '&&RC3200.PMPB       '//LIEU(IM)
              CALL JEVEUO ( JEXNUM(K24B,NUMGR), 'L', JPMPB )

              CALL TBAJLI ( NOMRES, NPAR3, NOPAR3, VALEI,
     +                      ZR(JPMPB-1+5*(IS-1)+1), C16B, LIEU(IM), 0 )
C
 204        CONTINUE
 202      CONTINUE
 200    CONTINUE
C
C     -----------------------------------------------------------------
      ELSEIF ( PMPB ) THEN
C
        CALL TBAJPA ( NOMRES, NPAR1, NOPAR1, TYPAR1 )
C
        DO 300 IG = 1 , NBGR
          NUMGR = ZI(JNUMGR+IG-1)
          VALEI(1) = NUMGR
          CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONMAX',
     +                                                    NBSIGR,K8B)
          CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)

          DO 302 IS = 1 , NBSIGR
            IOC = ZI(JNSG+IS-1)
            VALEI(2) = ZI(JNSITU+IOC-1)
            DO 304 IM = 1 , 2
              K24B = '&&RC3200.PMPB       '//LIEU(IM)
              CALL JEVEUO ( JEXNUM(K24B,NUMGR), 'L', JPMPB )

              CALL TBAJLI ( NOMRES, NPAR1, NOPAR1, VALEI,
     +                      ZR(JPMPB-1+5*(IS-1)+1), C16B, LIEU(IM), 0 )
C
 304        CONTINUE
 302      CONTINUE
 300    CONTINUE
C
C     -----------------------------------------------------------------
      ELSEIF ( SN ) THEN
C
        CALL TBAJPA ( NOMRES, NPAR2, NOPAR2, TYPAR2 )
C
        DO 400 IG = 1 , NBGR
          NUMGR = ZI(JNUMGR+IG-1)
          VALEI(1) = NUMGR
          CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONMAX',
     +                                                    NBSIGR,K8B)
          CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)

          DO 402 IS = 1 , NBSIGR
            IOC = ZI(JNSG+IS-1)
            VALEI(2) = ZI(JNSITU+IOC-1)
            DO 404 IM = 1 , 2
              K24B = '&&RC3200.PMPB       '//LIEU(IM)
              CALL JEVEUO ( JEXNUM(K24B,NUMGR), 'L', JPMPB )

              CALL TBAJLI ( NOMRES, NPAR2, NOPAR2, VALEI,
     +                      ZR(JPMPB-1+5*(IS-1)+4), C16B, LIEU(IM), 0 )
C
 404        CONTINUE
 402      CONTINUE
 400    CONTINUE
C
      ENDIF
C
      END
