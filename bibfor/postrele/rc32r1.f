      SUBROUTINE RC32R1 ( NOMRES )
      IMPLICIT   NONE
      CHARACTER*8         NOMRES
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/10/2006   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER       IBID, NPAR2, NPAR1, NPAR4, NPAR6, IM, IG, IS, I3,
     +              IRET, NBSIGR, VALEI(2), JNUMGR, JNSITU, JNSG, JVALE,
     +              JPMPB, NBGR, IOC, NUMGR, IS1, IS2, JREAS, JRESS, N1,
     +              JSEIGR, JCOMBI, IOC1, IOC2, IOCS, II, NPAR0
      PARAMETER    ( NPAR0 = 39, NPAR2 = 7, NPAR1 = 15, NPAR4 = 15, 
     +               NPAR6 = 13 )
      REAL*8        UTOT, VALER(2)
      COMPLEX*16    C16B
      CHARACTER*2   K2C, K2L
      CHARACTER*4   LIEU(2)
      CHARACTER*6   K6B
      CHARACTER*8   K8B, VALEK(4), TYPAR0(NPAR0), TYPAR6(NPAR6), TYPTAB
      CHARACTER*16  NOPAR2(NPAR2), NOPAR1(NPAR1), NOPAR4(NPAR4),
     +              NOPAR6(NPAR6), NOPAR0(NPAR0)
      CHARACTER*24  K24B, K24C, K24T
C     ------------------------------------------------------------------
      DATA LIEU   / 'ORIG' , 'EXTR' /
C
      DATA NOPAR0 / 'TYPE', 'SEISME', 'NUME_GROUPE', 'LIEU' ,  
     +              'PM_MAX', 'PB_MAX', 'PMPB_MAX', 'SM' ,
     +              'SN/3SM' , 'SN_MAX' , 'SN*_MAX' , 'SP_MAX',
     +              'KE_MAX', 'SALT_MAX', 'FACT_USAGE_CUMU',
     +              'NUME_SITU', 'NUME_SITU_I', 'NUME_SITU_J',
     +              'PM' , 'PB' , 'PMPB', 'SN', 'SN*', 'SP', 'KE_MECA',
     +              'KE_THER', 'SALT', 'NUME_SITU_K', 'NUME_SITU_L',
     +              'FACT_USAGE',  '%_FACT_USAGE' ,
     +              'SP_ETAT_A_A', 'SALT_ETAT_A_A' ,
     +              'SP_ETAT_B_A', 'SALT_ETAT_B_A' ,
     +              'SP_ETAT_A_B', 'SALT_ETAT_A_B' ,
     +              'SP_ETAT_B_B', 'SALT_ETAT_B_B' /
      DATA TYPAR0 / 'K8', 'K8', 'I', 'K8',  'R', 'R', 'R', 'R', 'R',
     +              'R', 'R', 'R', 'R', 'R', 'R', 'I', 'I', 'I', 'R',
     +              'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'K8', 'K8', 
     +              'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R'  /
C
C --- PARAMETRES FACTEUR D'USAGE
C
      DATA NOPAR2 / 'TYPE', 'NUME_GROUPE', 'LIEU', 'NUME_SITU_K', 
     +              'NUME_SITU_L', 'FACT_USAGE' , '%_FACT_USAGE' /
C
C --- PARAMETRES POUR LE CALCUL DU FACTEUR D'USAGE
C
      DATA NOPAR1 / 'TYPE', 'SEISME', 'NUME_GROUPE', 'LIEU', 
     +              'NUME_SITU_I', 'NUME_SITU_J', 'SN' ,
     +                     'SP_ETAT_A_A', 'SALT_ETAT_A_A' ,
     +                     'SP_ETAT_B_A', 'SALT_ETAT_B_A' ,
     +                     'SP_ETAT_A_B', 'SALT_ETAT_A_B' ,
     +                     'SP_ETAT_B_B', 'SALT_ETAT_B_B' /
C    
C --- PARAMETRES POUR CHAQUE SITUATION
C
      DATA NOPAR4 / 'TYPE', 'SEISME', 'NUME_GROUPE', 'LIEU' , 
     +              'NUME_SITU', 'PM' , 'PB' , 'PMPB', 'SN', 'SN*', 
     +              'SP', 'KE_MECA', 'KE_THER', 'SALT', 'FACT_USAGE'  /
C
C --- PARAMETRES POUR LES MAXIMA
C
      DATA NOPAR6 / 'TYPE', 'LIEU', 'PM_MAX', 'PB_MAX', 'PMPB_MAX',
     +              'SM' , 'SN/3SM' , 'SN_MAX' , 'SN*_MAX' , 'SP_MAX',
     +              'KE_MAX', 'SALT_MAX', 'FACT_USAGE_CUMU' /
C
      DATA TYPAR6 / 'K8', 'K8', 'R', 'R', 'R', 'R', 'R', 'R',
     +                          'R', 'R', 'R', 'R', 'R'  /
C DEB ------------------------------------------------------------------
C
      CALL GETVTX ( ' ', 'TYPE_RESU', 1,1,1, TYPTAB , N1 )
C
      CALL JELIRA ( '&&RC3200.SITU_NUME_GROUP', 'LONMAX', NBGR, K8B )
      CALL JEVEUO ( '&&RC3200.SITU_NUME_GROUP', 'L', JNUMGR )
      CALL JEVEUO ( '&&RC3200.SITU_SEISME'    , 'L', JSEIGR )

      CALL JEVEUO ( '&&RC3200.SITU_NUMERO', 'L', JNSITU )
      CALL JEVEUO ( '&&RC3200.SITU_COMBINABLE', 'L', JCOMBI )
C
C     ------------------------------------------------------------------
C
      IF ( TYPTAB .EQ. 'VALE_MAX' ) THEN
         CALL TBAJPA ( NOMRES, NPAR6, NOPAR6, TYPAR6 )
      ELSE
         CALL TBAJPA ( NOMRES, NPAR0, NOPAR0, TYPAR0 )
      ENDIF
C
C     -----------------------------------------------------------------
C     LES MAXIMUM DES QUANTITES
C     -----------------------------------------------------------------
C
      VALEK(1) = 'MAXI'
      DO 110 IM = 1 , 2
C
         VALEK(2) = LIEU(IM)
C
         CALL JEVEUO ( '&&RC3200.RESULTAT  .'//LIEU(IM), 'L', JVALE )
C
         CALL TBAJLI ( NOMRES, NPAR6, NOPAR6, IBID, ZR(JVALE),
     +                                              C16B, VALEK, 0 )
C
 110  CONTINUE
C
      IF ( TYPTAB .EQ. 'VALE_MAX' ) GOTO 9999
C
C     -----------------------------------------------------------------
C
      DO 100 IG = 1 , NBGR
         NUMGR = ZI(JNUMGR+IG-1)
         IOCS  = ZI(JSEIGR+IG-1)
         VALEI(1) = NUMGR
         CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONMAX',
     +                                                    NBSIGR,K8B)
         CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)

C        --------------------------------------------------------------
C        QUANTITE POUR CHAQUE SITUATION
C        --------------------------------------------------------------
         VALEK(1) = 'SITU'
         DO 102 IM = 1 , 2
            VALEK(3) = LIEU(IM)
            K24B = '&&RC3200.AVEC_SEISME'//LIEU(IM)
            CALL JEVEUO(JEXNUM(K24B,NUMGR),'L',JREAS)
            K24C = '&&RC3200.SANS_SEISME'//LIEU(IM)
            CALL JEVEUO(JEXNUM(K24C,NUMGR),'L',JRESS)
            VALEK(2) = 'AVEC'
            DO 104 IS = 1 , NBSIGR
              IOC = ZI(JNSG+IS-1)
              VALEI(2) = ZI(JNSITU+IOC-1)
              CALL TBAJLI ( NOMRES, NPAR4, NOPAR4, VALEI,
     +                      ZR(JREAS-1+10*(IS-1)+1), C16B, VALEK, 0 )
 104        CONTINUE
            VALEK(2) = 'SANS'
            DO 106 IS = 1 , NBSIGR
              IOC = ZI(JNSG+IS-1)
              VALEI(2) = ZI(JNSITU+IOC-1)
              CALL TBAJLI ( NOMRES, NPAR4, NOPAR4, VALEI,
     +                      ZR(JRESS-1+10*(IS-1)+1), C16B, VALEK, 0 )
 106        CONTINUE
 102     CONTINUE
C
C        --------------------------------------------------------------
C        QUANTITE POUR CHAQUE COMBINAISON
C        --------------------------------------------------------------
C
          VALEK(1) = 'COMB'
          DO 120 IM = 1 , 2
C
            VALEK(3) = LIEU(IM)
            K24B = '&&RC3200.COMBI_A_SEI'//LIEU(IM)
            CALL JEVEUO(JEXNUM(K24B,NUMGR),'L',JREAS)
            K24C = '&&RC3200.COMBI_S_SEI'//LIEU(IM)
            CALL JEVEUO(JEXNUM(K24C,NUMGR),'L',JRESS)
C
            II = 0
            DO 122 IS1 = 1,NBSIGR
               IOC1 = ZI(JNSG+IS1-1)
               IF (.NOT.ZL(JCOMBI+IOC1-1)) GO TO 122
               IF (IOC1.EQ.IOCS) GO TO 122
               VALEI(2) = ZI(JNSITU+IOC1-1)
               DO 124 IS2 = IS1 + 1,NBSIGR
                  IOC2 = ZI(JNSG+IS2-1)
                  IF (.NOT.ZL(JCOMBI+IOC2-1)) GO TO 124
                  IF (IOC2.EQ.IOCS) GO TO 124
                  VALEI(3) = ZI(JNSITU+IOC2-1)
C
                  VALEK(2) = 'AVEC'
                  CALL TBAJLI ( NOMRES, NPAR1, NOPAR1, VALEI,
     +                          ZR(JREAS+II), C16B, VALEK, 0 )
                  VALEK(2) = 'SANS'
                  CALL TBAJLI ( NOMRES, NPAR1, NOPAR1, VALEI,
     +                          ZR(JRESS+II), C16B, VALEK, 0 )

                  II = II + 9
 124           CONTINUE

 122        CONTINUE

 120     CONTINUE
C
C        --------------------------------------------------------------
C        FACTEUR D'USAGE
C        --------------------------------------------------------------
C
          VALEK(1) = 'FACT'
          DO 112 IM = 1 , 2
            VALEK(2) = LIEU(IM)
C
            CALL JEVEUO ( '&&RC3200.RESULTAT  .'//LIEU(IM), 'L', JVALE)
            UTOT = ZR(JVALE+7)
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
              VALEK(3) = K6B//K2L
              CALL CODENT ( IS2, 'D', K6B )
              VALEK(4) = K6B//K2C
              CALL TBAJLI ( NOMRES, NPAR2, NOPAR2, VALEI, VALER,
     +                                                C16B, VALEK, 0 )
C
 114        CONTINUE
 116        CONTINUE
 112      CONTINUE
 100    CONTINUE
C
 9999 CONTINUE
C
      END
