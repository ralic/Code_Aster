      SUBROUTINE ACEVBA(NBOCC,NLM,NLG,IER)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER           NBOCC,NLM,NLG,IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     VERIFICATION DES MOTS CLES POUR L'ELEMENT BARRE
C ----------------------------------------------------------------------
C IN  : NBOCC  : NOMBRE D'OCCURENCE
C OUT : NLM    : NOMBRE TOTAL DE MAILLE
C OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
C ----------------------------------------------------------------------
C     NSECBA : NOMBRE DE SECTIONS PAR BARRE
C     NTYPSE : NOMBRE DE TYPE DE SECTION
C ----------------------------------------------------------------------
      REAL*8        R8B, TST
      CHARACTER*8   K8B, KIOC, KI, NOMU
      CHARACTER*24 VALK(3)
      CHARACTER*16  K16B, SEC, CONCEP, CMD
      INTEGER      IARG
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IOC ,IRECE ,IRECH ,JCAR ,JCARA ,JEXP 
      INTEGER JPARA ,JSECT ,JTAB ,JTYPE ,JVALE ,L ,NBCAR 
      INTEGER NBO ,NBVAL ,NC ,NCAR ,NCARA ,NCMAX ,NDIM 
      INTEGER NG ,NM ,NS ,NSEC ,NSECBA ,NSOM ,NTYPSE 
      INTEGER NV ,NVAL 
      REAL*8 R8MAEM 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
C
      CALL WKVECT('&&ACEVBA.TAB_PARA','V V I',10,JPARA)
      CALL ACEDAT('BARRE',0,ZI(JPARA),K16B,K8B,K8B,K8B)
      NSECBA = ZI(JPARA  )
      NTYPSE = ZI(JPARA+1)
      NBO    = ZI(JPARA+2)
      NBCAR  = ZI(JPARA+3)
      NBVAL  = ZI(JPARA+4)
      CALL WKVECT('&&ACEVBA.NCP','V V I',NTYPSE,JTYPE)
      DO 2 I = 1,NTYPSE
         ZI(JTYPE+I-1) = ZI(JPARA+4+I)
 2    CONTINUE
      NDIM = ZI(JTYPE+1) * NTYPSE
      CALL WKVECT('&&ACEVBA.TYP_SECT','V V K16',NTYPSE,JSECT)
      CALL WKVECT('&&ACEVBA.EXPBAR'  ,'V V K8 ',NBO   ,JEXP )
      CALL WKVECT('&&ACEVBA.TABBAR'  ,'V V K8 ',NBO   ,JTAB )
      CALL WKVECT('&&ACEVBA.CARBAR'  ,'V V K8 ',NDIM  ,JCAR )
      CALL ACEDAT('BARRE',1,ZI(JPARA),ZK16(JSECT),ZK8(JEXP),ZK8(JTAB),
     &                                                      ZK8(JCAR))
      CALL WKVECT('&&ACEVBA.CARA','V V K8',NBCAR,JCARA)
      CALL WKVECT('&&ACEVBA.VALE','V V R8',NBVAL,JVALE)
C
      TST = R8MAEM()
      NLM = 0
      NLG = 0
      DO 10 IOC = 1 , NBOCC
         CALL CODENT(IOC,'G',KIOC)
         CALL GETVTX('BARRE','GROUP_MA'     ,IOC,IARG,0    ,K8B ,NG)
         CALL GETVTX('BARRE','MAILLE'       ,IOC,IARG,0    ,K8B ,NM)
         CALL GETVTX('BARRE','SECTION'      ,IOC,IARG,0    ,K8B ,NS)
         CALL GETVTX('BARRE','SECTION'      ,IOC,IARG,1    ,SEC ,NSEC)
         CALL GETVTX('BARRE','CARA'         ,IOC,IARG,0    ,K8B ,NC)
         CALL GETVTX('BARRE','CARA'        ,IOC,IARG,NBCAR,
     &                ZK8(JCARA),NCAR)
         CALL GETVR8('BARRE','VALE'         ,IOC,IARG,0    ,R8B ,NV)
         CALL GETVR8('BARRE','VALE',IOC,IARG,NBVAL,
     &               ZR(JVALE),NVAL)
C
C -- CARA
         IF (NCAR.GT.0) THEN
           NCARA = NCAR
           DO 20 L = 1 , NTYPSE
              IF (SEC.EQ.ZK16(JSECT+L-1)) THEN
                 NCMAX = ZI(JTYPE+L-1)*NSECBA
                 CALL CODENT(NCMAX,'G',KI)
                 IF (NCAR.GT.NCMAX .AND. L.NE.2) THEN
                     VALK(1) = KIOC
                     VALK(2) = KI
                     VALK(3) = ZK16(JSECT+L-1)
                     CALL U2MESK('E','MODELISA_44', 3 ,VALK)
                    IER = IER + 1
                 ENDIF
                 IF (L.EQ.2) THEN
                    IF (NCAR.GT.4) THEN
                        VALK(1) = KIOC
                        VALK(2) = ZK16(JSECT+L-1)
                        CALL U2MESK('E','MODELISA_45', 2 ,VALK)
                       IER = IER + 1
                    ENDIF
                    IRECH = 0
                    IRECE = 0
                    DO 30 I = 1,NCAR
                       IF (ZK8(JCARA+I-1)(1:2).EQ.'H ') THEN
                          IF (IRECH.EQ.2) THEN
                              VALK(1) = KIOC
                              VALK(2) = ZK16(JSECT+L-1)
                              CALL U2MESK('E','MODELISA_46', 2 ,VALK)
                             IER = IER + 1
                          ENDIF
                          IRECH = 1
                       ENDIF
                       IF (ZK8(JCARA+I-1)(1:2).EQ.'HY' .OR.
     &                                ZK8(JCARA+I-1)(1:2).EQ.'HZ') THEN
                          IF (IRECH.EQ.1) THEN
                              VALK(1) = KIOC
                              VALK(2) = ZK16(JSECT+L-1)
                              CALL U2MESK('E','MODELISA_47', 2 ,VALK)
                             IER = IER + 1
                          ENDIF
                          IRECH = 2
                       ENDIF
                       IF (ZK8(JCARA+I-1)(1:3).EQ.'EP ') THEN
                           IF (IRECE.EQ.1) THEN
                              VALK(1) = KIOC
                              VALK(2) = ZK16(JSECT+L-1)
                              CALL U2MESK('E','MODELISA_48', 2 ,VALK)
                              IER = IER + 1
                           ENDIF
                           IRECE = 2
                        ENDIF
                        IF (ZK8(JCARA+I-1)(1:3).EQ.'EPX' .OR.
     &                               ZK8(JCARA+I-1)(1:3).EQ.'EPY') THEN
                           IF (IRECE.EQ.2) THEN
                              VALK(1) = KIOC
                              VALK(2) = ZK16(JSECT+L-1)
                              CALL U2MESK('E','MODELISA_49', 2 ,VALK)
                              IER = IER + 1
                           ENDIF
                           IRECE = 1
                        ENDIF
 30                  CONTINUE
                  ENDIF
               ENDIF
 20         CONTINUE
         ENDIF
C
C -- VALE
         IF (NVAL.GT.0) THEN
            IF (NVAL.NE.NCARA) THEN
               CALL CODENT(NCARA,'G',KI)
                VALK(1) = KIOC
                VALK(2) = KI
                CALL U2MESK('E','MODELISA_50', 2 ,VALK)
               IER = IER + 1
            ELSE
               DO 70 I = 1 , NVAL
                  CALL CODENT(I,'G',KI)
                  IF (ZR(JVALE+I-1).EQ.TST) THEN
                      VALK(1) = KIOC
                      VALK(2) = ZK16(JSECT+L-1)
                      VALK(3) = KI
                      CALL U2MESK('E','MODELISA_51', 3 ,VALK)
                     IER = IER + 1
                  ENDIF
 70            CONTINUE
            ENDIF
         ENDIF
C
C ---    GROUP_MA + GROUP_NO + NOEUD + MAILLE
         NSOM = NG + NM
         IF (NSOM.EQ.NG .OR. NSOM.EQ.NM) THEN
            NLM = MAX(NLM,-NM)
            NLG = MAX(NLG,-NG)
         ENDIF
C
 10   CONTINUE
C
      CALL JEDETR('&&ACEVBA.TAB_PARA')
      CALL JEDETR('&&ACEVBA.NCP')
      CALL JEDETR('&&ACEVBA.TYP_SECT')
      CALL JEDETR('&&ACEVBA.EXPBAR')
      CALL JEDETR('&&ACEVBA.TABBAR')
      CALL JEDETR('&&ACEVBA.CARBAR')
      CALL JEDETR('&&ACEVBA.CARA')
      CALL JEDETR('&&ACEVBA.VALE')
C
      CALL JEDEMA()
      END
