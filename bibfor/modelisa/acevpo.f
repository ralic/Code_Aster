      SUBROUTINE ACEVPO(NBOCC,NLM,NLG,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBOCC,NLM,NLG,IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 01/03/2000   AUTEUR CIBHHPD P.DAVID 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     VERIFICATION DES MOTS CLES POUR L'ELEMENT POUTRE
C ----------------------------------------------------------------------
C IN  : NBOCC  : NOMBRE D'OCCURENCE
C OUT : NLM    : NOMBRE TOTAL DE MAILLE
C OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
C ----------------------------------------------------------------------
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      REAL*8        R8B, R8MAME, TST
      CHARACTER*8   K8B, NOMU, KIOC, KI
      CHARACTER*16  K16B, SEC, TOU, CONCEP, CMD
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
C
      CALL WKVECT('&&ACEVPO.TAB_PARA','V V I',10,JPARA)
      CALL ACEDAT('POUTRE',0,ZI(JPARA),K16B,K8B,K8B,K8B)
      NSECPO = ZI(JPARA  )
      NTYPSE = ZI(JPARA+1)
      NBO    = ZI(JPARA+2)
      NBCAR  = ZI(JPARA+3)
      NBVAL  = ZI(JPARA+4)
      CALL WKVECT('&&ACEVPO.NCP','V V I',NTYPSE,JTYPE)
      DO 2 I = 1,NTYPSE
         ZI(JTYPE+I-1) = ZI(JPARA+4+I)
 2    CONTINUE
      NDIM = ZI(JTYPE) * ( NSECPO + 1 )
      CALL WKVECT('&&ACEVPO.TYP_SECT','V V K16',NTYPSE     ,JSECT)
      CALL WKVECT('&&ACEVPO.EXPPOU'  ,'V V K8 ',NBO        ,JEXP )
      CALL WKVECT('&&ACEVPO.TABPOU'  ,'V V K8 ',NBO        ,JTAB )
      CALL WKVECT('&&ACEVPO.CARPOU'  ,'V V K8 ',NDIM*NTYPSE,JCAR )
      CALL ACEDAT('POUTRE',1,ZI(JPARA),ZK16(JSECT),ZK8(JEXP),ZK8(JTAB),
     +                                                      ZK8(JCAR))
      CALL WKVECT('&&ACEVPO.COMPOU','V V I' ,NTYPSE,JCOM )
      CALL WKVECT('&&ACEVPO.CARA'  ,'V V K8',NBCAR ,JCARA)
      CALL WKVECT('&&ACEVPO.VALE'  ,'V V R8',NBVAL ,JVALE)
C
      TST = R8MAEM()
      NLM = 0
      NLG = 0
      DO 10 IOC = 1 , NBOCC
         CALL CODENT(IOC,'G',KIOC)
         CALL GETVID('POUTRE','GROUP_MA'    ,IOC,1,0    ,K8B ,NG)
         CALL GETVID('POUTRE','MAILLE'      ,IOC,1,0    ,K8B ,NM)
         CALL GETVTX('POUTRE','SECTION'     ,IOC,1,0    ,K8B ,NS)
         CALL GETVTX('POUTRE','SECTION'     ,IOC,1,1    ,SEC ,NSEC)
         CALL GETVTX('POUTRE','CARA'       ,IOC,1,0    ,K8B ,NC)
         CALL GETVTX('POUTRE','CARA'       ,IOC,1,NBCAR,
     +                ZK8(JCARA),NCAR)
         CALL GETVR8('POUTRE','VALE'        ,IOC,1,0    ,R8B ,NV)
         CALL GETVR8('POUTRE','VALE'        ,IOC,1,NBVAL,
     +                ZR(JVALE),NVAL)
C
C -- IOC = 1
         IF (IOC.EQ.1) THEN
            IF (NV.EQ.0) THEN
               CALL UTMESS('E',CMD,'POUTRE : OCCURENCE 1 : LE MOT '//
     +                                     'CLE "VALE" EST OBLIGATOIRE')
               IER = IER + 1
            ENDIF
            IF (NS.EQ.0) THEN
               CALL UTMESS('E',CMD,'POUTRE : OCCURENCE 1 : LE MOT '//
     +                                 'CLE "SECTION" EST OBLIGATOIRE')
               IER = IER + 1
            ENDIF
            IF (NC.EQ.0) THEN
               CALL UTMESS('E',CMD,'POUTRE : OCCURENCE 1 : LE MOT '//
     +                                   'CLE "CARA" EST OBLIGATOIRE')
               IER = IER + 1
            ENDIF
         ENDIF
C
C -- SECTION
         IF (NSEC.GT.0) THEN
            IF (NCAR.EQ.0) THEN
               CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//KIOC//' : '//
     +     ' PRESENCE DE  "CARA" OBLIGATOIRE SI "SECTION" EST PRESENT')
               IER = IER + 1
            ENDIF
         ENDIF
C
C -- CARA
         IF (NCAR.GT.0) THEN
           NCARA = NCAR
           IF (NVAL.EQ.0) THEN
              CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//KIOC//' : '//
     +         ' PRESENCE DE "VALE" OBLIGATOIRE SI "CARA" EST PRESENT')
              IER = IER + 1
           ENDIF
           DO 20 L = 1 , NTYPSE
              IF (SEC.EQ.ZK16(JSECT+L-1)) THEN
                 NCMAX = ZI(JTYPE+L-1)*NSECPO
                 CALL CODENT(NCMAX,'G',KI)
                 IF (NCAR.GT.NCMAX .AND. L.NE.2) THEN
                    CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//KIOC//
     +                         ' : "CARA" : '//KI//' ARGUMENTS MAXI'//
     +                     ' POUR UNE SECTION "'//ZK16(JSECT+L-1)//'"')
                    IER = IER + 1
                 ENDIF
C
                 IF (L.EQ.2) THEN
                    IF (NCAR.GT.8) THEN
                       CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//KIOC//
     +                                ' : "CARA" : 8 ARGUMENTS MAXI'//
     +                     ' POUR UNE SECTION "'//ZK16(JSECT+L-1)//'"')
                       IER = IER + 1
                    ENDIF
                    IRECH = 0
                    IRECE = 0
                    DO 30 I = 1,NCAR
                       IF ( ZK8(JCARA+I-1)(1:2).EQ.'H ' .OR.
     +                      ZK8(JCARA+I-1)(1:2).EQ.'H1' .OR.
     +                      ZK8(JCARA+I-1)(1:2).EQ.'H2' ) THEN
                          IF (IRECH.EQ.2) THEN
                             CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//
     +                          KIOC//' : SECTION "'//ZK16(JSECT+L-1)//
     +                   ' ARGUMENT "H" INCOMPATIBLE AVEC "HY" OU "HZ"')
                             IER = IER + 1
                          ENDIF
                          IRECH = 1
                       ENDIF
                       IF ( ZK8(JCARA+I-1)(1:2).EQ.'HY' .OR.
     +                      ZK8(JCARA+I-1)(1:2).EQ.'HZ' ) THEN
                          IF (IRECH.EQ.1) THEN
                             CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//
     +                          KIOC//' : SECTION "'//ZK16(JSECT+L-1)//
     +                  ' ARGUMENT "HY" OU "HZ" INCOMPATIBLE AVEC "H" ')
                             IER = IER + 1
                          ENDIF
                          IRECH = 2
                       ENDIF
                       IF ( ZK8(JCARA+I-1)(1:3).EQ.'EP ' .OR.
     +                      ZK8(JCARA+I-1)(1:3).EQ.'EP1' .OR.
     +                      ZK8(JCARA+I-1)(1:3).EQ.'EP2' ) THEN
                           IF (IRECE.EQ.1) THEN
                             CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//
     +                          KIOC//' : SECTION "'//ZK16(JSECT+L-1)//
     +                ' ARGUMENT "EP" INCOMPATIBLE AVEC "EPY" OU "EPZ"')
                              IER = IER + 1
                           ENDIF
                           IRECE = 2
                        ENDIF
                        IF ( ZK8(JCARA+I-1)(1:3).EQ.'EPX' .OR.
     +                       ZK8(JCARA+I-1)(1:3).EQ.'EPY' ) THEN
                           IF (IRECE.EQ.2) THEN
                             CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//
     +                          KIOC//' : SECTION "'//ZK16(JSECT+L-1)//
     +                ' ARGUMENT "EPY" OU "EPZ" INCOMPATIBLE AVEC "EP"')
                              IER = IER + 1
                           ENDIF
                           IRECE = 1
                        ENDIF
 30                  CONTINUE
                  ENDIF
C
                  ZI(JCOM+L-1) = -1
                  DO 40 I = 1 , NCAR
                     CALL CODENT(I,'G',KI)
                     DO 50 J = 1 , ZI(JTYPE+L-1)
                   IF (ZK8(JCARA+I-1).EQ.ZK8(JCAR+J+NDIM*(L-1)-1)) THEN
                           IF (ZI(JCOM+L-1).EQ.1) THEN
                             CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//
     +                         KIOC//' : ARGUMENT '//KI//'DE "CARA" '//
     +                           'INCOMPATIBLE AVEC LE(S) PRECEDENT(S)')
                              IER = IER + 1
                           ENDIF
                           ZI(JCOM+L-1) = 0
                        ENDIF
 50                  CONTINUE
                     DO 60 J = ZI(JTYPE+L-1)+1,ZI(JTYPE+L-1)*(NSECPO+1)
                   IF (ZK8(JCARA+I-1).EQ.ZK8(JCAR+J+NDIM*(L-1)-1)) THEN
                           IF (ZI(JCOM+L-1).EQ.0) THEN
                             CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//
     +                        KIOC//' : ARGUMENT '//KI//'DE "CARA" '//
     +                          'INCOMPATIBLE AVEC LE(S) PRECEDENT(S)')
                              IER = IER + 1
                           ENDIF
                           ZI(JCOM+L-1) = 1
                        ENDIF
 60                  CONTINUE
 40               CONTINUE
               ENDIF
 20         CONTINUE
         ENDIF
C
C -- VALE
         IF (NVAL.GT.0) THEN
            IF (NVAL.NE.NCARA) THEN
               CALL CODENT(NCARA,'G',KI)
               CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//KIOC//' : '//
     +              '"CARA" : NOMBRE DE VALEURS ENTREES INCORRECT :'//
     +                                              ' IL EN FAUT '//KI)
               IER = IER + 1
            ELSE
               DO 70 I = 1 , NVAL
                  CALL CODENT(I,'G',KI)
                  IF (ZR(JVALE+I-1).EQ.TST) THEN
                     CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//KIOC//
     +              ' : SECTION "'//ZK16(JSECT+L-1)//' : VALEUR '//KI//
     +                    ' DE "VALE" NON ADMISE (VALEUR TEST INTERNE)')
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
      CALL JEDETR('&&ACEVPO.TAB_PARA')
      CALL JEDETR('&&ACEVPO.NCP')
      CALL JEDETR('&&ACEVPO.TYP_SECT')
      CALL JEDETR('&&ACEVPO.EXPPOU')
      CALL JEDETR('&&ACEVPO.TABPOU')
      CALL JEDETR('&&ACEVPO.CARPOU')
      CALL JEDETR('&&ACEVPO.COMPOU')
      CALL JEDETR('&&ACEVPO.CARA')
      CALL JEDETR('&&ACEVPO.VALE')
C
      CALL JEDEMA()
      END
