      SUBROUTINE TBIMFI ( NPARFI, TABLE, NEWTAB, IRET )
      IMPLICIT   NONE
      INTEGER             NPARFI, IRET
      CHARACTER*19        TABLE, NEWTAB
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     OPERATEUR  IMPR_TABLE , TRAITEMENT DU MOT CLE FACTEUR "FILTRE"
C     ------------------------------------------------------------------
C
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
      INTEGER      IBID, LTITR, JTITR, ITITR, II, IR, IC, IK, IOC,
     +             LONMAX, LONMA1, JPAFI, JCCFI, JVIFI, JVRFI, JVCFI,
     +             JVKFI, JPRFI, JCRFI, L , L1, L2, L3, L4, IRT
      REAL*8       R8B
      COMPLEX*16   CBID
      CHARACTER*8  K8B
      CHARACTER*80 MONTIT
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL JEEXIN ( TABLE//'.TITR', IRT )
      IF ( IRT .NE. 0 ) THEN
         CALL JEVEUO ( TABLE//'.TITR', 'L', LTITR )
         CALL JELIRA ( TABLE//'.TITR', 'LONMAX', LONMA1, K8B )
         LONMAX = LONMA1 + NPARFI
         CALL WKVECT ( NEWTAB//'.TITR', 'V V K80', LONMAX, JTITR )
         DO 10 ITITR = 1 , LONMA1
            ZK80(JTITR+ITITR-1) = ZK80(LTITR+ITITR-1)
 10      CONTINUE
      ELSE
         LONMA1 = 0
         LONMAX = LONMA1 + NPARFI
         CALL WKVECT ( NEWTAB//'.TITR', 'V V K80', LONMAX, JTITR )
      ENDIF
C
      CALL WKVECT ('&&TBIMFI.NOMS_PARA', 'V V K24', NPARFI, JPAFI )
      CALL WKVECT ('&&TBIMFI.CRIT_PARA', 'V V K8' , NPARFI, JCCFI )
      CALL WKVECT ('&&TBIMFI.VALE_I'   , 'V V I'  , NPARFI, JVIFI )
      CALL WKVECT ('&&TBIMFI.VALE_R'   , 'V V R'  , NPARFI, JVRFI )
      CALL WKVECT ('&&TBIMFI.VALE_C'   , 'V V C'  , NPARFI, JVCFI )
      CALL WKVECT ('&&TBIMFI.VALE_K'   , 'V V K80', NPARFI, JVKFI )
      CALL WKVECT ('&&TBIMFI.PRECISION', 'V V R'  , NPARFI, JPRFI )
      CALL WKVECT ('&&TBIMFI.CRITERE'  , 'V V K8' , NPARFI, JCRFI )
C
      II = -1
      IR = -1
      IC = -1
      IK = -1
C
      DO 20 IOC = 1 , NPARFI
         CALL GETVTX ('FILTRE','NOM_PARA',IOC,IARG,1,
     &                ZK24(JPAFI+IOC-1),L)
         CALL GETVTX ('FILTRE','CRIT_COMP',IOC,IARG,1,
     &                ZK8(JCCFI+IOC-1) ,L)
         MONTIT = ' '
         CALL GETVIS ('FILTRE', 'VALE_I' , IOC,IARG,0, IBID, L1 )
         CALL GETVR8 ('FILTRE', 'VALE'   , IOC,IARG,0, R8B , L2 )
         CALL GETVC8 ('FILTRE', 'VALE_C' , IOC,IARG,0, CBID, L3 )
         CALL GETVTX ('FILTRE', 'VALE_K' , IOC,IARG,0, K8B , L4 )
         IF ( L1 .NE. 0 ) THEN
            II = II + 1
            CALL GETVIS ('FILTRE','VALE_I',IOC,IARG,1,
     &                   ZI(JVIFI+II), L )
            WRITE(MONTIT,1010) ZK24(JPAFI+IOC-1), ZK8(JCCFI+IOC-1),
     +                         ZI(JVIFI+II)
         ENDIF
         IF ( L2 .NE. 0 ) THEN
            IR = IR + 1
            CALL GETVR8 ('FILTRE','VALE',IOC,IARG,1,
     &                   ZR(JVRFI+IR) ,L)
            CALL GETVR8 ('FILTRE','PRECISION',IOC,IARG,1,
     &                   ZR(JPRFI+IR) ,L)
            CALL GETVTX ('FILTRE','CRITERE',IOC,IARG,1,
     &                   ZK8(JCRFI+IR),L)
            WRITE(MONTIT,1020) ZK24(JPAFI+IOC-1), ZK8(JCCFI+IOC-1),
     +                         ZR(JVRFI+IR)
         ENDIF
         IF ( L3 .NE. 0 ) THEN
            IC = IC + 1
            CALL GETVC8 ('FILTRE','VALE_C', IOC,IARG,1, ZC(JVCFI+IC),L)
            WRITE(MONTIT,1030) ZK24(JPAFI+IOC-1), ZK8(JCCFI+IOC-1),
     +                         ZC(JVCFI+IC)
         ENDIF
         IF ( L4 .NE. 0 ) THEN
            IK = IK + 1
            CALL GETVTX ('FILTRE','VALE_K',IOC,IARG,1,
     &                   ZK80(JVKFI+IK),L)
            WRITE(MONTIT,1040) ZK24(JPAFI+IOC-1), ZK8(JCCFI+IOC-1),
     +                         ZK80(JVKFI+IK)
         ENDIF
         ZK80(JTITR+LONMA1+IOC-1) = MONTIT
 20   CONTINUE
C
      CALL TBEXTB ( TABLE, 'V', NEWTAB, NPARFI, ZK24(JPAFI),
     +              ZK8(JCCFI), ZI(JVIFI), ZR(JVRFI), ZC(JVCFI),
     +              ZK80(JVKFI), ZR(JPRFI), ZK8(JCRFI), IRET )
C
      CALL JEDETR ('&&TBIMFI.NOMS_PARA' )
      CALL JEDETR ('&&TBIMFI.CRIT_PARA' )
      CALL JEDETR ('&&TBIMFI.VALE_I'    )
      CALL JEDETR ('&&TBIMFI.VALE_R'    )
      CALL JEDETR ('&&TBIMFI.VALE_C'    )
      CALL JEDETR ('&&TBIMFI.VALE_K'    )
      CALL JEDETR ('&&TBIMFI.PRECISION' )
      CALL JEDETR ('&&TBIMFI.CRITERE'   )
C
 1010 FORMAT('FILTRE -> NOM_PARA: ',A16,' CRIT_COMP: ',A4,' VALE: ',I12)
 1020 FORMAT('FILTRE -> NOM_PARA: ',A16,' CRIT_COMP: ',A4,
     +                                ' VALE: ',1PE12.5)
 1030 FORMAT('FILTRE -> NOM_PARA: ',A16,' CRIT_COMP: ',A4,
     +                                ' VALE: ',1PE12.5,1X,1PE12.5)
 1040 FORMAT('FILTRE -> NOM_PARA: ',A16,' CRIT_COMP: ',A4,' VALE: ',A8)
C
      CALL JEDEMA()
      END
