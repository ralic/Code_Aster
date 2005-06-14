      SUBROUTINE OP0161 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
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
C     COMBINAISON FOURIER
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      COMPLEX*16    CBID
      CHARACTER*8   K8B, RESU, RESUIN
      CHARACTER*16  CONCEP, NOMCMD, NSYMB
      CHARACTER*24  NOMCH
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES(RESU,CONCEP,NOMCMD)
C
      CALL GETVID(' ','RESULTAT',1,1,1,RESUIN,N1)
      CALL RSORAC (RESUIN,'LONUTI',IBID,RBID,K8B,CBID,RBID,K8B,
     +                                       NBORDR,1,NBTROU)
      CALL WKVECT('&&OP0161.NUME_ORDRE','V V I',NBORDR,JORDR)
      CALL RSORAC (RESUIN,'TOUT_ORDRE',IBID,RBID,K8B,CBID,RBID,K8B,
     +                                        ZI(JORDR),NBORDR,IBID)

C
      CALL GETVTX(' ','NOM_CHAM',1,1,0,K8B,N2)
      NBCHAM = -N2
      CALL WKVECT('&&OP0161.CHAMP','V V K16',NBCHAM,JCHAM)
      CALL GETVTX(' ','NOM_CHAM',1,1,NBCHAM,ZK16(JCHAM),N2)
C
      CALL GETVR8(' ','ANGL',1,1,0,ANGLE,N3)
      NBANGL = -N3
      CALL WKVECT('&&OP0161.ANGLE','V V R',NBANGL,JANGL)
      CALL GETVR8(' ','ANGL',1,1,NBANGL,ZR(JANGL),N3)
C
      CALL RSCRSD ( RESU, CONCEP, NBANGL )
C
      CALL WKVECT('&&OP0161.NOM_CHAMP','V V K24',NBORDR,JNCH)
      CALL WKVECT('&&OP0161.TYP_CHAMP','V V K8' ,NBORDR,JTCH)
      CALL WKVECT('&&OP0161.NUM_HARMO','V V I'  ,NBORDR,JNHA)
      CALL WKVECT('&&OP0161.COEFFICIE','V V R'  ,NBORDR,JCOE)
      DO 100 ICH = 1 , NBCHAM
        NSYMB = ZK16(JCHAM+ICH-1)
        K = 0
        DO 120 IOR = 0 , NBORDR-1
          IORDR = ZI(JORDR+IOR)
          CALL RSEXCH ( RESUIN, NSYMB, IORDR, NOMCH, IRET )
          IF ( IRET .EQ. 0 ) THEN
            K = K + 1
            ZK24(JNCH+K-1) = NOMCH
            CALL RSADPA ( RESUIN,'L',1,'TYPE_MODE',IORDR,0,JTMO,K8B)
            ZK8(JTCH+K-1) = ZK8(JTMO)
            CALL RSADPA ( RESUIN,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
            ZI(JNHA+K-1) = ZI(JNMO)
            ZR(JCOE+K-1) = 1.D0
          ENDIF
 120    CONTINUE
        IF ( K .NE. 0 ) THEN
          DO 110 IAN = 1 , NBANGL
            CALL RSEXCH ( RESU, NSYMB, IAN, NOMCH, IRET )
            IF ( IRET .EQ. 110 ) THEN
              CALL RSAGSD ( RESU , 0 )
              CALL RSEXCH ( RESU, NSYMB, IAN, NOMCH, IRET )
            ELSEIF ( IRET .EQ. 100 ) THEN
            ELSE
              CALL UTMESS('F','OP0161','VRAIMENT DESOLE !')
            ENDIF
            ANGLE = ZR(JANGL+IAN-1) * R8DGRD()
            CALL REFODE ( K, ANGLE, ZK24(JNCH), ZI(JNHA), ZK8(JTCH),
     +                                    ZR(JCOE), 'G', NOMCH )
            CALL RSNOCH ( RESU, NSYMB, IAN, ' ' )
            CALL RSADPA ( RESU,'E',1,'ANGL',IAN,0,JJAN,K8B)
            ZR(JJAN) = ZR(JANGL+IAN-1)
 110      CONTINUE
        ENDIF
 100  CONTINUE
C
      CALL JEDEMA()
      END
