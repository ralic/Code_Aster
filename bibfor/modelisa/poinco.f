      SUBROUTINE POINCO (CHAR,MOTFAZ,NOMAZ,NZOCO,NSUCO,NMACO,NNOCO,
     +                   NNOQUA,PZONE,PSURMA,PSURNO,PNOQUA,NTRAV)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 22/07/2003   AUTEUR LAVERNE J.LAVERNE 
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
      IMPLICIT NONE
C
      INTEGER       NZOCO,NSUCO,NMACO,NNOCO,NTRAV,NNOQUA
      CHARACTER*8   CHAR
      CHARACTER*(*) MOTFAZ,NOMAZ
      CHARACTER*24  PZONE,PSURMA,PSURNO,PNOQUA
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CALICO
C ----------------------------------------------------------------------
C
C DETERMINATION DU NOMBRE DE ZONES DE CONTACT, ET DU NOMBRE TOTAL DE
C MAILLES ET DE NOEUDS DE CONTACT. REMPLISSAGE DES POINTEURS ASSOCIES.
C
C IN  MOTFAZ : MOT-CLE FACTEUR (VALANT 'CONTACT')
C IN  NOMAZ  : NOM DU MAILLAGE
C IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
C OUT NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
C OUT NMACO  : NOMBRE TOTAL DE MAILLES DES SURFACES
C OUT NNOCO  : NOMBRE TOTAL DE NOEUDS DES SURFACES
C OUT PZONE  : POINTEUR DES ZONES DE CONTACT
C OUT PSURMA : POINTEUR DES MAILLES DES SURFACES
C OUT PSURNO : POINTEUR DES NOEUDS DES SURFACES
C OUT NTRAV  : DIMENSION DU TABLEAU DE TRAVAIL
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32       JEXNUM , JEXNOM
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
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      NG0,NG1,NG2,NM0,NM1,NM2,NTOT,NBMA,NBNO
      INTEGER      NG,NGR,N1,N2,NBMAIL,NMAI,NUMAIL
      INTEGER      IOC,II1,II2,ISURF,INDICE
      INTEGER      JZONE,JSUMA,JSUNO,JGRO,JBID,IBID
      INTEGER      IATYMA,NUTYP,ITYP,JNOQUA,NBNOQU
      INTEGER      N1Q, N2Q, NOEUSO, NOEUMI, NBNOMI
      INTEGER      NOC, NOCN
      REAL*8       COEFPN
      CHARACTER*8  NOMTM, MOTCLE
      CHARACTER*1  K1BID
      CHARACTER*8  K8BID,NOMA,NOMAIL
      CHARACTER*16 MOTFAC,TYPF
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MOTFAC = MOTFAZ
      NOMA   = NOMAZ
C
      NSUCO = 0
      NMACO = 0
      NNOCO = 0
      NTRAV = 0
      INDICE= 0
C
C ======================================================================
C              DETERMINATION DU NOMBRE TOTAL DE SURFACES
C                  ET REMPLISSAGE DU POINTEUR PZONE
C ======================================================================
C
      CALL WKVECT (PZONE,'G V I',NZOCO+1,JZONE)
      ZI(JZONE) = 0
C
      DO 10 IOC = 1,NZOCO
C
         CALL GETVTX (MOTFAC,'METHODE',IOC,1,1,TYPF,NOC)
        IF(TYPF(1:8).EQ.'PENALISA') THEN
             CALL GETVR8 (MOTFAC,'E_N',1,1,1,COEFPN, NOCN)
             IF(NOCN.NE.0) INDICE=1
        ENDIF
        IF(TYPF(1:8).EQ.'CONTINUE') THEN
             INDICE=1
         ENDIF
C                
         CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_1',
     +            IOC,1,0,K8BID,NG1)
         CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_2',
     +            IOC,1,0,K8BID,NG2)
         CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_1',
     +          IOC,1,0,K8BID,NM1)
         CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_2',
     +          IOC,1,0,K8BID,NM2)
         IF (NG1.NE.0) NSUCO = NSUCO + 1
         IF (NG2.NE.0) NSUCO = NSUCO + 1
         IF (NM1.NE.0) NSUCO = NSUCO + 1
         IF (NM2.NE.0) NSUCO = NSUCO + 1
         NG0 = 0
         NM0 = 0
C     CALL GETVEM(XXX,'GROUP_MA',MOTFAC,'GROUP_MA',IOC,1,0,K8BID,NG0)
C     CALL GETVEM(XXX,'MAILLE',MOTFAC,'MAILLE',IOC,1,0,K8BID,NM0)
         NTOT = ABS(NG1) + ABS(NG2) + ABS(NM1) + ABS(NM2)
     +        + ABS(NG0) + ABS(NM0)
         NTRAV = MAX (NTRAV,NTOT)
         NSUCO = NSUCO + ABS(NG0) + ABS(NM0)
C
         ZI(JZONE+IOC) = NSUCO
C
 10   CONTINUE
C
C ======================================================================
C     LECTURE DES MAILLES ET DES NOEUDS POUR CALCULER NMACO ET NNOCO
C               ET REMPLIR LES POINTEURS PSURMA ET PSURNO
C ======================================================================
C
      CALL WKVECT (PSURMA,'G V I',NSUCO+1,JSUMA)
      CALL WKVECT (PSURNO,'G V I',NSUCO+1,JSUNO)
      CALL WKVECT (PNOQUA,'G V I',NSUCO+1,JNOQUA)
      CALL WKVECT ('&&POINCO.TRAV','V V K8',NTRAV,JBID)
C
      ZI(JSUMA)  = 0
      ZI(JSUNO)  = 0
      ZI(JNOQUA) = 0
C
      ISURF = 0
C
      DO 20 IOC = 1,NZOCO
C
C --- MOT-CLE GROUP_MA_1
C
         CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_1',
     +            IOC,1,0,K8BID,NG)
         IF (NG.NE.0) THEN
             MOTCLE = 'GROUP_MA'
             NG = -NG
             ISURF = ISURF + 1
             CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_1',
     +                IOC,1,NG,ZK8(JBID),NGR)
             CALL NBNOEL(CHAR,NOMA,MOTCLE,NGR,ZK8(JBID),INDICE,
     +                   NBMA,NBNO,NBNOQU)
             ZI(JSUMA  + ISURF) = ZI(JSUMA  + ISURF-1) + NBMA
             ZI(JSUNO  + ISURF) = ZI(JSUNO  + ISURF-1) + NBNO
             ZI(JNOQUA + ISURF) = ZI(JNOQUA + ISURF-1) + NBNOQU
         END IF
C
C --- MOT-CLE MAILLE_1
C
         CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_1',
     +          IOC,1,0,K8BID,NBMA)
         IF (NBMA.NE.0) THEN
             MOTCLE = 'MAILLE'
             NBMA   = -NBMA
             ISURF = ISURF + 1
             CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_1',
     +              IOC,1,NBMA,ZK8(JBID),NMAI)
             CALL NBNOEL(CHAR,NOMA,MOTCLE,0,ZK8(JBID),INDICE,
     +                   NBMA,NBNO,NBNOQU)
             ZI(JSUMA  + ISURF) = ZI(JSUMA  + ISURF-1) + NBMA
             ZI(JSUNO  + ISURF) = ZI(JSUNO  + ISURF-1) + NBNO
             ZI(JNOQUA + ISURF) = ZI(JNOQUA + ISURF-1) + NBNOQU
         ENDIF
C
C --- MOT-CLE GROUP_MA_2
C
         CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_2',
     +            IOC,1,0,K8BID,NG)
         IF (NG.NE.0) THEN
             MOTCLE = 'GROUP_MA'
             NG = -NG
             ISURF = ISURF + 1
             CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_2',
     +                IOC,1,NG,ZK8(JBID),NGR)
             CALL NBNOEL(CHAR,NOMA,MOTCLE,NGR,ZK8(JBID),
     +                   INDICE,NBMA,NBNO,NBNOQU)
             ZI(JSUMA  + ISURF) = ZI(JSUMA  + ISURF-1) + NBMA
             ZI(JSUNO  + ISURF) = ZI(JSUNO  + ISURF-1) + NBNO
             ZI(JNOQUA + ISURF) = ZI(JNOQUA + ISURF-1) + NBNOQU
         END IF
C
C --- MOT-CLE MAILLE_2
C
         CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_2',
     +          IOC,1,0,K8BID,NBMA)
         IF (NBMA.NE.0) THEN
             MOTCLE = 'MAILLE'
             NBMA   = -NBMA
             ISURF = ISURF + 1
             CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_2',
     +              IOC,1,NBMA,ZK8(JBID),NMAI)
             CALL NBNOEL(CHAR,NOMA,MOTCLE,0,ZK8(JBID),INDICE,
     +                   NBMA,NBNO,NBNOQU)
             ZI(JSUMA  + ISURF) = ZI(JSUMA  + ISURF-1) + NBMA
             ZI(JSUNO  + ISURF) = ZI(JSUNO  + ISURF-1) + NBNO
             ZI(JNOQUA + ISURF) = ZI(JNOQUA + ISURF-1) + NBNOQU
         ENDIF
C
C --- MOT-CLE GROUP_MA
C
C     CALL GETVEM(XXX,'GROUP_MA',MOTFAC,'GROUP_MA',IOC,1,0,K8BID,NG)
C         IF (NG.NE.0) THEN
C             NG = -NG
C             DO 27 II1 = 1, NGR
C                ISURF = ISURF + 1
C                NBNO = 0
C                CALL JEVEUO (JEXNOM(GRMAMA,ZK8(JBID+II1-1)),'L',JGRO)
C                CALL JELIRA (JEXNOM(GRMAMA,ZK8(JBID+II1-1)),'LONMAX',
C     &                       NBMAIL,K1BID)
C                ZI(JSUMA+ISURF) = ZI(JSUMA+ISURF-1) + NBMAIL
C                DO 28 II2 = 1, NBMAIL
C                  NUMAIL = ZI(JGRO-1+II2)
C                  CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
C                  CALL JELIRA (JEXNOM(NOMA//'.CONNEX',NOMAIL),'LONMAX',
C     &                         N1,K1BID)
C                  NBNO = NBNO + N1
C 28            CONTINUE
C               ZI(JSUNO+ISURF) = ZI(JSUNO+ISURF-1) + NBNO
C 27          CONTINUE
C         END IF
C
C --- MOT-CLE MAILLE
C
C     CALL GETVEM(XXX,'MAILLE',MOTFAC,'MAILLE',IOC,1,0,K8BID,NBMA)
C         IF (NBMA.NE.0) THEN
C             NBMA = -NBMA
C             DO 29 II1 = 1, NMAI
C                ISURF = ISURF + 1
C                CALL JELIRA (JEXNOM(NOMA//'.CONNEX',ZK8(JBID+II1-1)),
C     &                       'LONMAX',NBNO,K1BID)
C                ZI(JSUMA+ISURF) = ZI(JSUMA+ISURF-1) + NBMA
C                ZI(JSUNO+ISURF) = ZI(JSUNO+ISURF-1) + NBNO
C 29          CONTINUE
C         ENDIF
C
 20   CONTINUE
C
      NMACO  = ZI(JSUMA  + NSUCO)
      NNOCO  = ZI(JSUNO  + NSUCO)
      NNOQUA = ZI(JNOQUA + NSUCO)
C
C --- VERIFICATIONS ET ECRITURES
C
      IF (ISURF.NE.NSUCO) CALL UTMESS ('F','POINCO_01',
     +                                 'ERREUR SUR ISURF')
C
      CALL JEDETR ('&&POINCO.TRAV')
C
C ----------------------------------------------------------------------
C
      CALL JEDEMA()
      END
