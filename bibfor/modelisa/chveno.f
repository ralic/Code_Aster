      SUBROUTINE CHVENO ( FONREE, NOMA, NOMO )
      IMPLICIT   NONE
      CHARACTER*4         FONREE
      CHARACTER*(*)               NOMA, NOMO
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
C      OPERATEURS :     AFFE_CHAR_MECA ET AFFE_CHAR_MECA_C
C                                      ET AFFE_CHAR_MECA_F
C
C     VERIFICATION DES NORMALES AUX MAILLES SURFACIQUES EN 3D
C     ET LINEIQUES EN 2D
C     V1 : ON VERIFIE QUE LES NORMALES SONT HOMOGENES
C     V2 : ON VERIFIE QUE LES NORMALES SONT SORTANTES
C
C-----------------------------------------------------------------------
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       NBT, IER, N, NBMFAC, IMFAC, NOCC, IOCC, JGROUP,
     &              IPRES, IDNOR, IDTAN, IF1, IF2, IF3, IMF1, IMF2,
     &              NBOBJ, IOBJ, NBMC, IC, UTMOTP, NORIEN, NBPAR, IRET,
     &              JGRO, NORIE1, NORIE2, NBMAIL, IMA, NUMAIL,NUMA,IBID,
     &              IDTYMA, NUTYMA, INDIC, NDIM, NDIM1, IER1, NOC,
     &              NOC11,NOC12,NOC1, JCOOR, JTYMA, JNMA, NTRAIT
      INTEGER       JMAB, NBMAPR, NBMABO, JPRI, JBOR, IMPB
      PARAMETER    ( NBT = 5 )
      REAL*8        R8B, DNOR, R8PREM, DIR(3), ARMIN, PREC
      LOGICAL       GETEXM, REORIE, MCFL(NBT)
      COMPLEX*16    CBID
      CHARACTER*1   K1BID
      CHARACTER*8   K8B, MOT, NOGR, NOMAIL, NOMMA, TYPEL
      CHARACTER*16  MCFT(NBT), MOTFAC, VALMC(4), TYPMC(4), CONCEP, CMD
      CHARACTER*16  APPAR
      CHARACTER*19  NOMT19
      CHARACTER*24  GRMAMA, MAILMA, PARA
      CHARACTER*24 VALK(2)
C
      DATA MCFT / 'FACE_IMPO'  , 'PRES_REP' , 'FORCE_COQUE'  ,
     &            'EFFE_FOND'  , 'CONTACT'  /
C
C     LA NORMALE DOIT ETRE SORTANTE:
      DATA MCFL / .TRUE.       , .TRUE.     , .FALSE.        ,
     &            .TRUE.       , .TRUE.     /
C     ------------------------------------------------------------------
C
      IER = 0
      REORIE = .FALSE.
      CALL GETRES ( K8B, CONCEP, CMD )
C
C     NOMBRE DE MOTS-CLES FACTEUR A VERIFIER
      NBMFAC = NBT
C
      NOMMA = NOMA
      GRMAMA = NOMMA//'.GROUPEMA'
      MAILMA = NOMMA//'.NOMMAI'
C
C --- RECUPERATION DE L'ARETE MINIMUM DU MAILLAGE
C
      CALL JEEXIN ( NOMMA//'           .LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL LTNOTB ( NOMMA , 'CARA_GEOM' , NOMT19 )
         NBPAR = 0
         PARA = 'AR_MIN                  '
         CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, CBID, K8B,
     &                K8B, R8B , PARA, K8B, IBID, ARMIN, CBID,
     &                K8B, IRET )
         IF ( IRET .EQ. 0 ) THEN
            PREC = ARMIN*1.D-06
         ELSEIF ( IRET .EQ. 1 ) THEN
            PREC = 1.D-10
         ELSE
            CALL U2MESS('F','MODELISA2_13')
         ENDIF
      ELSE
         CALL U2MESS('F','MODELISA3_18')
      ENDIF
C
      CALL GETVTX ( ' ', 'VERI_NORM', 0,1,1, MOT, N )
      IF ( MOT .EQ. 'NON' ) NBMFAC = 0
C
      NDIM = 0
      CALL DISMOI('F','DIM_GEOM',NOMO,'MODELE',NDIM,K8B,IER1)
      IF ( NDIM .GT. 1000 )  NDIM = 3
C
      CALL JEEXIN ( NOMMA//'.TYPMAIL        ', IRET )
      IF ( IRET .NE. 0 )
     &             CALL JEVEUO ( NOMMA//'.TYPMAIL        ', 'L', JTYMA )
      CALL JEVEUO ( NOMMA//'.COORDO    .VALE', 'L', JCOOR )
C
      DO 100 IMFAC = 1 , NBMFAC
         MOTFAC = MCFT(IMFAC)
C
C        CAS OU UN MOT CLE N'EXISTE QUE POUR CERTAINS CATALOGUES
C        (PAR EXEMPLE EFFE_FOND)
         IF ( .NOT. GETEXM(MOTFAC,' ') ) GOTO 100
C
         CALL GETFAC ( MOTFAC, NOCC )
         DO 200  IOCC = 1 , NOCC
C           POUR CERTAINS MOTS-CLES, IL NE FAUT TESTER QUE
C           POUR CERTAINS CHARGEMENTS
            IF (MOTFAC.EQ.'FACE_IMPO') THEN
               IPRES = UTMOTP(FONREE,MOTFAC,IOCC,'PRES')
               IDNOR = UTMOTP(FONREE,MOTFAC,IOCC,'DNOR')
               IDTAN = UTMOTP(FONREE,MOTFAC,IOCC,'DTAN')
               IF (IPRES.EQ.0.AND.IDNOR.EQ.0.AND.IDTAN.EQ.0) GOTO 200
               IF (IDNOR.NE.0) THEN
                  IF (FONREE.EQ.'REEL') THEN
                     CALL GETVR8(MOTFAC,'DNOR',IOCC,1,1,DNOR,N)
                     IF ( ABS(DNOR) .LE. R8PREM() ) GOTO 200
                  ENDIF
               ENDIF
            ELSEIF (MOTFAC.EQ.'FORCE_COQUE') THEN
               IPRES = UTMOTP(FONREE,MOTFAC,IOCC,'PRES')
               IF1   = UTMOTP(FONREE,MOTFAC,IOCC,'F1  ')
               IF2   = UTMOTP(FONREE,MOTFAC,IOCC,'F2  ')
               IF3   = UTMOTP(FONREE,MOTFAC,IOCC,'F3  ')
               IMF1  = UTMOTP(FONREE,MOTFAC,IOCC,'MF1 ')
               IMF2  = UTMOTP(FONREE,MOTFAC,IOCC,'MF2 ')
               IF (IPRES.EQ.0.AND.IF1.EQ.0.AND.IF2.EQ.0.AND.IF3.EQ.0
     &                       .AND.IMF1.EQ.0.AND.IMF2.EQ.0) GOTO 200
            ENDIF
C
            IF ( MOTFAC .EQ. 'CONTACT' ) THEN
               NBMC = 4
               VALMC(1) = 'GROUP_MA_ESCL'
               VALMC(2) = 'GROUP_MA_MAIT'
               VALMC(3) = 'MAILLE_ESCL'
               VALMC(4) = 'MAILLE_MAIT'
               TYPMC(1) = 'GROUP_MA'
               TYPMC(2) = 'GROUP_MA'
               TYPMC(3) = 'MAILLE'
               TYPMC(4) = 'MAILLE'
            ELSE
               NBMC = 2
               VALMC(1) = 'GROUP_MA'
               VALMC(2) = 'MAILLE'
               TYPMC(1) = 'GROUP_MA'
               TYPMC(2) = 'MAILLE'
            ENDIF
C
C ---       RECUPERATION DE LA DIMENSION DU PROBLEME
C
            NOC  = 0
            IF (MOTFAC .EQ. 'CONTACT') THEN
               CALL GETVR8 (MOTFAC,'VECT_NORM_ESCL',IOCC,1,3,DIR,NOC)
               CALL GETVR8 (MOTFAC,'VECT_Y',       IOCC,1,3, DIR, NOC11)
               CALL GETVR8 (MOTFAC,'VECT_ORIE_POU',IOCC,1,3, DIR, NOC12)
               NOC1=NOC11+NOC12
            ENDIF
C
            INDIC = 0
C
            DO 210  IC = 1 , NBMC
               CALL GETVID ( MOTFAC, VALMC(IC), IOCC,1,0, K8B, NBOBJ )
               IF ( NBOBJ .EQ. 0 ) GOTO 210
C
               NBOBJ = -NBOBJ
               CALL WKVECT ( '&&CHVENO.OBJET', 'V V K8', NBOBJ,JGROUP)
               CALL GETVEM ( NOMA, TYPMC(IC), MOTFAC, VALMC(IC),
     &                          IOCC,1,NBOBJ, ZK8(JGROUP), NBOBJ )
               IF ( TYPMC(IC) .EQ. 'GROUP_MA' ) THEN
                 DO 212  IOBJ = 1 , NBOBJ
                  NOGR = ZK8(JGROUP-1+IOBJ)
                  IF (MOTFAC .EQ. 'CONTACT') THEN
C
C ---              RECUPERATION DU NOMBRE DE MAILLES DU GROUP_MA :
C                  ---------------------------------------------
                   CALL JELIRA (JEXNOM(GRMAMA,NOGR),'LONMAX',NBMAIL,
     &                                                       K1BID)
                   CALL JEVEUO (JEXNOM(GRMAMA,NOGR),'L',JGRO)
C
                   DO 213 IMA=1,NBMAIL
                     NUMAIL = ZI(JGRO-1+IMA)
                     CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
C
C ---                NUMERO DE LA MAILLE
C                    ------------------
                     CALL JENONU(JEXNOM(NOMMA//'.NOMMAI',NOMAIL),NUMA)
                     CALL JEVEUO(NOMMA//'.TYPMAIL','L',IDTYMA)
                     NUTYMA = ZI(IDTYMA+NUMA-1)
C
C ---                TYPE DE LA MAILLE :
C                    -----------------
                     CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPEL)
C
C ---                CAS D'UNE MAILLE POINT
C                    ----------------------
                     IF (TYPEL(1:3) .EQ. 'POI') THEN
                       IF (INDIC .EQ. 0) THEN
                         INDIC = 1
                         GOTO 211
                       ELSE
C ---                    CAS D'UN CONTACT POINT-POINT
C                        ----------------------------
C                        ON VERIFIE DANS CALICO QUE CETTE OPTION N'EST
C                        UTILISEE QUE DANS LE CAS DE L'APPARIEMENT NODAL
                         IF(NOC.EQ.0) THEN
                           CALL U2MESS('F','MODELISA4_20')
                         ELSE
                           GOTO 100
                         ENDIF
                       ENDIF
C
C ---               CAS D'UNE MAILLE TRIA OU QUAD
C                   -----------------------------
                    ELSEIF(TYPEL(1:4) .EQ. 'TRIA'
     &                .OR. TYPEL(1:4) .EQ. 'QUAD') THEN
                      NDIM1 = 3
                      IF(NDIM .NE. NDIM1) THEN
                          VALK(1) = NOMAIL
                          VALK(2) = TYPEL
                          CALL U2MESK('F','MODELISA4_21', 2 ,VALK)
                      ENDIF
C
C ---               CAS D'UNE MAILLE SEG
C                   --------------------
                    ELSE IF(TYPEL(1:3) .EQ. 'SEG') THEN
                      NDIM1 = 2
                      IF(NDIM .NE. NDIM1 .AND. NOC1.EQ.0) THEN
                          VALK(1) = NOMAIL
                          VALK(2) = TYPEL
                          CALL U2MESK('F','MODELISA4_22', 2 ,VALK)
                      ENDIF
C
                    ENDIF
 213               CONTINUE
C
C ---              FIN DE BOUCLE SUR LES MAILLES DU GROUP_MA
C
                  ENDIF
                  NORIE1 = 0
                  NORIE2 = 0
                  CALL JELIRA (JEXNOM(GRMAMA,NOGR),'LONMAX',NBMAIL,
     &                                                       K1BID)
                  CALL JEVEUO (JEXNOM(GRMAMA,NOGR),'L',JGRO)
C
                  IF ( MCFL(IC) ) THEN
C
                 CALL WKVECT('&&CHVENO.MAILLE_BORD','V V I',NBMAIL,JMAB)
                    CALL CHBORD ( NOMO, NBMAIL, ZI(JGRO), ZI(JMAB),
     &                            NBMAPR, NBMABO )
                    IF ( NBMAPR.EQ.NBMAIL .AND. NBMABO.EQ.0 ) THEN
                      CALL ORNORM ( NOMMA, ZI(JGRO), NBMAIL, REORIE,
     &                                                          NORIE1 )
                    ELSEIF ( NBMAPR.EQ.0 .AND. NBMABO.EQ.NBMAIL ) THEN
                      CALL ORILMA ( NOMMA, NDIM, ZI(JGRO), NBMAIL,
     &                                    NORIE1, NTRAIT, REORIE, PREC )
                    ELSEIF ( NBMAPR.EQ.0 .AND. NBMABO.EQ.0 ) THEN
                      CALL ORNORM ( NOMMA, ZI(JGRO), NBMAIL, REORIE,
     &                                                          NORIE1 )
                    ELSE
                      CALL WKVECT('&&CHVENO.PRIN','V V I',NBMAPR,JPRI)
                      CALL WKVECT('&&CHVENO.BORD','V V I',NBMABO,JBOR)
                      NBMAPR = 0
                      NBMABO = 0
                      DO 218 IMPB = 1 , NBMAIL
                         IF ( ZI(JMAB+IMPB-1) .EQ. 0 ) THEN
                            NBMAPR = NBMAPR + 1
                            ZI(JPRI+NBMAPR-1) = ZI(JGRO+IMPB-1)
                         ELSE
                            NBMABO = NBMABO + 1
                            ZI(JBOR+NBMABO-1) = ZI(JGRO+IMPB-1)
                         ENDIF
 218                  CONTINUE
                      CALL ORNORM ( NOMMA, ZI(JPRI), NBMAPR, REORIE,
     &                                                          NORIE1 )
                      CALL ORILMA ( NOMMA, NDIM, ZI(JBOR), NBMABO,
     &                                    NORIE1, NTRAIT, REORIE, PREC )
                      CALL JEDETR('&&CHVENO.PRIN')
                      CALL JEDETR('&&CHVENO.BORD')
                    ENDIF
                    CALL JEDETR('&&CHVENO.MAILLE_BORD')
                  ELSE
                    CALL ORNORM ( NOMMA, ZI(JGRO), NBMAIL, REORIE,
     &                                                          NORIE2 )
                  ENDIF
                  NORIEN = NORIE1 + NORIE2
                  IF ( NORIEN .NE. 0 ) THEN
                    IER = IER + 1
                    CALL UTDEBM('E',CMD,'GROUP_MA')
                    CALL UTIMPK('S',' ', 1, NOGR)
                    CALL UTIMPI('S','MAILLES MAL ORIENTEES ',0,NORIEN)
                    CALL UTFINM()
                  ENDIF
 212             CONTINUE
C
C ------------ CAS DES MAILLES :
C              ---------------
               ELSE
                  CALL WKVECT('&&CHVENO.NUME_MAILLE','V V I',NBOBJ,JNMA)
                  DO 216  IOBJ = 1 , NBOBJ
                     NOMAIL = ZK8(JGROUP-1+IOBJ)
                     CALL JENONU(JEXNOM(NOMMA//'.NOMMAI',NOMAIL),NUMA)
                     ZI(JNMA+IOBJ-1) = NUMA
                     IF (MOTFAC .EQ. 'CONTACT') THEN
                       CALL JEVEUO(NOMMA//'.TYPMAIL','L',IDTYMA)
                       NUTYMA = ZI(IDTYMA+NUMA-1)
                       CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),
     &                                                            TYPEL)
C
C ---                  CAS D'UNE MAILLE POINT
C                      ----------------------
                       IF (TYPEL(1:3) .EQ. 'POI') THEN
                         IF (INDIC .EQ. 0) THEN
                           INDIC = 1
                           GOTO 211
                         ELSE
C
C ---                      CAS D'UN CONTACT POINT-POINT
C                          ----------------------------
C                          ON VERIFIE DANS CALICO QUE CETTE OPTION
C                          N'EST UTILISEE QUE DANS LE CAS DE
C                          L'APPARIEMENT NODAL
                           IF(NOC.EQ.0) THEN
                             CALL U2MESS('F','MODELISA4_20')
                           ELSE
                             GOTO 100
                           ENDIF
                         ENDIF
C
C ---                  CAS D'UNE MAILLE TRIA OU QUAD
C                      -----------------------------
                       ELSEIF(TYPEL(1:4) .EQ. 'TRIA'
     &                   .OR. TYPEL(1:4) .EQ. 'QUAD') THEN
                         NDIM1 = 3
                         IF(NDIM .NE. NDIM1) THEN
                            VALK(1) = NOMAIL
                            VALK(2) = TYPEL
                            CALL U2MESK('F','MODELISA4_21', 2 ,VALK)
                         ENDIF
C
C ---                  CAS D'UNE MAILLE SEG
C                      --------------------
                       ELSE IF(TYPEL(1:3) .EQ. 'SEG') THEN
                         NDIM1 = 3
                         IF(NDIM .NE. NDIM1) THEN
                            VALK(1) = NOMAIL
                            VALK(2) = TYPEL
                            CALL U2MESK('F','MODELISA4_23', 2 ,VALK)
                         ENDIF
                       ENDIF
C
                     ENDIF
 216              CONTINUE
                  NORIE1 = 0
                  NORIE2 = 0
                  IF ( MCFL(IC) ) THEN
                  CALL WKVECT('&&CHVENO.MAILLE_BORD','V V I',NBOBJ,JMAB)
                    CALL CHBORD ( NOMO, NBOBJ, ZI(JNMA), ZI(JMAB),
     &                            NBMAPR, NBMABO )
                    IF ( NBMAPR.EQ.NBOBJ .AND. NBMABO.EQ.0 ) THEN
                      CALL ORNORM ( NOMMA, ZI(JNMA), NBOBJ, REORIE,
     &                                                          NORIE1 )
                    ELSEIF ( NBMAPR.EQ.0 .AND. NBMABO.EQ.NBOBJ ) THEN
                      CALL ORILMA ( NOMMA, NDIM, ZI(JNMA), NBOBJ,
     &                                    NORIE1, NTRAIT, REORIE, PREC )
                    ELSEIF ( NBMAPR.EQ.0 .AND. NBMABO.EQ.0 ) THEN
                      CALL ORNORM ( NOMMA, ZI(JNMA), NBOBJ, REORIE,
     &                                                          NORIE1 )
                    ELSE
                      CALL WKVECT('&&CHVENO.PRIN','V V I',NBMAPR,JPRI)
                      CALL WKVECT('&&CHVENO.BORD','V V I',NBMABO,JBOR)
                      NBMAPR = 0
                      NBMABO = 0
                      DO 220 IMPB = 1 , NBOBJ
                         IF ( ZI(JMAB+IMPB-1) .EQ. 0 ) THEN
                            NBMAPR = NBMAPR + 1
                            ZI(JPRI+NBMAPR-1) = ZI(JNMA+IMPB-1)
                         ELSE
                            NBMABO = NBMABO + 1
                            ZI(JBOR+NBMABO-1) = ZI(JNMA+IMPB-1)
                         ENDIF
 220                  CONTINUE
                      CALL ORNORM ( NOMMA, ZI(JPRI), NBMAPR, REORIE,
     &                                                          NORIE1 )
                      CALL ORILMA ( NOMMA, NDIM, ZI(JBOR), NBMABO,
     &                                    NORIE1, NTRAIT, REORIE, PREC )
                      CALL JEDETR('&&CHVENO.PRIN')
                      CALL JEDETR('&&CHVENO.BORD')
                    ENDIF
                    CALL JEDETR('&&CHVENO.MAILLE_BORD')
                  ELSE
                     CALL ORNORM ( NOMMA, ZI(JNMA), NBOBJ, REORIE,
     &                                                          NORIE2 )
                  ENDIF
                  NORIEN = NORIE1 + NORIE2
                  IF ( NORIEN .NE. 0 ) THEN
                     IER = IER + 1
                     CALL UTDEBM('E',CMD,'MAILLE MAL ORIENTEE')
                     CALL UTIMPK('S',': ', 1, NOMAIL)
                     CALL UTFINM()
                  ENDIF
                  CALL JEDETR('&&CHVENO.NUME_MAILLE')
               ENDIF
 211         CONTINUE
             CALL JEDETR ('&&CHVENO.OBJET')
 210        CONTINUE
 200     CONTINUE
 100  CONTINUE
C
      IF (IER .NE. 0)  CALL U2MESS('F','MODELISA4_24')
C
      END
