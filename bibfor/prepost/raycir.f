      SUBROUTINE RAYCIR(JVECPG, JDTAU, JVECN, NBORDR, NBVEC, NOMMET)
C MODIF PREPOST  DATE 08/04/2003   AUTEUR F1BHHAJ J.ANGLES 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C RESPONSABLE F1BHHAJ
C TOLE  CRP_20
      IMPLICIT      NONE
      INTEGER       JVECPG, JDTAU, JVECN, NBORDR, NBVEC
      CHARACTER*16  NOMMET
C ---------------------------------------------------------------------
C BUT: DETERMINER LE PLUS PETIT CERCLE CIRCONSCRIT AUX POINTS
C      REPRESANTANT LE VECTEUR DE CISAILLEMENT TAU DANS LE PLAN u, v.
C ---------------------------------------------------------------------
C ARGUMENTS:
C JVECPG     IN    I : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT 
C                      LES COMPOSANTES u ET v DU VECTEUR TAU 
C                      (CISAILLEMENT), POUR TOUS LES NUMEROS
C                      D'ORDRE.
C JDTAU      IN    I : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT 
C                      LES VALEURS DE DELTA_TAU_MAX POUR CHAQUE VECTEUR.
C JVECN      IN    I : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT 
C                      LA VALEUR DU POINTEUR PERMETTANT D'ACCEDER AU
C                      VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX.
C NBORDR     IN    I : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
C                      STRUCTURE DE DONNEES RESULTAT.
C NBVEC      IN    I : NOMBRE DE VECTEURS NORMAUX.
C
C-----------------------------------------------------------------------
C---- COMMUNS NORMALISES  JEVEUX
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNOM,JEXNUM,JEXATR
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER      I, J, IVECT, IORDR, K, N1
      INTEGER      JSEC1, JSEC2, JSEC3, JSEC4, JDOM1
      INTEGER      JDOM2, JCOORP, JCER3P
      INTEGER      NBPTS1, NBPTS2, NBPTS3, NBPTS4, NBPTD1, NBPTD2
      INTEGER      INDSEC, N, IRETH, IRETV, NBOUCL, NBR, IRET3P
C
      REAL*8       R8MAEM, CUMIN, CUMAX, CVMIN, CVMAX, CUI, CVI, DIAMIN
      REAL*8       RAYMIN, CUOMI1, CVOMI1, CUOMI2, CVOMI2, CUO1, CVO1
      REAL*8       DISTO1, DISTS(4), DISTD1, DISTD2, DMAXI(6)
      REAL*8       COORPT(24), DMAX, CUPPE1, CVPPE1, CUPPE2, CVPPE2
      REAL*8       DUN, DVN, CUON, CVON, DSEGN, RSEGN, CUPN, CVPN
      REAL*8       CUPN0, CVPN0, CUPN1, CVPN1, CUPN2, CVPN2, RAY3PT
      REAL*8       ETIR, RAYON, DIST, CUTAU, CVTAU, P, EPSILO, X
      REAL*8       CETIR, CUOI, CVOI, RMIN3P
C
      CHARACTER*5  ORICAD
C
C-----------------------------------------------------------------------
C234567                                                              012
C
      CALL JEMARQ()

      IF ( NOMMET(1:15) .EQ. 'CERCLE_APPROCHE') THEN
         GOTO 777
      ENDIF
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                 --------------------------
C                 |    PREMIERE METHODE    |
C                 --------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      CETIR = SQRT(3.0D0/4.0D0)
      EPSILO = 1.0D-7
C
      CALL WKVECT('&&RAYCIR.SECT1', 'V V R', NBORDR*2, JSEC1)
      CALL WKVECT('&&RAYCIR.SECT2', 'V V R', NBORDR*2, JSEC2)
      CALL WKVECT('&&RAYCIR.SECT3', 'V V R', NBORDR*2, JSEC3)
      CALL WKVECT('&&RAYCIR.SECT4', 'V V R', NBORDR*2, JSEC4)
      CALL WKVECT('&&RAYCIR.DOM1', 'V V R', NBORDR*2, JDOM1)
      CALL WKVECT('&&RAYCIR.DOM2', 'V V R', NBORDR*2, JDOM2)
C
      CALL WKVECT('&&RAYCIR.COORP', 'V V R', 12, JCOORP)
      CALL WKVECT('&&RAYCIR.CER3PT', 'V V R', 6, JCER3P)
      
C
C ININTIALISATION
C
      N1 = 0
C
      DO 30 IVECT=1, NBVEC
         CUMIN = R8MAEM()
         CUMAX = -R8MAEM()
         CVMIN = R8MAEM()
         CVMAX = -R8MAEM()
         CALL JERAZO('&&RAYCIR.SECT1', NBORDR*2, 1)
         CALL JERAZO('&&RAYCIR.SECT2', NBORDR*2, 1)
         CALL JERAZO('&&RAYCIR.SECT3', NBORDR*2, 1)
         CALL JERAZO('&&RAYCIR.SECT4', NBORDR*2, 1)
         CALL JERAZO('&&RAYCIR.DOM1', NBORDR*2, 1)
         CALL JERAZO('&&RAYCIR.DOM2', NBORDR*2, 1)
C
         DO 40 IORDR=1, NBORDR
            N1 = N1 + 1
            CUI = ZR( JVECPG + (N1-1)*2 )
            CVI = ZR( JVECPG + (N1-1)*2 + 1 )
C
            IF (CUI .LT. CUMIN) THEN
               CUMIN = CUI
            ENDIF
            IF (CUI .GT. CUMAX) THEN
               CUMAX = CUI
            ENDIF
            IF(CVI .LT. CVMIN) THEN
               CVMIN = CVI
            ENDIF
            IF(CVI .GT. CVMAX) THEN
               CVMAX = CVI
            ENDIF
 40      CONTINUE
C
C-----------------------------------------------------------------------
C   ------------------------------------
C  |  TRAITEMENT DES CAS PARTICULIERS  |
C  ------------------------------------
C
C 1/ CAS OU TOUS LES POINTS SONT DANS UNE BOITE DONT LES DEUX COTES
C    SONT INFERIEURS A EPSILO.
C
         IF ( (ABS(CVMAX-CVMIN) .LT. EPSILO) .AND.
     &        (ABS(CUMAX-CUMIN) .LT. EPSILO) ) THEN
            ZR(JDTAU + (IVECT-1)) = 0.0D0
            ZI(JVECN + (IVECT-1)) = IVECT
            GOTO 30
C
C 2/ CAS OU TOUS LES POINTS SONT ALIGNES HORIZONTALEMENT
C
         ELSEIF ( ABS(CVMAX-CVMIN) .LT. EPSILO ) THEN
            ZR(JDTAU + (IVECT-1)) = ABS(CUMAX-CUMIN)/2.0D0
            ZI(JVECN + (IVECT-1)) = IVECT
            GOTO 30
C
C 3/ CAS OU TOUS LES POINTS SONT ALIGNES VERTICALEMENT
C
         ELSEIF ( ABS(CUMAX-CUMIN) .LT. EPSILO ) THEN
            ZR(JDTAU + (IVECT-1)) = ABS(CVMAX-CVMIN)/2.0D0
            ZI(JVECN + (IVECT-1)) = IVECT
            GOTO 30
         ENDIF
C
C-----------------------------------------------------------------------
C
         IF ((CUMAX - CUMIN) .GE. (CVMAX - CVMIN)) THEN
            DIAMIN = CUMAX - CUMIN
            RAYMIN = (CUMAX - CUMIN)/2.0D0
            CUOMI1 = CUMIN + (CUMAX - CUMIN)/2.0D0
            CVOMI1 = CVMIN
            CUOMI2 = CUMIN + (CUMAX - CUMIN)/2.0D0
            CVOMI2 = CVMAX
            ETIR   = (CVMAX - CVMIN)/(CUMAX - CUMIN)
            ORICAD = 'HORIZ'
         ELSE
            DIAMIN = CVMAX - CVMIN
            RAYMIN = (CVMAX - CVMIN)/2.0D0
            CUOMI1 = CUMIN
            CVOMI1 = CVMIN + (CVMAX - CVMIN)/2.0D0
            CUOMI2 = CUMAX
            CVOMI2 = CVMIN + (CVMAX - CVMIN)/2.0D0
            ETIR   = (CUMAX - CUMIN)/(CVMAX - CVMIN)
            ORICAD = 'VERTI'
         ENDIF
C
         CUO1 = CUMIN + (CUMAX - CUMIN)/2.0D0
         CVO1 = CVMIN + (CVMAX - CVMIN)/2.0D0
C
C DETERMINATION DES POINTS SITUES DANS LES 4 SECTEURS ET LES 2 DOMAINES
C
         NBPTS1 = 0
         NBPTS2 = 0
         NBPTS3 = 0
         NBPTS4 = 0
         NBPTD1 = 0
         NBPTD2 = 0
         N1 = N1 - NBORDR
         DO 50 IORDR=1, NBORDR
            N1 = N1 + 1
            CUI = ZR( JVECPG + (N1-1)*2 )
            CVI = ZR( JVECPG + (N1-1)*2 + 1 )
C
            DISTO1   = SQRT((CUO1 - CUI)**2   + (CVO1 - CVI)**2)
            DISTS(1) = SQRT((CUMIN - CUI)**2  + (CVMAX - CVI)**2)
            DISTS(2) = SQRT((CUMAX - CUI)**2  + (CVMAX - CVI)**2)
            DISTS(3) = SQRT((CUMAX - CUI)**2  + (CVMIN - CVI)**2)
            DISTS(4) = SQRT((CUMIN - CUI)**2  + (CVMIN - CVI)**2)
            DISTD1   = SQRT((CUOMI1 - CUI)**2 + (CVOMI1 - CVI)**2)
            DISTD2   = SQRT((CUOMI2 - CUI)**2 + (CVOMI2 - CVI)**2)
C
            INDSEC = 0
            DO 60 I=1, 4
               IF ((DISTS(I) .GT. DIAMIN) .AND. (INDSEC .EQ. 0)) THEN
                  IF (CUI .GE. CUO1) THEN
                     IF (CVI .GE. CVO1) THEN
                        ZR(JSEC2 + NBPTS2*2) = CUI
                        ZR(JSEC2 + NBPTS2*2 +1) = CVI
                        NBPTS2 = NBPTS2 + 1
                        INDSEC = 1
                     ELSE
                        ZR(JSEC3 + NBPTS3*2) = CUI
                        ZR(JSEC3 + NBPTS3*2 +1) = CVI
                        NBPTS3 = NBPTS3 + 1
                        INDSEC = 1
                     ENDIF
                  ELSE
                     IF (CVI .GE. CVO1) THEN
                        ZR(JSEC1 + NBPTS1*2) = CUI
                        ZR(JSEC1 + NBPTS1*2 +1) = CVI
                        NBPTS1 = NBPTS1 + 1
                        INDSEC = 1
                     ELSE
                        ZR(JSEC4 + NBPTS4*2) = CUI
                        ZR(JSEC4 + NBPTS4*2 +1) = CVI
                        NBPTS4 = NBPTS4 + 1
                        INDSEC = 1
                     ENDIF
                  ENDIF
               ENDIF
 60         CONTINUE
C
            IF ((DISTD1 .GT. RAYMIN) .OR. (DISTO1 .GT. RAYMIN)) THEN
               ZR(JDOM1 + NBPTD1*2) = CUI
               ZR(JDOM1 + NBPTD1*2 + 1) = CVI
               NBPTD1 = NBPTD1 + 1
            ENDIF
C
            IF ((DISTD2 .GT. RAYMIN) .OR. (DISTO1 .GT. RAYMIN)) THEN
               ZR(JDOM2 + NBPTD2*2) = CUI
               ZR(JDOM2 + NBPTD2*2 + 1) = CVI
               NBPTD2 = NBPTD2 + 1
            ENDIF
 50      CONTINUE
C
C RECHERCHE DES 2 POINTS LES PLUS ELOIGNES PARMI LES POINTS DES
C SECTEURS 1, 2, 3 ET 4.
C
C EXEMPLE : CAS OU LE CADRE EST ORIENTE HORIZONTALEMENT
C
C       -----------------------------------
C       | Sect. 1                 Sect. 2 |
C       |                                 |
C       |                                 |
C       |                                 |
C       |                                 |
C       | Sect. 4                 Sect. 3 |
C       -----------------------------------
C
         DO 70 I=1, 6
            DMAXI(I) = 0.0D0
 70      CONTINUE
         DO 80 I=1, 24
            COORPT(I) = 0.0D0
 80      CONTINUE
         IF (( NBPTS1 .GT. 0 ) .AND. ( NBPTS2 .GT. 0 )) THEN
            IF ((ORICAD .EQ. 'HORIZ') .OR.
     &          ((ORICAD .EQ. 'VERTI') .AND. (ETIR .GE. CETIR))) THEN
               CALL DIMAX1 (JSEC1, JSEC2, NBPTS1, NBPTS2, DMAXI(1),
     &                      COORPT(1), COORPT(2), COORPT(3), COORPT(4))
            ELSE
               DMAXI(1) = 0.0D0
            ENDIF
         ENDIF
C
         IF (( NBPTS1 .GT. 0 ) .AND. ( NBPTS3 .GT. 0 )) THEN
            CALL DIMAX1 (JSEC1, JSEC3, NBPTS1, NBPTS3, DMAXI(2),
     &                   COORPT(5), COORPT(6), COORPT(7), COORPT(8))
         ELSE
            DMAXI(2) = 0.0D0
         ENDIF
C
         IF (( NBPTS1 .GT. 0 ) .AND. ( NBPTS4 .GT. 0 )) THEN
            IF ((ORICAD .EQ. 'VERTI') .OR.
     &          ((ORICAD .EQ. 'HORIZ') .AND. (ETIR .GE. CETIR))) THEN
               CALL DIMAX1 (JSEC1, JSEC4, NBPTS1, NBPTS4, DMAXI(3),
     &                      COORPT(9), COORPT(10), COORPT(11),
     &                      COORPT(12))
            ELSE
              DMAXI(3) = 0.0D0
            ENDIF        
         ENDIF
C
         IF (( NBPTS2 .GT. 0 ) .AND. ( NBPTS3 .GT. 0 )) THEN
            IF ((ORICAD .EQ. 'VERTI') .OR.
     &          ((ORICAD .EQ. 'HORIZ') .AND. (ETIR .GE. CETIR))) THEN
               CALL DIMAX1 (JSEC2, JSEC3, NBPTS2, NBPTS3, DMAXI(4),
     &                      COORPT(13), COORPT(14), COORPT(15),
     &                      COORPT(16))
            ELSE
              DMAXI(4) = 0.0D0
            ENDIF
         ENDIF
C
         IF (( NBPTS2 .GT. 0 ) .AND. ( NBPTS4 .GT. 0 )) THEN
            CALL DIMAX1 (JSEC2, JSEC4, NBPTS2, NBPTS4, DMAXI(5),
     &                   COORPT(17), COORPT(18), COORPT(19),
     &                   COORPT(20))
         ELSE
            DMAXI(5) = 0.0D0
         ENDIF
C
         IF (( NBPTS3 .GT. 0 ) .AND. ( NBPTS4 .GT. 0 )) THEN
            IF ((ORICAD .EQ. 'HORIZ') .OR.
     &          ((ORICAD .EQ. 'VERTI') .AND. (ETIR .GE. CETIR))) THEN
               CALL DIMAX1 (JSEC3, JSEC4, NBPTS3, NBPTS4, DMAXI(6),
     &                      COORPT(21), COORPT(22), COORPT(23),
     &                      COORPT(24))
            ELSE
               DMAXI(6) = 0.0D0
            ENDIF
         ENDIF
C
         DMAX = 0.0D0
         N = 0
         DO 90 I=1, 6
            IF (DMAXI(I) .GT. DMAX) THEN
               DMAX = DMAXI(I)
               N = I
            ENDIF
 90      CONTINUE
         CUPPE1 = COORPT((N-1)*4 + 1)
         CVPPE1 = COORPT((N-1)*4 + 2)
         CUPPE2 = COORPT((N-1)*4 + 3)
         CVPPE2 = COORPT((N-1)*4 + 4)
C
C CALCUL DU CENTRE DU SEGMENT LE PLUS LONG ET DE SA LONGUEUR.
C
         DUN = ABS(CUPPE1 - CUPPE2)/2.0D0
         DVN = ABS(CVPPE1 - CVPPE2)/2.0D0
         IF (CUPPE1 .LT. CUPPE2) THEN
            CUON = CUPPE1 + DUN
         ELSE
            CUON = CUPPE2 + DUN
         ENDIF
         IF (CVPPE1 .LT. CVPPE2) THEN
            CVON = CVPPE1 + DVN
         ELSE
            CVON = CVPPE2 + DVN
         ENDIF
C
         DSEGN = SQRT((CUPPE1 - CUPPE2)**2 + (CVPPE1 - CVPPE2)**2)
         RSEGN = DSEGN/2.0D0
C
C ON CHERCHE SI IL EXISTE DES POINTS Pi TELS QUE LEURS DISTANCES AU
C CENTRE 'On' SOIENT SUPERIEURES AU RAYON 'RSEGN'. SI OUI ON PREND LE
C PLUS ELOIGNE.
C SUIVANT LA POSITION DE 'On' PAR RAPPORT AU CENTRE DE LA BOITE O1,
C LA RECHERCHE EST FAITE, SOIT DANS LE DOMAINE1, SOIT DANS LE DOMAINE2.
C
         IRETH = 0
         IRETV = 0
C
         IF (ORICAD(1:5) .EQ. 'HORIZ') THEN
            IF (CVON .LT. CVO1) THEN
               CALL DIMAX2(JDOM1, NBPTD1, CUON, CVON, RSEGN,
     &                     CUPN, CVPN, IRETH)
            ELSE
               CALL DIMAX2(JDOM2, NBPTD2, CUON, CVON, RSEGN,
     &                     CUPN, CVPN, IRETH)
            ENDIF
         ELSEIF (ORICAD(1:5) .EQ. 'VERTI') THEN
            IF (CUON .LT. CUO1) THEN
               CALL DIMAX2(JDOM1, NBPTD1, CUON, CVON, RSEGN,
     &                     CUPN, CVPN, IRETV)
            ELSE
               CALL DIMAX2(JDOM2, NBPTD2, CUON, CVON, RSEGN,
     &                     CUPN, CVPN, IRETV)
            ENDIF
         ENDIF
C
C ON CHERCHE LE PLUS PETIT CERCLE CIRCONSCRIT PAR LA METHODE DU CERCLE
C PASSANT PAR TROIS POINTS.
C
         NBOUCL = 0
C
 100     CONTINUE
C
         NBOUCL = NBOUCL + 1
C
         IF (IRETH .EQ. 1) THEN
            IF (NBOUCL .EQ. 1) THEN
               CUPN0 = CUPPE1
               CVPN0 = CVPPE1
               CUPN1 = CUPPE2
               CVPN1 = CVPPE2
               CUPN2 = CUPN
               CVPN2 = CVPN
               CUON = 0.0D0
               CVON = 0.0D0
               RAY3PT = 0.0D0
               CALL CER3PT (CUPN0, CVPN0, CUPN1, CVPN1,
     &                      CUPN2, CVPN2, CUON, CVON, RAY3PT)
C
               IF (CVON .LT. CVO1) THEN
                  CALL DIMAX2(JDOM1, NBPTD1, CUON, CVON, RAY3PT,
     &                        CUPN, CVPN, IRETH)
               ELSE
                  CALL DIMAX2(JDOM2, NBPTD2, CUON, CVON, RAY3PT,
     &                        CUPN, CVPN, IRETH)
               ENDIF
               GOTO 100               
C
            ELSEIF (NBOUCL .GT. 1) THEN
               ZR(JCOORP)      = CUPN0
               ZR(JCOORP + 1)  = CVPN0
               ZR(JCOORP + 2)  = CUPN1
               ZR(JCOORP + 3)  = CVPN1
               ZR(JCOORP + 4)  = CUPN2
               ZR(JCOORP + 5)  = CVPN2
               ZR(JCOORP + 6)  = CUPN
               ZR(JCOORP + 7)  = CVPN
               ZR(JCOORP + 8)  = CUPN0
               ZR(JCOORP + 9)  = CVPN0
               ZR(JCOORP + 10) = CUPN1
               ZR(JCOORP + 11) = CVPN1
C
               RAY3PT = R8MAEM()
               K = 0
C
               DO 110 I=1, 3
                  CUPN0 = ZR(JCOORP + I*2)
                  CVPN0 = ZR(JCOORP + I*2 + 1)
                  CUPN1 = ZR(JCOORP + I*2 + 2)
                  CVPN1 = ZR(JCOORP + I*2 + 3)
                  CUPN2 = ZR(JCOORP + I*2 + 4)
                  CVPN2 = ZR(JCOORP + I*2 + 5)
                  CALL CER3PT (CUPN0, CVPN0, CUPN1, CVPN1,
     &                         CUPN2, CVPN2, CUOI, CVOI, RMIN3P)
C
                  CALL DIMAX2(JCOORP, 4, CUOI, CVOI, RMIN3P,
     &                        CUPN, CVPN, IRET3P)
C
                  IF (IRET3P .EQ. 0) THEN
                     K = K + 1
                  ENDIF
C
                  IF ((RMIN3P .LT. RAY3PT) .AND. (IRET3P .EQ. 0)) THEN
                     RAY3PT = RMIN3P
                     CUON = CUOI
                     CVON = CVOI
                     ZR(JCER3P)     = CUPN0
                     ZR(JCER3P + 1) = CVPN0
                     ZR(JCER3P + 2) = CUPN1
                     ZR(JCER3P + 3) = CVPN1
                     ZR(JCER3P + 4) = CUPN2
                     ZR(JCER3P + 5) = CVPN2
                  ENDIF
C
 110           CONTINUE
               IF (K .EQ. 0) THEN
                  CALL UTMESS('F', 'RAYCIR.1', 'AUCUN CERCLE N''EST '//
     &                        ' CIRCONSCRIT AUX QUATRE POINTS.' )
               ENDIF
C
               IF (CVON .LT. CVO1) THEN
                  CALL DIMAX2(JDOM1, NBPTD1, CUON, CVON, RAY3PT,
     &                        CUPN, CVPN, IRETH)
               ELSE
                  CALL DIMAX2(JDOM2, NBPTD2, CUON, CVON, RAY3PT,
     &                        CUPN, CVPN, IRETH)
               ENDIF
C
               IF (IRETH .EQ. 1) THEN
                  CUPN0 = ZR(JCER3P)
                  CVPN0 = ZR(JCER3P + 1)
                  CUPN1 = ZR(JCER3P + 2)
                  CVPN1 = ZR(JCER3P + 3)
                  CUPN2 = ZR(JCER3P + 4)
                  CVPN2 = ZR(JCER3P + 5)
               ENDIF
C
               GOTO 100               
            ENDIF
C
         ELSEIF (IRETV .EQ. 1) THEN
            IF (NBOUCL .EQ. 1) THEN
               CUPN0 = CUPPE1
               CVPN0 = CVPPE1
               CUPN1 = CUPPE2
               CVPN1 = CVPPE2
               CUPN2 = CUPN
               CVPN2 = CVPN
               CALL CER3PT (CUPN0, CVPN0, CUPN1, CVPN1,
     &                      CUPN2, CVPN2, CUON, CVON, RAY3PT)
C
               IF (CVON .LT. CVO1) THEN
                  CALL DIMAX2(JDOM1, NBPTD1, CUON, CVON, RAY3PT,
     &                        CUPN, CVPN, IRETV)
               ELSE
                  CALL DIMAX2(JDOM2, NBPTD2, CUON, CVON, RAY3PT,
     &                        CUPN, CVPN, IRETV)
               ENDIF
C
               GOTO 100               
C
            ELSEIF (NBOUCL .GT. 1) THEN
               ZR(JCOORP)      = CUPN0
               ZR(JCOORP + 1)  = CVPN0
               ZR(JCOORP + 2)  = CUPN1
               ZR(JCOORP + 3)  = CVPN1
               ZR(JCOORP + 4)  = CUPN2
               ZR(JCOORP + 5)  = CVPN2
               ZR(JCOORP + 6)  = CUPN
               ZR(JCOORP + 7)  = CVPN
               ZR(JCOORP + 8)  = CUPN0
               ZR(JCOORP + 9)  = CVPN0
               ZR(JCOORP + 10) = CUPN1
               ZR(JCOORP + 11) = CVPN1
C
               RAY3PT = R8MAEM()
C
               DO 120 I=1, 3
                  CUPN0 = ZR(JCOORP + I*2)
                  CVPN0 = ZR(JCOORP + I*2 + 1)
                  CUPN1 = ZR(JCOORP + I*2 + 2)
                  CVPN1 = ZR(JCOORP + I*2 + 3)
                  CUPN2 = ZR(JCOORP + I*2 + 4)
                  CVPN2 = ZR(JCOORP + I*2 + 5)
                  CALL CER3PT (CUPN0, CVPN0, CUPN1, CVPN1,
     &                         CUPN2, CVPN2, CUOI, CVOI, RMIN3P)
C
                  CALL DIMAX2(JCOORP, 4, CUOI, CVOI, RMIN3P,
     &                        CUPN, CVPN, IRET3P)
C
                  IF ((RMIN3P .LT. RAY3PT) .AND. (IRET3P .EQ. 0)) THEN
                     RAY3PT = RMIN3P
                     CUON = CUOI
                     CVON = CVOI
                     ZR(JCER3P)     = CUPN0
                     ZR(JCER3P + 1) = CVPN0
                     ZR(JCER3P + 2) = CUPN1
                     ZR(JCER3P + 3) = CVPN1
                     ZR(JCER3P + 4) = CUPN2
                     ZR(JCER3P + 5) = CVPN2
                  ENDIF
C
 120           CONTINUE
C
               IF (CVON .LT. CVO1) THEN
                  CALL DIMAX2(JDOM1, NBPTD1, CUON, CVON, RAY3PT,
     &                        CUPN, CVPN, IRETV)
               ELSE
                  CALL DIMAX2(JDOM2, NBPTD2, CUON, CVON, RAY3PT,
     &                        CUPN, CVPN, IRETV)
               ENDIF
C
               IF (IRETV .EQ. 1) THEN
                  CUPN0 = ZR(JCER3P)
                  CVPN0 = ZR(JCER3P + 1)
                  CUPN1 = ZR(JCER3P + 2)
                  CVPN1 = ZR(JCER3P + 3)
                  CUPN2 = ZR(JCER3P + 4)
                  CVPN2 = ZR(JCER3P + 5)
               ENDIF
C
               GOTO 100               
            ENDIF

         ELSE
            IF (NBOUCL .EQ. 1) THEN
               ZR(JDTAU + (IVECT-1)) = RSEGN
               ZI(JVECN + (IVECT-1)) = IVECT
            ELSEIF (NBOUCL .GT. 1) THEN
               ZR(JDTAU + (IVECT-1)) = RAY3PT
               ZI(JVECN + (IVECT-1)) = IVECT
            ENDIF
         ENDIF
C
 30   CONTINUE

      GOTO 999

 777  CONTINUE
C
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                 --------------------------
C                 |    DEUXIEME METHODE    |
C                 --------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C
      EPSILO = 1.0D-3
      X = 5.0D-2
C
C INITIALISATION
C
      DO 200 IVECT=1, NBVEC
         CUO1 = 0.0D0
         CVO1 = 0.0D0
         DO 210 IORDR=1, NBORDR
            CUO1 = CUO1 + ZR( JVECPG + (IORDR-1)*2 +
     &                       (IVECT-1)*NBORDR*2 )
            CVO1 = CVO1 + ZR( JVECPG + (IORDR-1)*2 +
     &                       (IVECT-1)*NBORDR*2 + 1 )
 210     CONTINUE
C
      CUO1 = CUO1/NBORDR
      CVO1 = CVO1/NBORDR
      RAYON = 0.0D0
C
C CALCUL RECURRENT
C
      CUON = CUO1
      CVON = CUO1
      N = 0
      NBR = 0
C
 300     CONTINUE
C
         N = N + 1
         IF ( N .GT. NBORDR ) THEN
            N = N - NBORDR
         ENDIF
C
         CUTAU = ZR(JVECPG + (N-1)*2 + (IVECT-1)*NBORDR*2)
         CVTAU = ZR(JVECPG + (N-1)*2 + (IVECT-1)*NBORDR*2 + 1)
C
         DIST = SQRT((CUTAU - CUON)**2 + (CVTAU - CVON)**2)
         P = DIST - RAYON
C
         IF (P .GT. EPSILO) THEN
            NBR = 0
            RAYON = RAYON + X*P
            CUON = CUTAU + 
     &             RAYON*((CUTAU - CUON)/SQRT((CUTAU - CUON)**2))
            CVON = CVTAU + 
     &             RAYON*((CVTAU - CVON)/SQRT((CVTAU - CVON)**2))
         ELSE
            NBR = NBR + 1
         ENDIF
C
         IF (NBR .LT. NBORDR) THEN
            GOTO 300
         ELSE
            ZR(JDTAU + (IVECT-1)) = RAYON
            ZI(JVECN + (IVECT-1)) = IVECT
         ENDIF
C
 200  CONTINUE
C
C
 999  CONTINUE
C
C MENAGE

      CALL JEDETR ('&&RAYCIR.SECT1')
      CALL JEDETR ('&&RAYCIR.SECT2')
      CALL JEDETR ('&&RAYCIR.SECT3')
      CALL JEDETR ('&&RAYCIR.SECT4')
      CALL JEDETR ('&&RAYCIR.DOM1')
      CALL JEDETR ('&&RAYCIR.DOM2')
      CALL JEDETR ('&&RAYCIR.COORP')
      CALL JEDETR ('&&RAYCIR.CER3PT')
C
      CALL JEDEMA()
      END
