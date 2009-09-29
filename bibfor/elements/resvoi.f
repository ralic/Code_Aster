      SUBROUTINE RESVOI ( MOZ , MAZ , CHVOIZ )
      IMPLICIT NONE
C
C DECLARATION PARAMETRES D'APPEL
C
      CHARACTER*(*)       MOZ,  MAZ, CHVOIZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2009   AUTEUR GNICOLAS G.NICOLAS 
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
C ......................................................................
C    - FONCTION REALISEE:  RECHERCHE DES VOISINS DES ELEMENTS D'UN
C                          MAILLAGE 2D OU 3D (MAILLES DE BORD COMPRISES)
C                          ON REMPLIT CHVOIS
C
C    - ARGUMENTS:
C       IN (JXIN)           MOZ            -->  MODELE
C       IN                  MAZ            -->  NOM DU MAILLAGE
C       IN (JXVAR)          CHVOIZ         -->  CHAM_ELEM VOISIN
C
C ......................................................................
C
C DECLARATION VARIABLES LOCALES
C
      CHARACTER*32     NOE1,NOE2,NOE3,NOE4
      CHARACTER*24     TYPMAI, CONNEX, CONINV
      CHARACTER*8      MA, KBID, TYPEMA, MO
      CHARACTER*19     LIGRMO, CHVOIS
      INTEGER          IBID
      INTEGER IBIDT(1)
      INTEGER          NBNO, NBMA, NBS,NBF,TYMVOL
      INTEGER          IMA, INO, INO1,INO2,INO3,INO4, KMA, JMA
      INTEGER          IAMAV1,IAMAV2,IAMAV3,IAMAV4,IAREPE,IAVALE
      INTEGER          IFA,IMA1,IMA2,IMA3,IMA4,IER
      INTEGER          IGREL, IEL, IAVAL1, IAVAL2, JAD, IAD, IADV
      INTEGER          JCELD,NBMAV1,NBMAV2,NBMAV3,NBMAV4
      INTEGER          NUMAV1,NUMAV2,NUMAV3,NUMAV4,TYP,SOM(4,6,4),IATYMA
      LOGICAL          TROISD
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM 
      INTEGER            ZI,DEBUGR
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
      CHARACTER*1 K1BID
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C   INITIALISATION DES NUMEROS DE SOMMETS DES FACES D'ELEMENTS 3D
C     SOM (IN,IFA,TYMVOL) : IN     : NUMERO DU SOMMET DANS LA FACE
C                           IFA    : NUMERO DE LA FACE
C                           TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
C                                    1 : HEXAEDRES
C                                    2 : PENTAEDRES
C                                    3 : TETRAEDRES
C                                    4 : PYRAMIDES
C  ==> POUR LE IN-EME SOMMET DE LA IFA-EME FACE D'UNE MAILLE DE TYPE
C      TYMVOL, SOMMET (IN,IFA,TYMVOL) EST SON NUMERO LOCAL DANS LA
C      DESCRIPTION DE LA MAILLE VOLUMIQUE.
C      ON RAPPELLE QU'EN FORTRAN L'ORDRE DE RANGEMENT EST LE SUIVANT :
C   (1,1,1) (2,1,1) (3,1,1) (4,1,1) (1,2,1) (2,2,1) ... (4,2,1)
C   (1,3,1)  ...    (3,6,4) (4,6,4)
C    ON COMMENCE AINSI PAR LES 4 SOMMETS DE LA 1ERE FACE DE L'HEXAEDRE,
C    PUIS LES 4 SOMMETS DE LA 2EME FACE DE L'HEXAEDRE,
C    ETC JUSQU'AUX 4 SOMMETS DE LA 6EME FACE DE L'HEXAEDRE.
C    ENSUITE ON A LES 3 SOMMETS DE LA 1ERE FACE DU PENTAEDRE, ETC
C    ON CHOISIT UNE ORIENTATION ENTRANTE POUR DECRIRE UNE FACE
C     VOIR TE003 POUR LES EXPLICATIONS DETAILLEES
C
      DATA SOM/1,2,3,4, 1,5,6,2, 2,6,7,3, 3,7,8,4, 4,8,5,1, 5,8,7,6,
     &         1,2,3,0, 4,6,5,0, 1,4,5,2, 2,5,6,3, 1,3,6,4, 0,0,0,0,
     &         1,2,3,0, 2,4,3,0, 3,4,1,0, 1,4,2,0, 0,0,0,0, 0,0,0,0,
     &         1,5,2,0, 2,5,3,0, 3,5,4,0, 4,5,1,0, 1,2,3,4, 0,0,0,0/
C
C --------- CONSTRUCTION DE LA CONNECTIVITE INVERSE --------------------
C
      CALL JEMARQ()
      CHVOIS = CHVOIZ
      MO     = MOZ
      MA     = MAZ
      CALL DISMOI ('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNO,KBID,IER)
      CALL DISMOI ('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMA,KBID,IER)
C
      TYPMAI = MA//'.TYPMAIL'
      CONNEX = MA//'.CONNEX'
C
C --------- RECHERCHE DES EVENTUELLES MAILLES 3D DANS LE MODELE --------
C
      TROISD = .FALSE.
      CALL JEVEUO (TYPMAI,'L',IATYMA)
      DO 500 IMA = 1,NBMA
        IAD=IATYMA-1+IMA
        CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',ZI(IAD)),TYPEMA)
        IF (TYPEMA(1:2) .EQ. 'TE' .OR. TYPEMA(1:2) .EQ. 'PE' .OR.
     &      TYPEMA(1:2) .EQ. 'HE') THEN
           TROISD = .TRUE.
           GO TO 600
        ENDIF
  500 CONTINUE
        IF(NBMA.LT.1) GOTO 700
C
C
C --------- CREATION DU POINTEUR DE LONGUEUR DE CONINV ----------------
C
600   CONTINUE
      CONINV = '&&RESVOI.CONINV'
      CALL CNCINV(MA,IBIDT,0,'G',CONINV)
C
C ----------- RECHERCHE DES ADRESSES DE STOCKAGE POUR CHVOIS -------
C
      LIGRMO = MO//'.MODELE'
      CALL JEVEUO (LIGRMO//'.REPE','L',IAREPE)

C     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
      CALL CELVER(CHVOIS,'NBVARI_CST','STOP',IBID)
      CALL CELVER(CHVOIS,'NBSPT_1','STOP',IBID)

      CALL JEVEUO (CHVOIS//'.CELD','L',JCELD)
      CALL JEVEUO (CHVOIS//'.CELV','E',IAVALE)
C
C   RECHERCHE DES VOISINS DE CHAQUE MAILLE
C
      IF(TROISD) THEN
C
C    CAS 3D
C
C ----------- BOUCLE SUR LES MAILLES -------------------------------
C
      DO 801 , IMA = 1 , NBMA
C
        CALL JEVEUO (JEXNUM(CONNEX,IMA),'L',JAD)
        IAD=IATYMA-1+IMA
        CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',ZI(IAD)),TYPEMA)
C
        IF (TYPEMA(1:4) .EQ. 'HEXA')THEN
           NBF = 6
           TYMVOL = 1
        ELSE IF (TYPEMA(1:4) .EQ. 'PENT' ) THEN
           NBF = 5
           TYMVOL = 2
        ELSE IF (TYPEMA(1:4) .EQ. 'TETR' ) THEN
           NBF = 4
           TYMVOL = 3
        ELSE IF (TYPEMA(1:4) .EQ. 'PYRA' ) THEN
           NBF = 5
           TYMVOL = 4
CGN        WRITE(6,*) '. MAILLE ',IMA
        ELSE
           GOTO 801
        ENDIF
C
        IGREL = ZI(IAREPE-1+2*(IMA-1)+1)
        IEL = ZI(IAREPE-1+2*(IMA-1)+2)
        IF (IEL .EQ. 0) GO TO 801
        DEBUGR=ZI(JCELD-1+ZI(JCELD-1+4+IGREL)+8)
        IAVAL1 = IAVALE - 1 + DEBUGR
        IAVAL2 = IAVAL1 + 14*(IEL-1)
C
C ---------- BOUCLE SUR LES FACES DE LA MAILLE -------------------
C      NBMAVI = NOMBRE DE MAILLES POSSEDANT LE I-EME SOMMET
C               S'IL N'Y EN A QU'UNE, C'EST LA MAILLE COURANTE DONC
C               IL N'Y A PAS DE VOISINS PAR CETTE FACE : ON PASSE
C               A LA FACE SUIVANTE.
C
        DO 802 , IFA = 1 , NBF
CGN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '.. FACE NUMERO ',IFA
          INO1 = SOM(1,IFA,TYMVOL)
          NOE1 = JEXNUM(CONINV,ZI(JAD-1+INO1))
CGN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... INO1 ',INO1
          CALL JELIRA (NOE1,'LONMAX',NBMAV1,K1BID)
          IF ( NBMAV1.EQ.1 ) GO TO 802
CGN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... NBMAV1 ',NBMAV1
          CALL JEVEUO (NOE1,'L',IAMAV1)
C
          INO2 = SOM(2,IFA,TYMVOL)
CGN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... INO2 ',INO2
          NOE2 = JEXNUM(CONINV,ZI(JAD-1+INO2))
          CALL JELIRA (NOE2,'LONMAX',NBMAV2,K1BID)
CGN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... NBMAV2 ',NBMAV2
          IF ( NBMAV2.EQ.1 ) GO TO 802
          CALL JEVEUO (NOE2,'L',IAMAV2)
C
          INO3 = SOM(3,IFA,TYMVOL)
CGN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... INO3 ',INO3
          NOE3 = JEXNUM(CONINV,ZI(JAD-1+INO3))
          CALL JELIRA (NOE3,'LONMAX',NBMAV3,K1BID)
CGN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... NBMAV3 ',NBMAV3
          IF ( NBMAV3.EQ.1 ) GO TO 802
          CALL JEVEUO (NOE3,'L',IAMAV3)
C
CAS DES FACES QUADRANGULAIRES
          INO4 = SOM(4,IFA,TYMVOL)
CGN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... INO4 ',INO4
          IF (INO4 .NE. 0) THEN
            NOE4 = JEXNUM(CONINV,ZI(JAD-1+INO4))
            CALL JELIRA (NOE4,'LONMAX',NBMAV4,K1BID)
CGN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... NBMAV4 ',NBMAV4
            IF ( NBMAV4.EQ.1 ) GO TO 802
            CALL JEVEUO (NOE4,'L',IAMAV4)
          ENDIF
C
C REPERAGE ET STOCKAGE DES VOISINS
C  BOUCLE 803 : ON REGARDE TOUTES LES MAILLES POSSEDANT LE 1ER SOMMET
C               DE LA MAILLE COURANTE, IMA, ET QUI NE SONT PAS IMA
C
          DO 803 , IMA1 = 1,NBMAV1
            NUMAV1 = ZI(IAMAV1-1+IMA1)
            IF ( NUMAV1.NE.IMA ) THEN
C
C  BOUCLE 804 : ON REGARDE TOUTES LES MAILLES POSSEDANT LE 2EME SOMMET
C               DE LA MAILLE COURANTE
C               ON RETIENT CELLES QUI POSSEDENT AUSSI LE PREMIER
C
              DO 804 , IMA2 = 1,NBMAV2
                NUMAV2 = ZI(IAMAV2-1+IMA2)
                IF ( NUMAV2.EQ.NUMAV1 ) THEN
C
C  BOUCLE 805 : ON REGARDE TOUTES LES MAILLES POSSEDANT LE 3EME SOMMET
C               DE LA MAILLE COURANTE
C               ON RETIENT CELLES QUI POSSEDENT AUSSI LE PREMIER
C               ELLES POSSEDENT AUSSI LE DEUXIEME.
C
                  DO 805 , IMA3 = 1,NBMAV3
                    NUMAV3 = ZI(IAMAV3-1+IMA3)
                    IF (NUMAV3.EQ.NUMAV1) THEN
C
C       CAS DES FACES QUADRANGULAIRES
C  BOUCLE 806 : ON REGARDE TOUTES LES MAILLES POSSEDANT LE 4EME SOMMET
C               DE LA MAILLE COURANTE
C               ON RETIENT CELLES QUI POSSEDENT AUSSI LE PREMIER
C               ELLES POSSEDENT AUSSI LE DEUXIEME ET LE TROISIEME.
                      IF ( INO4.NE.0 ) THEN
C
                        DO 806 IMA4 = 1,NBMAV4
                          NUMAV4 = ZI(IAMAV4-1+IMA4)
                         IF (NUMAV4 .EQ. NUMAV1) THEN
C- ------STOCKAGE DU NUMERO DU VOISIN ET DE SON TYPE ----------------
                            ZI(IAVAL2+IFA) = NUMAV1
                            IADV=IATYMA-1+NUMAV1
                            TYP = ZI(IADV)
                            ZI(IAVAL2+IFA+7) = TYP
                            GOTO 802
                          ENDIF
  806                   CONTINUE
C
                      ELSE
C
C       CAS DES FACES TRIANGULAIRES
C-------STOCKAGE DU NUMERO DU VOISIN ET DE SON TYPE ----------------
                        ZI(IAVAL2+IFA) = NUMAV1
                        IADV=IATYMA-1+NUMAV1
                        TYP = ZI(IADV)
                        ZI(IAVAL2+IFA+7) = TYP
                        GOTO 802
C
                      ENDIF
                    ENDIF
C
  805             CONTINUE
                ENDIF
C
  804         CONTINUE
C
            ENDIF
C
  803     CONTINUE
  802   CONTINUE
C
C --------- STOCKAGE DU NUMERO DE L'ELEMENT ET DE SON TYPE -------------
C
        ZI(IAVAL2) = IMA
        TYP = ZI(IAD)
        ZI(IAVAL2+7) = TYP
C
  801 CONTINUE
C
C
      ELSE
C
C    CAS 2D
C
C ----------- BOUCLE SUR LES MAILLES -------------------------------
C
      DO 601 IMA = 1,NBMA
C
        CALL JEVEUO (JEXNUM(CONNEX,IMA),'L',JAD)
C
        IAD=IATYMA-1+IMA
        CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',ZI(IAD)),TYPEMA)
         IF (TYPEMA(1:4) .EQ. 'QUAD')THEN
                NBS = 4
           ELSE IF (TYPEMA(1:4) .EQ. 'TRIA' ) THEN
                NBS = 3
           ELSE
                GOTO 601
         ENDIF
C
        IGREL = ZI(IAREPE-1+2*(IMA-1)+1)
        IEL = ZI(IAREPE-1+2*(IMA-1)+2)
        IF (IEL .EQ. 0) GO TO 601
        DEBUGR=ZI(JCELD-1+ZI(JCELD-1+4+IGREL)+8)
        IAVAL1 = IAVALE - 1 + DEBUGR
C
C ---------- BOUCLE SUR LES SOMMETS DE LA MAILLE -------------------
C
        DO 602 INO = 1,NBS
          CALL JELIRA (JEXNUM(CONINV,ZI(JAD-1+INO)),'LONMAX',
     +                NBMAV1,K1BID)
          CALL JEVEUO (JEXNUM(CONINV,ZI(JAD-1+INO)),'L',IAMAV1)
C
          DO 603 KMA = 1,NBMAV1
            NUMAV1 = ZI(IAMAV1-1+KMA)
            IF (NUMAV1 .NE. IMA) THEN
C
              IF (INO .EQ. NBS) THEN
                INO2 = 1
                ELSE
                INO2 = INO + 1
              ENDIF
C
              CALL JELIRA (JEXNUM(CONINV,ZI(JAD-1+INO2)),'LONMAX'
     &                    ,NBMAV2,K1BID)
              CALL JEVEUO (JEXNUM(CONINV,ZI(JAD-1+INO2)),'L',IAMAV2)
C
              DO 604 JMA = 1,NBMAV2
                NUMAV2 = ZI(IAMAV2-1+JMA)
                IF (NUMAV2 .EQ. NUMAV1) THEN
C
C --------- STOCKAGE DU NUMERO DU VOISIN ET DE SON TYPE ----------------
C
                  ZI(IAVAL1+14*(IEL-1)+INO) = NUMAV1
                  IADV=IATYMA-1+NUMAV1
                  TYP = ZI(IADV)
                  ZI(IAVAL1+14*(IEL-1)+INO+7) = TYP
C
                  GOTO 602
                ENDIF
  604         CONTINUE
            ENDIF
  603     CONTINUE
  602   CONTINUE
C
C --------- STOCKAGE DU NUMERO DE L'ELEMENT ET DE SON TYPE -------------
C
        ZI(IAVAL1+14*(IEL-1)) = IMA
        TYP = ZI(IAD)
        ZI(IAVAL1+14*(IEL-1)+7) = TYP
  601 CONTINUE
      ENDIF
      CALL JEDETR( '&&RESVOI.LONGCONINV' )
      CALL JEDETR( '&&RESVOI.CONINV'     )
      CALL JEDEMA()
  700 CONTINUE
      END
