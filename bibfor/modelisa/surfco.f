      SUBROUTINE SURFCO (NOMA,NZOCO,NSUCO,NMACO,NNOCO,PZONE,PSURMA,
     &                   PSURNO,CONTMA,CONTNO,METHCO,CHAMCO,COEFCO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/09/2002   AUTEUR BSERRE B.SERRE 
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
      INTEGER      NZOCO,NSUCO,NMACO,NNOCO
      CHARACTER*8  NOMA
      CHARACTER*24 PZONE,PSURMA,PSURNO,CONTMA,CONTNO,METHCO
      CHARACTER*24 CHAMCO,COEFCO
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CALICO
C ----------------------------------------------------------------------
C
C ECRITURE DES RESULTATS DE LA LECTURE DU MOT-CLE CONTACT.
C
C IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
C IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
C IN  NMACO  : NOMBRE TOTAL DE MAILLES DES SURFACES
C IN  NNOCO  : NOMBRE TOTAL DE NOEUDS DES SURFACES
C IN  NOMA   : NOM DU MAILLAGE
C IN  PZONE  : POINTEUR DES ZONES DE CONTACT
C IN  PSURMA : POINTEUR DES MAILLES DES SURFACES
C IN  PSURNO : POINTEUR DES NOEUDS DES SURFACES
C IN  CONTMA : LISTE DES NUMEROS DES MAILLES DE CONTACT
C IN  CONTNO : LISTE DES NUMEROS DES NOEUDS DE CONTACT
C IN  METHCO : PARAMETRES DE LA METHODE DE CONTACT
C IN  CHAMCO : CHAMP IMPACTE PAR LA CONDITION UNILATERALE
C              POUR CHAQUE ZONE
C                +/-1 : DEPLACEMENT
C                  -2 : PRESSION (ELEMENTS THM UNIQUEMENT)
C                  -3 : TEMPERATURE (ELEMENTS THM UNIQUEMENT)
C IN  COEFCO : COEFFICIENT MULTIPLICATEUR DE LA CONDITION UNILATERALE
C              POUR LA PRESSION OU LA TEMPERATURE
C IN  CHAMCO : PARAMETRES DE LA METHODE DE CONTACT
C IN  COEFCO : PARAMETRES DE LA METHODE DE CONTACT
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
      INTEGER      IFM,NIV,IBID
      INTEGER      JZONE,JSUMA,JSUNO,JMACO,JNOCO,JMETH,JCHAM,JCOEF
      INTEGER      NBZONE,IZONE,NBSURF,ISURF,ISUCO,NBMA,JNOMMA,JNOMNO
      INTEGER      JDECMA,NBNO,JDECNO,IMA,INO,NUMMA,NUMNO,K
      CHARACTER*8  CHAIN1,CHAIN2
      CHARACTER*24 NOEUMA,MAILMA
      CHARACTER*31 TEXT
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL INFNIV(IFM,NIV)
      MAILMA = NOMA//'.NOMMAI'
      NOEUMA = NOMA//'.NOMNOE'
C
      CALL JEVEUO (PZONE, 'L',JZONE)
      CALL JEVEUO (PSURMA,'L',JSUMA)
      CALL JEVEUO (PSURNO,'L',JSUNO)
      CALL JEVEUO (CONTMA,'L',JMACO)
      CALL JEVEUO (CONTNO,'L',JNOCO)
      CALL JEVEUO (METHCO,'L',JMETH)
      CALL JEVEUO (CHAMCO,'L',JCHAM)
      CALL JEVEUO (COEFCO,'L',JCOEF)
      CALL WKVECT ('&&SURFCO.TRAVMA','V V K8',NMACO,JNOMMA)
      CALL WKVECT ('&&SURFCO.TRAVNO','V V K8',NNOCO,JNOMNO)
C
C ======================================================================
C                    IMPRESSIONS POUR L'UTILISATEUR
C ======================================================================
C
      IF (NIV.EQ.2) THEN
C
        WRITE (IFM,*)
        WRITE (IFM,1100) '--------------------------------------'
        WRITE (IFM,1110) '          ZONES DE CONTACT '
        WRITE (IFM,1100) '--------------------------------------'
        WRITE (IFM,*)
        ISUCO = 0
        NBZONE = ZI(JMETH)
        WRITE (IFM,1000) 'NOMBRE DE ZONES    DE CONTACT : ',NBZONE
        WRITE (IFM,1000) 'NOMBRE DE SURFACES DE CONTACT : ',
     &                    ZI(JZONE+NBZONE)
        WRITE (IFM,1000) 'NOMBRE DE MAILLES  DE CONTACT : ',
     &                    ZI(JSUMA+ZI(JZONE+NBZONE))
        WRITE (IFM,1000) 'NOMBRE DE NOEUDS   DE CONTACT : ',
     &                    ZI(JSUNO+ZI(JZONE+NBZONE))
C
        DO 10 IZONE = 1,ZI(JMETH)
C
           WRITE (IFM,*)
           WRITE (IFM,1010) '************* ZONE ',IZONE,' *************'
           WRITE (IFM,*)
           IF (ZI(JCHAM+IZONE-1).EQ.1) THEN
             TEXT = 'DEPLACEMENTS (AVEC APPARIEMENT)'
           ELSE IF (ZI(JCHAM+IZONE-1).EQ.-1) THEN
             TEXT = 'DEPLACEMENTS (SANS APPARIEMENT)'
           ELSE IF (ZI(JCHAM+IZONE-1).EQ.-2) THEN
             TEXT = 'PRESSION (SANS APPARIEMENT)'
           ELSE IF (ZI(JCHAM+IZONE-1).EQ.-3) THEN
             TEXT = 'TEMPERATURE (SANS APPARIEMENT)'
           END IF
           WRITE (IFM,1120) 'CHAMP SUR LEQUEL PORTE LA CONDITION '
     &                   //' UNILATERALE : ',TEXT
           WRITE (IFM,1130) 'COEFFICIENT DE LA CONDITION '
     &                   //' UNILATERALE :         ',ZR(JCOEF+IZONE-1)
           NBSURF =  ZI(JZONE+IZONE) - ZI(JZONE+IZONE-1)
           WRITE (IFM,1020) 'NOMBRE DE SURFACES DE CETTE ZONE : ',
     &        NBSURF
           WRITE (IFM,1020) 'NOMBRE DE MAILLES  DE CETTE ZONE : ',
     &        ZI(JSUMA+ZI(JZONE+IZONE))-ZI(JSUMA+ZI(JZONE+IZONE-1))
           WRITE (IFM,1020) 'NOMBRE DE NOEUDS   DE CETTE ZONE : ',
     &        ZI(JSUNO+ZI(JZONE+IZONE))-ZI(JSUNO+ZI(JZONE+IZONE-1))
C
           IF (NIV.EQ.2) THEN
             DO 20 ISURF = 1,NBSURF
                ISUCO = ISUCO + 1
                NBMA  = ZI(JSUMA+ISUCO) - ZI(JSUMA+ISUCO-1)
                NBNO  = ZI(JSUNO+ISUCO) - ZI(JSUNO+ISUCO-1)
                JDECMA = ZI(JSUMA+ISUCO-1)
                JDECNO = ZI(JSUNO+ISUCO-1)
                WRITE (IFM,*)
                CHAIN1 = ' MAILLES'
                CHAIN2 = ' NOEUDS'
                IF (NBMA.LE.1) CHAIN1 = ' MAILLE '
                IF (NBNO.LE.1) CHAIN2 = ' NOEUD '
                WRITE (IFM,1030) '---> SURFACE  ',ISURF,' : ',NBMA,
     &                            CHAIN1,NBNO,CHAIN2
                DO 30 IMA = 1,NBMA
                   NUMMA = ZI(JMACO+JDECMA+IMA-1)
                   CALL JENUNO (JEXNUM(MAILMA,NUMMA),ZK8(JNOMMA+IMA-1))
 30             CONTINUE
                WRITE (IFM,1040) '     LISTE DES MAILLES : '
                WRITE (IFM,1050) (ZK8(JNOMMA+IMA-1),IMA=1,NBMA)
                DO 40 INO = 1,NBNO
                   NUMNO = ZI(JNOCO+JDECNO+INO-1)
                   CALL JENUNO (JEXNUM(NOEUMA,NUMNO),ZK8(JNOMNO+INO-1))
 40             CONTINUE
                WRITE (IFM,1040) '     LISTE DES NOEUDS  : '
                WRITE (IFM,1050) (ZK8(JNOMNO+INO-1),INO=1,NBNO)
 20          CONTINUE
           END IF
C
 10     CONTINUE
C
        WRITE (IFM,*)
C
      END IF
C
C ======================================================================
C                    IMPRESSIONS POUR LES DEVELOPPEURS
C ======================================================================
C
      IF (NIV.EQ.2) THEN
        WRITE (IFM,*)
        WRITE (IFM,1090) '--------------------------------------'
        WRITE (IFM,1090) '     IMPRESSIONS DE VERIFICATION      '
        WRITE (IFM,1090) '    APRES LECTURE (ROUTINE SURFCO)    '
        WRITE (IFM,1090) '--------------------------------------'
        WRITE (IFM,*)
        WRITE (IFM,1070) 'NZOCO  : ',NZOCO
        WRITE (IFM,1070) 'NSUCO  : ',NSUCO
        WRITE (IFM,1070) 'NMACO  : ',NMACO
        WRITE (IFM,1070) 'NNOCO  : ',NNOCO
        WRITE (IFM,*)
        WRITE (IFM,1080) 'METHCO : '
        WRITE (IFM,1060) (ZI(JMETH+K),  K=0,9*NZOCO)
        WRITE (IFM,1080) 'CONTMA : '
        WRITE (IFM,1060) (ZI(JMACO+K-1),K=1,NMACO)
        WRITE (IFM,1080) 'CONTNO : '
        WRITE (IFM,1060) (ZI(JNOCO+K-1),K=1,NNOCO)
        WRITE (IFM,1080) 'PZONE  : '
        WRITE (IFM,1060) (ZI(JZONE+K),  K=0,NZOCO)
        WRITE (IFM,1080) 'PSURMA : '
        WRITE (IFM,1060) (ZI(JSUMA+K),  K=0,NSUCO)
        WRITE (IFM,1080) 'PSURNO : '
        WRITE (IFM,1060) (ZI(JSUNO+K),  K=0,NSUCO)
        WRITE (IFM,*)
        WRITE (IFM,1090) '--------------------------------------'
        WRITE (IFM,*)
      END IF
C
      CALL JEDETR ('&&SURFCO.TRAVMA')
      CALL JEDETR ('&&SURFCO.TRAVNO')
C
C ----------------------------------------------------------------------
C
 1000 FORMAT ('<CONTACT_1> ',A32,I5)
 1010 FORMAT ('<CONTACT_1> ',A19,I5,A14)
 1020 FORMAT ('<CONTACT_1> ',A37,I5)
 1030 FORMAT ('<CONTACT_1> ',A13,I5,A3,I5,A8,1X,I5,A7)
 1040 FORMAT ('<CONTACT_1> ',A25)
 1050 FORMAT (('<CONTACT_1> ',10X,4(A8,1X)))
 1060 FORMAT (('<CONTACT_3> ',9X,8(I5,1X)))
 1070 FORMAT ('<CONTACT_3> ',A9,I5)
 1080 FORMAT ('<CONTACT_3> ',A9)
 1090 FORMAT ('<CONTACT_3> ',A38)
 1100 FORMAT ('<CONTACT_1> ',A38)
 1110 FORMAT ('<CONTACT_1> ',A36)
 1120 FORMAT ('<CONTACT_1> ',A51,A31)
 1130 FORMAT ('<CONTACT_1> ',A51,E12.5)
C
      CALL JEDEMA()
      END
