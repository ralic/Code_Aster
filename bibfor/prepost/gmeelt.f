      SUBROUTINE  GMEELT(NBTYMA, NOMAIL, NBMAIL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 31/03/2003   AUTEUR JMBHH01 J.M.PROIX 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      GMEELT --   ECRITURE DES MAILLES ET DES GROUP_MA VENANT
C                  D'UN FICHIER .GMSH DANS LE FICHIER .MAIL
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NBTYMA         IN    I         NOMBRE  DE TYPES DE MAILLES
C    NOMAIL(*)      IN    K8        TABLEAU DES NOMS DES TYPES DE MAILLE
C    NBMAIL         IN    I         NOMBRE TOTAL DE MAILLES
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           INTEGER     NBTYMA, NBMAIL
           CHARACTER*8 NOMAIL(*)
C -----  VARIABLES LOCALES
           INTEGER      NEU2(32), IER
           CHARACTER*1  PRFNOE, PRFMAI
           CHARACTER*4  CT(3)
           CHARACTER*8  CHGROU, CHTAB(32), CHMAIL, K8BID
           CHARACTER*12 AUT, CHENTI
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
      CHARACTER*32 JEXNOM, JEXNUM
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C.========================= DEBUT DU CODE EXECUTABLE ==================
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      PRFMAI   = 'M'
      PRFNOE   = 'N'
      CHGROU   = '        '
      CHMAIL   = '        '
      K8BID    = '        '
      CHENTI   = 'NBOBJ=      '
C
      DO 10 I = 1, 32
        CHTAB(I) = '        '
  10  CONTINUE
C
C --- RECUPERATION DE L'UNITE LOGIQUE DE SORTIE :
C     -----------------------------------------
      IMOD = IUNIFI('FICHIER-MODELE')
C
C --- RECUPERATION DES OBJETS DE TRAVAIL :
C     ----------------------------------
      CALL JEVEUO('&&PREGMS.NUMERO.MAILLES','L',JNUMA)
      CALL JEVEUO('&&PREGMS.TYPE.MAILLES','L',JTYPMA)
      CALL JEVEUO('&&PREGMS.NBNO.MAILLES','L',JNBNMA)
      CALL JEVEUO('&&PREGMS.CONNEC.MAILLES','L',JNOMA)
      CALL JEVEUO('&&PREGMS.NBMA.GROUP_MA','L',JNBMAG)
      CALL JEVEUO('&&PREGMS.NBTYP.MAILLES','L',JNBTYM)
      CALL JEVEUO('&&PREGMS.INDICE.GROUP_MA','L',JINDMA)
C
C --- ECRITURE DES MAILLES :
C     --------------------
      DO 20 NTE = 1, NBTYMA
C
        IF (ZI(JNBTYM+NTE-1).EQ.0) GOTO 20
        CALL CODENT(ZI(JNBTYM+NTE-1),'G',CHENTI(7:12))
C
C ---   ECRITURE DE LA DATE :
C       -------------------
C        CALL JJMMAA(CT,AUT)
        WRITE(UNIT=IMOD,FMT='(A,3X,A,3X,A)') NOMAIL(NTE),
     +    'NOM=INDEFINI',CHENTI
C        WRITE(IMOD,'(11X,2A,11X,A,A2,A,A2,A,A4)') 'AUTEUR=',AUT,
C     +    'DATE=',CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
C
        IJ = 0
C
C ---   BOUCLE SUR LES MAILLES :
C       ----------------------
        DO 30 IMA = 1, NBMAIL
          ITYP = ZI(JTYPMA+IMA-1)
          NBNO = ZI(JNBNMA+IMA-1)
          IF (ITYP.EQ.NTE) THEN
            INUM = ZI(JNUMA+IMA-1)
            CALL CODNOP(CHMAIL,PRFMAI,1,1)
            CALL CODENT(INUM,'G',CHMAIL(2:8))
C
            DO 40 INO = 1, NBNO
              NEU2(INO) = ZI(JNOMA+IJ+INO-1)
              CALL CODNOP(CHTAB(INO),PRFNOE,1,1)
              CALL CODENT(NEU2(INO),'G',CHTAB(INO)(2:8))
  40        CONTINUE
C
            IDIV  = INT(NBNO/8)
            IREST = MOD(NBNO,8)
C
            IF (IREST.NE.0) THEN
              WRITE(IMOD,202) CHMAIL,(CHTAB(I),I=1,NBNO)
            ELSE
              DO 1000 K = 1, IDIV
                L = 8*(K-1)
                IF (IDIV.EQ.1) THEN
                  WRITE(IMOD,'(A,8(1X,A))') CHMAIL,(CHTAB(I),I=1+L,8+L)
                ELSE
                  WRITE(IMOD,'(8X,8(1X,A))') (CHTAB(I),I=1+L,8+L)
                ENDIF
 1000         CONTINUE
            ENDIF
          ENDIF
          IJ = IJ + NBNO
  30    CONTINUE
C
        WRITE(IMOD,'(A)') 'FINSF'
        WRITE(IMOD,'(A)') '%'
C
  20  CONTINUE
C
 202  FORMAT(A,8(1X,A),/,(8X,8(1X,A)))
C
C --- ECRITURE DES GROUP_MA :
C     ---------------------
      CALL JELIRA('&&PREGMS.INDICE.GROUP_MA','LONUTI',INDMAX,K8BID)
C
      MAXMAI = 0
      DO 50 I =1, INDMAX
        MAXMAI = MAX(MAXMAI,ZI(JNBMAG+I-1))
  50  CONTINUE
C
      CALL WKVECT('&&PREGMS.GRMA.MAILLES','V V K8',MAXMAI,JGRMAI)
C
      CHGROU(1:2) = 'GM'
C
C --- BOUCLE SUR LES GROUPES DE MAILLES :
C     ---------------------------------
      IER = 0
      DO 60 I = 1, INDMAX
        NUMGRO = ZI(JINDMA+I-1)
        IF ( NUMGRO .GE. 1000000 ) THEN
           IER = IER + 1
           CALL UTDEBM('E','GMEELT','LE NUMERO DU GROUPE DE MAILLES')
           CALL UTIMPI('S',' EST TROP GRAND: ',1,NUMGRO)
           CALL UTIMPI('L',
     +         ' LE NUMERO DU GROUPE DOIT ETRE INFERIEUR A ',1,1000000)
           CALL UTFINM
           GOTO 60
        ENDIF
        CALL CODENT(NUMGRO,'G',CHGROU(3:8))
        WRITE(IMOD,'(A,4X,2A)') 'GROUP_MA','NOM=',CHGROU
        CALL JEVEUO(JEXNUM('&&PREGMS.LISTE.GROUP_MA',I),'E',JGR)
        DO 70 K = 1, ZI(JNBMAG+I-1)
          CALL CODNOP(CHMAIL,PRFMAI,1,1)
          CALL CODENT(ZI(JGR+K-1),'G',CHMAIL(2:8))
          ZK8(JGRMAI+K-1) = CHMAIL
  70    CONTINUE
C
C ---   ECRITURE DES MAILLES DU GROUPE DE MAILLES COURANT :
C       -------------------------------------------------
        WRITE(IMOD,'(8(2X,A))') (ZK8(JGRMAI+K-1),K=1,ZI(JNBMAG+I-1))
C
        WRITE(IMOD,'(A)') 'FINSF'
        WRITE(IMOD,'(A)') '%'
C
C --- DANS LE CAS D'UN POINT ECRITURE D'UN GROUPNO
C ---  LE GROUPE DE MAILLE CONTIENT ALORS UNE SEULE MAILLE POI1

        IF (ZI(JNBMAG+I-1).EQ.1) THEN
           CALL JEVEUO(JEXNUM('&&PREGMS.LISTE.GROUP_MA',I),'E',JGR)
           IMA1=ZI(JGR)
           IJ=0
           DO 80 IMA = 1, NBMAIL
              INUM = ZI(JNUMA+IMA-1)
              NBNO = ZI(JNBNMA+IMA-1)
              IF (INUM.EQ.IMA1) THEN
                 IF (NBNO.EQ.1) THEN
                    WRITE(IMOD,'(A,4X,2A)') 'GROUP_NO','NOM=',CHGROU
                    NEU2(INO) = ZI(JNOMA+IJ)
                    CALL CODNOP(CHTAB(INO),PRFNOE,1,1)
                    CALL CODENT(NEU2(INO),'G',CHTAB(INO)(2:8))
                    WRITE(IMOD,'((2X,A))') CHTAB(INO)
                    WRITE(IMOD,'(A)') 'FINSF'
                    WRITE(IMOD,'(A)') '%'
                    GOTO 90
                 ENDIF
              ENDIF
            IJ = IJ + NBNO
  80       CONTINUE
        ENDIF
  90    CONTINUE

  60  CONTINUE
  
  
      IF ( IER .NE. 0 ) THEN
         CALL UTMESS('F','GMEELT','ARRET SUR ERREUR(S)')
      ENDIF
C
      CALL JEDEMA()
C
C.============================ FIN DE LA ROUTINE ======================
      END
