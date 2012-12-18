      SUBROUTINE  GMEELT(IMOD, NBTYMA, NOMAIL, NBNOMA, NUCONN, NBMAIL)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER     IMOD, NBTYMA, NBMAIL, NBNOMA(19),NUCONN(19,32)
      CHARACTER*8 NOMAIL(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C
C      GMEELT --   ECRITURE DES MAILLES ET DES GROUP_MA VENANT
C                  D'UN FICHIER .GMSH DANS LE FICHIER .MAIL
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NBTYMA         IN    I         NOMBRE  DE TYPES DE MAILLES
C    NOMAIL(*)      IN    K8        TABLEAU DES NOMS DES TYPES DE MAILLE
C    NBMAIL         IN    I         NOMBRE TOTAL DE MAILLES
C    NUCONN         IN    I         PASSAGE DE LA NUMEROTATION DES NDS
C                                     D'UNE MAILLE : ASTER -> GMSH
C
C
C
C
      INTEGER      NEU2(32),IER,I,IJ,NTE,IMA,ITYP,NBNO,INUM,NBNOAS
      INTEGER      IDIV,INO,IREST,K,L,MAXMAI,NUMGRO,JGRMAI,JGR,IMA1
      INTEGER      INDMAX,VALI(2)
      INTEGER      JNUMA,JTYPMA,JNBNMA,JNOMA,JNBMAG,JNBTYM,JINDMA
      CHARACTER*1  PRFNOE,PRFMAI
      CHARACTER*8  CHGROU,CHTAB(32),CHMAIL,K8BID
      CHARACTER*12 CHENTI
C
C ----------------------------------------------------------------------
C
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
        WRITE(UNIT=IMOD,FMT='(A,3X,A,3X,A)') NOMAIL(NTE),
     &    'NOM=INDEFINI',CHENTI
C
        IJ = 0
C
C --- ON VERIFIE QUE LE NOMBRE MAX DE MAILLES N'EST PAS ATTEINT
C     LA LIMITE EST DE 9 999 999 MAILLES
C
      IF(NBMAIL.GE.10000000) THEN
        VALI (1) = NBMAIL
        CALL U2MESG('E', 'PREPOST6_43',0,' ',1,VALI,0,0.D0)
      ENDIF
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
            NBNOAS = NBNOMA(NTE)

            DO 40 INO = 1, NBNOAS
              NEU2(INO) = ZI(JNOMA+IJ+NUCONN(NTE,INO)-1)
              CALL CODNOP(CHTAB(INO),PRFNOE,1,1)
              CALL CODENT(NEU2(INO),'G',CHTAB(INO)(2:8))
  40        CONTINUE
C
            IDIV  = INT(NBNOAS/8)
            IREST = MOD(NBNOAS,8)
C
            IF (IREST.NE.0) THEN
              WRITE(IMOD,202) CHMAIL,(CHTAB(I),I=1,NBNOAS)
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
      IER = 0
      CALL JELIRA('&&PREGMS.INDICE.GROUP_MA','LONUTI',INDMAX,K8BID)
C
      MAXMAI = 0
      DO 50 I =1, INDMAX
        MAXMAI = MAX(MAXMAI,ZI(JNBMAG+I-1))
  50  CONTINUE
C
C --- SI IL N Y A AU MOINS UN GROUPE :
C     ------------------------------
      IF (MAXMAI.NE.0) THEN
C
        CALL WKVECT('&&PREGMS.GRMA.MAILLES','V V K8',MAXMAI,JGRMAI)
C
        CHGROU(1:2) = 'GM'
C
C --- BOUCLE SUR LES GROUPES DE MAILLES :
C     ---------------------------------
        DO 60 I = 1, INDMAX
          NUMGRO = ZI(JINDMA+I-1)
          IF ( NUMGRO .GE. 1000000 ) THEN
             IER = IER + 1
             VALI (1) = NUMGRO
             VALI (2) = 1000000
             CALL U2MESG('E', 'PREPOST5_21',0,' ',2,VALI,0,0.D0)
             GOTO 60
          ENDIF
          CALL CODENT(NUMGRO,'G',CHGROU(3:8))
          WRITE(IMOD,'(A,4X,2A)') 'GROUP_MA','NOM=',CHGROU
          CALL JEVEUO(JEXNUM('&&PREGMS.LISTE.GROUP_MA',I),'E',JGR)
          DO 70 K = 1, ZI(JNBMAG+I-1)
            CALL CODNOP(CHMAIL,PRFMAI,1,1)
            CALL CODENT(ZI(JGR+K-1),'G',CHMAIL(2:8))
            ZK8(JGRMAI+K-1) = CHMAIL
  70      CONTINUE
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
  80         CONTINUE
          ENDIF
  90      CONTINUE

  60    CONTINUE
C
      ENDIF

      IF ( IER .NE. 0 ) THEN
         CALL U2MESS('F','PREPOST_60')
      ENDIF
C
      CALL JEDEMA()
C
C.============================ FIN DE LA ROUTINE ======================
      END
