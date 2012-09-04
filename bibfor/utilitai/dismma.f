      SUBROUTINE DISMMA(QUESTI,NOMOBZ,REPI,REPKZ,IERD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 04/09/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
C     --     DISMOI(MAILLAGE)
C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI
      CHARACTER*32 REPK
      CHARACTER*8 NOMOB
      CHARACTER*(*) NOMOBZ,REPKZ
C ----------------------------------------------------------------------
C    IN:
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOB  : NOM D'UN OBJET DE TYPE MAILLAGE
C    OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPK   : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)

C ----------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      COMPLEX*16 C16B
      CHARACTER*19 TABLE
      CHARACTER*1 K1BID
      REAL*8  ZMAX,ZMIN
      INTEGER JDIME,IBID,IER,ILMACO,ISM,K,NBMA,NBNO
      INTEGER NBSM,NNO,TYPV,JTYPMA
      CHARACTER*8 KBID,TYPMA



      CALL JEMARQ()
      REPK  = ' '
      REPI  = 0
      IERD = 0

      NOMOB = NOMOBZ
      CALL JEVEUO(NOMOB//'.DIME','L',JDIME)


      IF (QUESTI.EQ.'NB_MA_MAILLA') THEN
C     ---------------------------------
        REPI = ZI(JDIME-1+3)


      ELSE IF (QUESTI.EQ.'NB_SM_MAILLA') THEN
C     ---------------------------------
        REPI = ZI(JDIME-1+4)


      ELSE IF (QUESTI.EQ.'NB_NO_MAILLA') THEN
C     ---------------------------------
        REPI = ZI(JDIME-1+1)


      ELSE IF (QUESTI.EQ.'NB_NL_MAILLA') THEN
C     ---------------------------------
        REPI = ZI(JDIME-1+2)


      ELSE IF (QUESTI.EQ.'NB_NO_SS_MAX') THEN
C     ---------------------------------
        NBSM = ZI(JDIME-1+4)
        REPI = 0
        DO 10,ISM = 1,NBSM
          CALL JELIRA(JEXNUM(NOMOB//'.SUPMAIL',ISM),'LONMAX',NNO,KBID)
          REPI = MAX(REPI,NNO)
   10   CONTINUE


      ELSE IF (QUESTI.EQ.'Z_CST') THEN
C     ---------------------------------
        CALL LTNOTB ( NOMOB, 'CARA_GEOM' , TABLE )
        CALL TBLIVA(TABLE,0,' ',IBID,0.D0,C16B,K1BID,'ABSO',
     &          0.D0,'Z_MIN',K1BID,IBID,ZMIN,C16B,K1BID,IER )
        CALL TBLIVA(TABLE,0,' ',IBID,0.D0,C16B,K1BID,'ABSO',
     &          0.D0,'Z_MAX',K1BID,IBID,ZMAX,C16B,K1BID,IER )

        IF (ZMIN.EQ.ZMAX) THEN
          REPK = 'OUI'
        ELSE
          REPK = 'NON'
        ENDIF


      ELSE IF (QUESTI.EQ.'Z_ZERO'.OR.QUESTI.EQ.'DIM_GEOM') THEN
C     ------------------------------------------------------------
        CALL LTNOTB( NOMOB, 'CARA_GEOM' , TABLE )
        CALL TBLIVA(TABLE,0,' ',IBID,0.D0,C16B,K1BID,'ABSO',
     &          0.D0,'Z_MIN',K1BID,IBID,ZMIN,C16B,K1BID,IER )
        CALL TBLIVA(TABLE,0,' ',IBID,0.D0,C16B,K1BID,'ABSO',
     &          0.D0,'Z_MAX',K1BID,IBID,ZMAX,C16B,K1BID,IER )

        IF (ZMIN.EQ.ZMAX.AND. ZMIN.EQ.0.D0) THEN
          REPK = 'OUI'
        ELSE
          REPK = 'NON'
        ENDIF

        IF (QUESTI.EQ.'DIM_GEOM') THEN
C     ----------------------------------------
           REPI = ZI(JDIME-1+6)
C          -- ON RETOURNE 2 SI Z=0. PARTOUT :
           IF ((REPI.EQ.3).AND.(REPK.EQ.'OUI')) THEN
             REPI=2
           ENDIF
           REPK='???'
        ENDIF


      ELSE IF (QUESTI.EQ.'DIM_GEOM_B') THEN
C     ----------------------------------------
        REPI = ZI(JDIME-1+6)
        REPK='???'


      ELSE IF (QUESTI.EQ.'NB_NO_MA_MAX') THEN
C     ----------------------------------------
        NBMA = ZI(JDIME-1+3)
        CALL JEVEUO(JEXATR(NOMOB//'.CONNEX','LONCUM'),'L',ILMACO)
        REPI = 0
        DO 40,K = 1,NBMA
          NBNO = ZI(ILMACO+K) - ZI(ILMACO-1+K)
          REPI = MAX(REPI,NBNO)
   40   CONTINUE


      ELSE IF ((QUESTI.EQ.'EXI_TRIA3'  ) .OR.
     &         (QUESTI.EQ.'EXI_TRIA6'  ) .OR.
     &         (QUESTI.EQ.'EXI_QUAD4'  ) .OR.
     &         (QUESTI.EQ.'EXI_QUAD8'  ) .OR.
     &         (QUESTI.EQ.'EXI_QUAD9'  ) .OR.
     &         (QUESTI.EQ.'EXI_SEG2'   ) .OR.
     &         (QUESTI.EQ.'EXI_SEG3'   ) .OR.
     &         (QUESTI.EQ.'EXI_HEXA8'  ) .OR.
     &         (QUESTI.EQ.'EXI_HEXA20' ) .OR.
     &         (QUESTI.EQ.'EXI_HEXA27' ) .OR.
     &         (QUESTI.EQ.'EXI_PENTA6' ) .OR.
     &         (QUESTI.EQ.'EXI_PENTA15') .OR.
     &         (QUESTI.EQ.'EXI_TETRA4' ) .OR.
     &         (QUESTI.EQ.'EXI_TETRA10') .OR.
     &         (QUESTI.EQ.'EXI_PYRAM5' ) .OR.
     &         (QUESTI.EQ.'EXI_PYRAM13') .OR.
     &         (QUESTI.EQ.'EXI_POI1'   )) THEN
C     ----------------------------------------
        TYPMA='XXXX'
        IF (QUESTI.EQ.'EXI_TRIA3'  ) TYPMA='TRIA3'
        IF (QUESTI.EQ.'EXI_TRIA6'  ) TYPMA='TRIA6'
        IF (QUESTI.EQ.'EXI_QUAD4'  ) TYPMA='QUAD4'
        IF (QUESTI.EQ.'EXI_QUAD8'  ) TYPMA='QUAD8'
        IF (QUESTI.EQ.'EXI_QUAD9'  ) TYPMA='QUAD9'
        IF (QUESTI.EQ.'EXI_SEG2'   ) TYPMA='SEG2'
        IF (QUESTI.EQ.'EXI_SEG3'   ) TYPMA='SEG3'
        IF (QUESTI.EQ.'EXI_HEXA8'  ) TYPMA='HEXA8'
        IF (QUESTI.EQ.'EXI_HEXA20' ) TYPMA='HEXA20'
        IF (QUESTI.EQ.'EXI_HEXA27' ) TYPMA='HEXA27'
        IF (QUESTI.EQ.'EXI_PENTA6' ) TYPMA='PENTA6'
        IF (QUESTI.EQ.'EXI_PENTA15') TYPMA='PENTA15'
        IF (QUESTI.EQ.'EXI_TETRA4' ) TYPMA='TETRA4'
        IF (QUESTI.EQ.'EXI_TETRA10') TYPMA='TETRA10'
        IF (QUESTI.EQ.'EXI_PYRAM5' ) TYPMA='PYRAM5'
        IF (QUESTI.EQ.'EXI_PYRAM13') TYPMA='PYRAM13'
        IF (QUESTI.EQ.'EXI_POI1'   ) TYPMA='POI1'
        CALL ASSERT(TYPMA.NE.'XXXX')
        CALL JENONU(JEXNOM('&CATA.TM.NOMTM',TYPMA),TYPV)
        CALL ASSERT(TYPV.GT.0)

        REPK = 'NON'
        NBMA = ZI(JDIME-1+3)
        CALL JEVEUO(NOMOB//'.TYPMAIL','L',JTYPMA)
        DO 50,K = 1,NBMA
          IF (ZI(JTYPMA-1+K).EQ.TYPV) GOTO 51
   50   CONTINUE
        GOTO 52
   51   CONTINUE
        REPK='OUI'
   52   CONTINUE

      ELSE
        IERD = 1
      END IF



      REPKZ = REPK
      CALL JEDEMA()
      END
