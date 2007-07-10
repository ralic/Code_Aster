      SUBROUTINE BOITIM(UNIT  ,NOMBOZ,NGRMAZ)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*(*)  NOMBOZ, NGRMAZ
      INTEGER       UNIT
C
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN ENSEMBLE DE MAILLES
C
C ROUTINE D'AFFICHAGE
C
C ----------------------------------------------------------------------
C
C
C IN  UNIT   : NUMERO LOGIQUE DU FICHIER D'ECRITURE
C IN  NOMBOZ : NOM DE LA SD BOITE
C IN  NGRMAZ : NOM DE LA LISTE DES MAILLES ASSOCIEES AUX BOITES
C
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*16  NOMBOI
      CHARACTER*19  NGRMA
      INTEGER      JDIME,JMNMX,JBPAN,JBSOM,JMMGL,JBOIH
      INTEGER      NDIME,NMA,NPAN,NSOM
      INTEGER      IMA,IPAN,ISOM
      INTEGER      INDMA,IRET,NUMMAI,JGRMA
      REAL*8       A1,B1,C1,D1,E1
      REAL*8       X1,Y1,Z1
      LOGICAL      DEBUG
      CHARACTER*8  NOMO,NOMA,NOMMAI
      INTEGER      IOCC,JNOMA
C
C ----------------------------------------------------------------------
C
      DEBUG = .TRUE.

      NOMBOI=NOMBOZ
      NGRMA=NGRMAZ
C
C --- RECUPERATION MAILLAGE
C
      CALL GETVID(' ','MODELE',0,1,1,NOMO,IOCC)
      IF (IOCC.EQ.0) THEN
        WRITE(UNIT,*) '<BOITES  > AFFICHAGE IMPOSSIBLE (VOIR BOITIM) '
        GOTO 999
      ENDIF
      CALL JEVEUO(NOMO(1:8)//'.MODELE    .LGRF','L',JNOMA)
      NOMA = ZK8(JNOMA)
C
      CALL JEVEUO(NGRMA,'L',JGRMA)
C
      CALL JEEXIN(NOMBOI//'.DIME'  ,IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<BOITES  > SD BOITE INCORRECTE: <',
     &                NOMBOI//'.DIME','> N''EXISTE PAS'
      ELSE
        CALL JEVEUO(NOMBOI//'.DIME'  ,'L',JDIME)
        NDIME = ZI(JDIME)
        NMA   = ZI(JDIME+1)
        IF (NDIME.GT.3) THEN
          WRITE(UNIT,*) '<BOITES  > SD BOITE INCORRECTE (DIMENSION) '
          GOTO 999
        ENDIF
        IF (NMA.LT.0) THEN
          WRITE(UNIT,*) '<BOITES  > SD BOITE INCORRECTE (NBRE) '
          GOTO 999
        ENDIF
        WRITE(UNIT,*) '<BOITES  > ... NOMBRE DE BOITES  : ',NMA
        WRITE(UNIT,*) '<BOITES  > ... DIMENSION ESPACE  : ',NDIME
      ENDIF
C
      CALL JEEXIN(NOMBOI//'.MMGLOB',IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<BOITES  > SD BOITE INCORRECTE: <',
     &                NOMBOI//'.MMGLOB','> N''EXISTE PAS'
      ELSE
        CALL JEVEUO(NOMBOI//'.MMGLOB','L',JMMGL)
        WRITE(UNIT,*) '<BOITES  > ... BOITE ENGLOBANTE DE LA ZONE...'
        WRITE(UNIT,*) '<BOITES  > ...... X = (',ZR(JMMGL),',',
     &                                      ZR(JMMGL+1),')'
        WRITE(UNIT,*) '<BOITES  > ...... Y = (',ZR(JMMGL+2),',',
     &                                      ZR(JMMGL+3),')'
        IF (NDIME.EQ.3) THEN
          WRITE(UNIT,*) '<BOITES  > ...... Z = (',ZR(JMMGL+4),',',
     &                                      ZR(JMMGL+5),')'
        ENDIF
      ENDIF
C
      IF (.NOT.DEBUG) GOTO 999
C
      DO 10 IMA = 1 , NMA
C
        NPAN = ZI(JDIME+2+2*(IMA-1)+2) -
     &         ZI(JDIME+2+2*(IMA-1))
        NSOM = ZI(JDIME+2+2*(IMA-1)+3) -
     &         ZI(JDIME+2+2*(IMA-1)+1)
C
        NUMMAI = ZI(JGRMA+IMA-1)
        CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUMMAI),NOMMAI)
C
        WRITE(UNIT,*) '<BOITES  > ... BOITE NUMERO (',IMA,
     &                ' ) - MAILLE ',NOMMAI
C
        CALL JEEXIN(NOMBOI//'.MINMAX',IRET)
        INDMA = NDIME*2*(IMA-1)
        IF (IRET.EQ.0) THEN
          WRITE(UNIT,*) '<BOITES  > SD BOITE INCORRECTE: <',
     &                NOMBOI//'.MINMAX','> N''EXISTE PAS'
        ELSE
          CALL JEVEUO(NOMBOI//'.MINMAX','L',JMNMX)
          WRITE(UNIT,*) '<BOITES  > ...... MINMAX '
          WRITE(UNIT,*) '<BOITES  > ......... X = (',
     &                 ZR(JMNMX+INDMA  ),',',
     &                 ZR(JMNMX+INDMA+1),')'
          WRITE(UNIT,*) '<BOITES  > ......... Y = (',
     &                 ZR(JMNMX+INDMA+2),',',
     &                 ZR(JMNMX+INDMA+3),')'
          IF (NDIME.EQ.3) THEN
            WRITE(UNIT,*) '<BOITES  > ......... Z = (',
     &                 ZR(JMNMX+INDMA+4),',',
     &                 ZR(JMNMX+INDMA+5),')'
          ENDIF
        ENDIF
C
        CALL JEEXIN(NOMBOI//'.H'     ,IRET)
        IF (IRET.EQ.0) THEN
          WRITE(UNIT,*) '<BOITES  > SD BOITE INCORRECTE: <',
     &                NOMBOI//'.H','> N''EXISTE PAS'
        ELSE
          CALL JEVEUO(NOMBOI//'.H'     ,'L',JBOIH)
          WRITE(UNIT,*) '<BOITES  > ...... LONGUEUR CARACTERISTIQUE :',
     &                 ZR(JBOIH+IMA-1)
        ENDIF
C
        CALL JEEXIN(NOMBOI//'.PAN'   ,IRET)
        IF (IRET.EQ.0) THEN
          WRITE(UNIT,*) '<BOITES  > SD BOITE INCORRECTE: <',
     &                NOMBOI//'.PAN','> N''EXISTE PAS'
        ELSE
          CALL JEVEUO(NOMBOI//'.PAN'   ,'L',JBPAN)

          WRITE(UNIT,*) '<BOITES  > ...... NOMBRE DE PANS : ',NPAN
C
          DO 20 IPAN = 1 , NPAN
            WRITE(UNIT,*) '<BOITES  > ......... PAN (',IPAN,' )'
            IF (NDIME.EQ.2) THEN
              A1 = ZR(JBPAN+4*(IPAN-1))
              B1 = ZR(JBPAN+4*(IPAN-1)+1)
              D1 = ZR(JBPAN+4*(IPAN-1)+2)
              E1 = ZR(JBPAN+4*(IPAN-1)+3)
              WRITE(UNIT,*) '<BOITES  > ......... * ENGLOBANT: ',
     &                      A1,B1,D1
              WRITE(UNIT,*) '<BOITES  > ......... * INSCRIT  : ',
     &                      A1,B1,E1
            ELSEIF (NDIME.EQ.3) THEN
              A1 = ZR(JBPAN+5*(IPAN-1))
              B1 = ZR(JBPAN+5*(IPAN-1)+1)
              C1 = ZR(JBPAN+5*(IPAN-1)+2)
              D1 = ZR(JBPAN+5*(IPAN-1)+3)
              E1 = ZR(JBPAN+5*(IPAN-1)+4)
              WRITE(UNIT,*) '<BOITES  > ......... * ENGLOBANT: ',
     &                      A1,B1,C1,D1
              WRITE(UNIT,*) '<BOITES  > ......... * INSCRIT  : ',
     &                      A1,B1,C1,E1
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
  20      CONTINUE
        ENDIF
C
        CALL JEEXIN(NOMBOI//'.SOMMET',IRET)
        IF (IRET.EQ.0) THEN
          WRITE(UNIT,*) '<BOITES  > SD BOITE INCORRECTE: <',
     &           NOMBOI//'.SOMMET','> N''EXISTE PAS'
        ELSE
          CALL JEVEUO(NOMBOI//'.SOMMET','L',JBSOM)

          WRITE(UNIT,*) '<BOITES  > ...... NOMBRE DE SOMMETS : ',
     &                   NSOM
          DO 30 ISOM = 1 , NSOM
            WRITE(UNIT,*) '<BOITES  > ......... SOMMET (',ISOM,' )'
            IF (NDIME.EQ.2) THEN
              X1 = ZR(JBSOM+2*(ISOM-1))
              Y1 = ZR(JBSOM+2*(ISOM-1)+1)
              WRITE(UNIT,*) '<BOITES  > ......... * COORD : (',X1,',',
     &                      Y1,')'
            ELSEIF (NDIME.EQ.3) THEN
              X1 = ZR(JBSOM+3*(ISOM-1))
              Y1 = ZR(JBSOM+3*(ISOM-1)+1)
              Z1 = ZR(JBSOM+3*(ISOM-1)+2)
              WRITE(UNIT,*) '<BOITES  > ......... * COORD : (',X1,',',
     &                      Y1,',',Z1,')'
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
  30      CONTINUE
        ENDIF
 10   CONTINUE

 999  CONTINUE

      END
