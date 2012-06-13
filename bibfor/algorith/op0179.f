      SUBROUTINE OP0179 ()
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C-----------------------------------------------------------------------
C
C     OPERATEUR LIRE_FORC_MISS
C
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER      IBID, N1, N2, N4, NBMODE, JSCDE, ULISOP
      REAL*8       PARTR, PARTI, COEF, DPI
      CHARACTER*8  NOMRES, BASEMO, NUMGEN, K8BID
      CHARACTER*16 TYPRES, NOMCOM, TYPBAS, K16NOM, TISSF
      CHARACTER*19 RESU
      CHARACTER*19 NOMNUM, NOMSTO
      CHARACTER*24 TABRIG, TABRI2
      CHARACTER*72 TEXTE
      CHARACTER*255 FICHI
      CHARACTER*2  NOMCMP
      CHARACTER*4  NOMCHA
      REAL*8       A(3), A2(3)
      LOGICAL      LISSF
      INTEGER      IARG
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES(NOMRES,TYPRES,NOMCOM)
C
C --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
C
      CALL GETVIS(' ','UNITE_RESU_FORC',1,IARG,1,IFMIS,N1)
      CALL GETVTX (' ','NOM_CMP',1,IARG,1,NOMCMP,NC )
      IF (NC.EQ.0) THEN
        CALL GETVIS(' ','NUME_CHAR',1,IARG,1,IC,N1)
      ENDIF
      CALL GETVTX (' ','NOM_CHAM',1,IARG,1,NOMCHA,IBID )
      CALL GETVR8(' ','FREQ_EXTR',1,IARG,1,FREQ,NFR)
      LISSF = .FALSE.
      CALL GETVTX ( ' ', 'ISSF' , 1,IARG,1, TISSF, N2 )
      IF (TISSF(1:3).EQ.'OUI') LISSF = .TRUE.
      CALL GETVTX (' ','NOM_RESU_FORC',1,IARG,1,FICHI,NF )
      IF ( NF .EQ. 0 ) THEN
        K16NOM = ' '
        IF ( ULISOP ( IFMIS, K16NOM ) .EQ. 0 )  THEN
          CALL ULOPEN ( IFMIS,' ',' ','NEW','O')
        ENDIF
      ELSE
        CALL ULOPEN ( IFMIS, FICHI, ' ', 'NEW', 'O' )
      ENDIF
      CALL IRMIFR(IFMIS,FREQ,IFREQ,NFREQ,ICF)
C      WRITE(6,*) 'FREQ= ',FREQ,' IFREQ= ',IFREQ,' IF= ',ICF
      CALL GETVID ( ' ', 'BASE'          , 1,IARG,1, BASEMO, N4 )
      CALL GETVID ( ' ', 'NUME_DDL_GENE' , 1,IARG,1, NUMGEN, N2 )
C
      CALL GETTCO ( BASEMO, TYPBAS )
C
      NOMNUM = NUMGEN//'      .NUME'
      NOMSTO = NUMGEN//'      .SLCS'
      DPI = 8.D0*ATAN2(1.D0,1.D0)

C==================================================
C
C
C RECUPERATION DU NOMBRE DE MODES REDUIT,
C NB_VECT DONNE PAR NUME_DDL_GENE
C      NBMODE   = ZI(JSCDE-1+1)
C
      CALL DISMOI('F','NB_MODES_DYN',BASEMO,'RESULTAT',
     &                      NBMODD,K8BID,IER)
      CALL DISMOI('F','NB_MODES_STA',BASEMO,'RESULTAT',
     &                      NBMODS,K8BID,IER)

      IF (LISSF) THEN
        NBMODE = NBMODD + NBMODS
      ELSE
        NBMODE = NBMODS
      ENDIF
      TABRIG = '&&OP0179.RIGM'
      TABRI2 = '&&OP0179.RIG2'
      CALL WKVECT(TABRIG,'V V R',2*NBMODE,JRIG)
      CALL WKVECT(TABRI2,'V V R',2*NBMODE,JRI2)
      REWIND IFMIS
      READ(IFMIS,'(A72)') TEXTE
      IF (TEXTE(1:4).EQ.'XXXX') GOTO 4
      NSAU0 = 0
      IF (NC.NE.0) THEN
        IF (NOMCMP.EQ.'DY') NSAU0 = (NFREQ+1)*NBMODE
        IF (NOMCMP.EQ.'DZ') NSAU0 = 2*(NFREQ+1)*NBMODE
      ELSE
        NSAU0 = (IC-1)*(NFREQ+1)*NBMODE
      ENDIF
      DO 1 I1 = 1,NBMODE
        NSAUT = NFREQ
        IF (ICF.EQ.1) NSAUT = NFREQ-1
        IF (I1.EQ.1) NSAUT = IFREQ + NSAU0
        DO 2 I = 1, NSAUT
          READ(IFMIS,'(A72)') TEXTE
    2   CONTINUE
        READ(IFMIS,*) (A(J),J=1,3)
        IF (NOMCHA.EQ.'VITE') THEN
          COEF = -1.D0/(DPI*A(1))
          ZR(JRIG+2*I1-2) = A(3)*COEF
          ZR(JRIG+2*I1-1) = A(2)*COEF
        ELSE
          COEF = 1.D0
          IF (NOMCHA.EQ.'ACCE') COEF = -1.D0/(DPI*A(1))**2
          ZR(JRIG+2*I1-2) = A(2)*COEF
          ZR(JRIG+2*I1-1) = -A(3)*COEF
        ENDIF
        IF (ICF.EQ.1) THEN
          READ(IFMIS,*) (A2(J),J=1,3)
          IF (NOMCHA.EQ.'VITE') THEN
            COEF = -1.D0/(DPI*FREQ)
            ZR(JRIG+2*I1-2)=COEF*
     &                     (A(3)+(FREQ-A(1))/(A2(1)-A(1))*(A2(3)-A(3)))
            ZR(JRIG+2*I1-1)=COEF*
     &                     (A(2)+(FREQ-A(1))/(A2(1)-A(1))*(A2(2)-A(2)))
          ELSE
            COEF = 1.D0
            IF (NOMCHA.EQ.'ACCE') COEF = -1.D0/(DPI*FREQ)**2
            ZR(JRIG+2*I1-2)=COEF*
     &                     (A(2)+(FREQ-A(1))/(A2(1)-A(1))*(A2(2)-A(2)))
            ZR(JRIG+2*I1-1)=-COEF*
     &                     (A(3)+(FREQ-A(1))/(A2(1)-A(1))*(A2(3)-A(3)))
          ENDIF
        ENDIF
        IF (ICF.EQ.2) THEN
          READ(IFMIS,*) (A2(J),J=1,3)
          IF (NOMCHA.EQ.'VITE') THEN
            COEF = -1.D0/(DPI*A2(1))
            ZR(JRI2+2*I1-2) = A2(3)*COEF
            ZR(JRI2+2*I1-1) = A2(2)*COEF
          ELSE
            COEF = 1.D0
            IF (NOMCHA.EQ.'ACCE') COEF = -1.D0/(DPI*A2(1))**2
            ZR(JRI2+2*I1-2) = A2(2)*COEF
            ZR(JRI2+2*I1-1) = -A2(3)*COEF
          ENDIF
          ZR(JRIG+2*I1-2) = ZR(JRIG+2*I1-2) + (FREQ-A(1))/(A2(1)-A(1))
     &                   * (ZR(JRI2+2*I1-2)-ZR(JRIG+2*I1-2))
          ZR(JRIG+2*I1-1) = ZR(JRIG+2*I1-1) + (FREQ-A(1))/(A2(1)-A(1))
     &                   * (ZR(JRI2+2*I1-1)-ZR(JRIG+2*I1-1))
        ENDIF
    1 CONTINUE
    4 CONTINUE
C
C ----- RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
C

      CALL JEVEUO ( NOMSTO//'.SCDE', 'L', JSCDE )
C
      RESU = ' '
      RESU(1:8) = NOMRES
C
C --- CREATION DE L OBJET VECT_GENE RESULTAT
C
      NBMODT = NBMODD + NBMODS
      CALL WKVECT(RESU//'.VALE','G V C',NBMODT,IAVALE)
      CALL WKVECT(RESU//'.REFE','G V K24',2,IAREFE)
      CALL WKVECT(RESU//'.DESC','G V I',3,IADESC)
      CALL JEECRA(RESU//'.DESC','DOCU',0,'VGEN')
C
C --- REMPLISSAGE DU .REFE ET .VALE
C
      ZK24(IAREFE) = BASEMO
      ZK24(IAREFE+1) = NOMNUM
      ZI(IADESC) = 1
      ZI(IADESC+1) = NBMODT
C   ON TESTE LA HAUTEUR MAXIMALE DES COLONNES DE LA MATRICE
C   SI CETTE HAUTEUR VAUT 1, ON SUPPOSE QUE LE STOCKAGE EST DIAGONAL
      IF (ZI(JSCDE-1+4).EQ.1) THEN
        ZI(IADESC+2) = 1
      ELSE
        ZI(IADESC+2) = 2
      ENDIF

C
      DO 20 I = 1 , NBMODT
C
        II = I - NBMODD
        IF (LISSF.AND.I.LE.NBMODD) II = I+NBMODS
C
C --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
C
        IF (.NOT.LISSF.AND.I.LE.NBMODD) THEN
          ZC(IAVALE+I-1) = DCMPLX(0.D0,0.D0)
        ELSE
C
C ----------- STOCKAGE DANS LE .VALE A LA BONNE PLACE
C
          PARTR = ZR(JRIG+2*II-2)
          PARTI = ZR(JRIG+2*II-1)
          ZC(IAVALE+I-1) = DCMPLX(PARTR,PARTI)
          
        ENDIF
 20   CONTINUE

      CALL JEDETR(TABRIG)
C
      CALL JEDEMA()
      END
