      SUBROUTINE OP0164 ( IERR )
C      IMPLICIT NONE
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IERR
C TOLE CRP_4
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/06/2010   AUTEUR DEVESA G.DEVESA 
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
C-----------------------------------------------------------------------
C
C     OPERATEUR LIRE_IMPE_MISS
C
C-----------------------------------------------------------------------
C      ---- DEBUT DES COMMUNS JEVEUX ----------------------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8   ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32   JEXNUM
C      ---- FIN DES COMMUNS JEVEUX ------------------------------------
C
      INTEGER      N1, N2, N4, IADRIF, JSCDE, ULISOP,IER
      REAL*8       PARTR, PARTI
      CHARACTER*8  K8B, NOMRES, BASEMO, NUMGEN, INTERF
      CHARACTER*16 TYPRES,NOMCOM,TYPBAS,K16NOM,TYPBIN,TISSF,TSYM
      CHARACTER*19 RESU , STOLCI
      CHARACTER*14 NUGENE
      CHARACTER*24 TABRIG, TABFRQ
      CHARACTER*72 TEXTE
      REAL*8 A(3)
      INTEGER*8    LONG1,LONG2,LONG3
      LOGICAL      LISSF, LSYM
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES(NOMRES,TYPRES,NOMCOM)
C
C --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
C
      CALL GETVIS(' ','UNITE_RESU_IMPE',1,1,1,IFMIS,N1)
      CALL GETVR8(' ','FREQ_EXTR',1,1,1,FREQ,NFR)
      CALL GETVID ( ' ', 'BASE'          , 1,1,1, BASEMO, N4 )
      CALL GETVID ( ' ', 'NUME_DDL_GENE' , 1,1,1, NUMGEN, N2 )
      CALL GETVTX ( ' ', 'TYPE' , 1,1,1, TYPBIN, N2 )
      LISSF = .FALSE.
      CALL GETVTX ( ' ', 'ISSF' , 1,1,1, TISSF, N2 )
      IF (TISSF(1:3).EQ.'OUI') LISSF = .TRUE.
      LSYM = .FALSE.
      CALL GETVTX ( ' ', 'SYME' , 1,1,1, TSYM, N2 )
      IF (TSYM(1:3).EQ.'OUI') LSYM = .TRUE.
C
      CALL GETTCO ( BASEMO, TYPBAS )
C
      NUGENE = NUMGEN
      STOLCI = NUMGEN//'      .SLCS'

C==================================================
C
C
C RECUPERATION DU NOMBRE DE MODES REDUIT,
C NB_VECT DONNE PAR NUME_DDL_GENE
      CALL JEVEUO(STOLCI//'.SCDE','L',JSCDE)
      
      CALL DISMOI('F','NB_MODES_DYN',BASEMO,'RESULTAT',
     &                      NBMODD,K8B,IER)
      CALL DISMOI('F','NB_MODES_STA',BASEMO,'RESULTAT',
     &                      NBMODS,K8B,IER)
      IF (LISSF) THEN
        NBMODE = NBMODD + NBMODS
      ELSE
        NBMODE = NBMODS
      ENDIF
C
      TABRIG = '&&OP0164.RIGM'
      TABFRQ = '&&OP0164.FREQ'
      CALL WKVECT(TABRIG,'V V R',2*NBMODE*NBMODE,JRIG)
      IF (TYPBIN.NE.'BINAIRE') THEN
        K16NOM = ' '
        IF ( ULISOP ( IFMIS, K16NOM ) .EQ. 0 )  THEN
          CALL ULOPEN ( IFMIS,' ',' ','NEW','O')
        ENDIF
        CALL IRMIFR(IFMIS,FREQ,IFREQ,NFREQ)
        CALL WKVECT(TABFRQ,'V V R',NFREQ,JFRQ)
        REWIND IFMIS
        READ(IFMIS,'(A72)') TEXTE
        IF (TEXTE(1:4).EQ.'XXXX') GOTO 4
        DO 1 I2 = 1,NBMODE
        DO 1 I1 = 1,NBMODE
          NSAUT = NFREQ
          IF (I1.EQ.1.AND.I2.EQ.1) NSAUT = IFREQ
          DO 2 I = 1, NSAUT
            READ(IFMIS,'(A72)') TEXTE
    2     CONTINUE
          READ(IFMIS,*) (A(J),J=1,3)
          ZR(JRIG+2*(I2-1)*NBMODE+2*I1-2) = A(2)
          ZR(JRIG+2*(I2-1)*NBMODE+2*I1-1) = -A(3)
    1   CONTINUE
    4   CONTINUE
      ELSE
        REWIND IFMIS
C
C   Lecture d'entiers INTEGER*8 en binaire venant de MISS3D
C   On convertit ensuite en INTEGER (*4 sur machine 32 bits, sinon *8).
C   Les reels ne posent pas de probleme : ce sont toujours des REAL*8
C
        READ(IFMIS) LONG1,LONG2,LONG3
        NFREQ=LONG1
        NBMODE=LONG2
        N1=LONG3
        CALL WKVECT(TABFRQ,'V V R',NFREQ,JFRQ)
        READ(IFMIS) (ZR(JFRQ+IFR-1),IFR=1,NFREQ)
        DO 3 I = 1, NFREQ
          A(1) = ZR(JFRQ+I-1)+1.D-6
          IF (FREQ.LE.A(1)) THEN
            IFREQ = I
            GOTO 7
        ENDIF
    3   CONTINUE
        IFREQ = NFREQ
    7   CONTINUE
        DO 5 I = 1, IFREQ-1
          READ(IFMIS) A(1)
    5   CONTINUE
        READ(IFMIS) ((ZR(JRIG+2*(I2-1)*NBMODE+2*I1-2),
     &                ZR(JRIG+2*(I2-1)*NBMODE+2*I1-1),
     &                I1=1,NBMODE),I2=1,NBMODE)
        DO 6 I1 = 1, NBMODE
        DO 6 I2 = 1, NBMODE
          ZR(JRIG+2*(I2-1)*NBMODE+2*I1-1)=
     &   -ZR(JRIG+2*(I2-1)*NBMODE+2*I1-1)
    6   CONTINUE
      ENDIF
C
C ----- RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
C

      CALL JEVEUO ( STOLCI//'.SCDE', 'L', JSCDE )
      NUEQ   = ZI(JSCDE-1+1)
C      NTBLOC = ZI(JSCDE-1+2)
C      NBLOC  = ZI(JSCDE-1+3)
      NTERM = NUEQ*(NUEQ+1)/2
C
      RESU = ' '
      RESU(1:8) = NOMRES
      IF (LSYM) THEN
        CALL JECREC ( RESU//'.VALM', 'G V C', 'NU', 'DISPERSE',
     &                                              'CONSTANT', 1 )
      ELSE
        CALL JECREC ( RESU//'.VALM', 'G V C', 'NU', 'DISPERSE',
     &                                              'CONSTANT', 2 )
      ENDIF
      CALL JEECRA ( RESU//'.VALM', 'LONMAX', NTERM, K8B )
C
      CALL WKVECT ( RESU//'.LIME', 'G V K24', 1, IALIME )
      ZK24(IALIME) = '                        '
C
      CALL WKVECT ( RESU//'.CONL', 'G V C' , NUEQ, IACONL )
      DO 10 I = 1 , NUEQ
         ZC(IACONL+I-1) = DCMPLX(1.0D0,0.0D0)
 10   CONTINUE
C
      CALL WKVECT ( RESU//'.REFA', 'G V K24',11, JREFA )
      ZK24(JREFA-1+11)='MPI_COMPLET'
      ZK24(JREFA-1+1)   = BASEMO
      ZK24(JREFA-1+2) = NUGENE
      IF (LSYM) THEN
        ZK24(JREFA-1+9) = 'MS'
      ELSE
        ZK24(JREFA-1+9) = 'MR'
      ENDIF
      ZK24(JREFA-1+10) = 'GENE'
C
      CALL WKVECT ( RESU//'.DESC', 'G V I', 3, IADESC )
      ZI(IADESC)   = 2
      ZI(IADESC+1) = NUEQ
C   ON TESTE LA HAUTEUR MAXIMALE DES COLONNES DE LA MATRICE
C   SI CETTE HAUTEUR VAUT 1, ON SUPPOSE QUE LE STOCKAGE EST DIAGONAL
      IF (ZI(JSCDE-1+4).EQ.1) THEN
        ZI(IADESC+2) = 1
      ELSE
        ZI(IADESC+2) = 2
      ENDIF
C
C
C --- RECUPERATION DE LA STRUCTURE DE LA MATR_ASSE_GENE
C
C
C
      CALL JECROC ( JEXNUM(RESU//'.VALM',1) )
      CALL JEVEUO ( JEXNUM(RESU//'.VALM',1), 'E', LDBLO )
      IF (.NOT.LSYM) THEN
        CALL JECROC ( JEXNUM(RESU//'.VALM',2) )
        CALL JEVEUO ( JEXNUM(RESU//'.VALM',2), 'E', LDBLO2 )
      ENDIF
C
C ------ PROJECTION DE LA MATRICE ASSEMBLEE
C
C        BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
C
C
      DO 30 I = 1 , NUEQ
C
         II = I - NBMODD
         IF (LISSF.AND.I.LE.NBMODD) II = I+NBMODS
C
C --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
C
         DO 40 J = 1 , I
           JJ = J - NBMODD
           IF (LISSF.AND.J.LE.NBMODD) JJ = J+NBMODS
C
C ----------- PRODUIT SCALAIRE VECTASS * MODE
C
           IF (.NOT.LISSF.AND.(I.LE.NBMODD.OR.J.LE.NBMODD)) THEN
             ZC(LDBLO+I*(I-1)/2+J-1) = DCMPLX(0.D0,0.D0)
             IF (.NOT.LSYM) THEN
               ZC(LDBLO2+I*(I-1)/2+J-1) = DCMPLX(0.D0,0.D0)
             ENDIF
           ELSE
C
C ----------- STOCKAGE DANS LE .UALF A LA BONNE PLACE (1 BLOC)
C
             PARTR = ZR(JRIG+2*(II-1)*NBMODE+2*JJ-2)
             PARTI = ZR(JRIG+2*(II-1)*NBMODE+2*JJ-1)
             IF (LSYM) THEN
               PARTR = 0.5D0*(ZR(JRIG+2*(JJ-1)*NBMODE+2*II-2)
     &                      + PARTR)
               PARTI = 0.5D0*(ZR(JRIG+2*(JJ-1)*NBMODE+2*II-1)
     &                      + PARTI)
             ENDIF
             ZC(LDBLO+I*(I-1)/2+J-1) = DCMPLX(PARTR,PARTI)
             IF (.NOT.LSYM) THEN
               PARTR = ZR(JRIG+2*(JJ-1)*NBMODE+2*II-2)
               PARTI = ZR(JRIG+2*(JJ-1)*NBMODE+2*II-1)
               ZC(LDBLO2+I*(I-1)/2+J-1) = DCMPLX(PARTR,PARTI)
             ENDIF
           ENDIF
C
 40      CONTINUE
 30   CONTINUE
      CALL JELIBE ( JEXNUM(RESU//'.VALM', 1) )
      IF (.NOT.LSYM) CALL JELIBE ( JEXNUM(RESU//'.VALM', 2) )
      CALL JEDETR(TABRIG)
      CALL JEDETR(TABFRQ)
C
      CALL JEDEMA()
      END
