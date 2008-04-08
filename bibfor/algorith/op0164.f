      SUBROUTINE OP0164 ( IERR )
C      IMPLICIT NONE
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IERR
C TOLE CRP_4
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
      INTEGER      N1, N2, N4, IADRIF, JSCDE, ULISOP
      REAL*8       PARTR, PARTI
      CHARACTER*8  K8B, NOMRES, BASEMO, NUMGEN, INTERF
      CHARACTER*16 TYPRES, NOMCOM, TYPBAS, K16NOM, TYPBIN
      CHARACTER*19 RESU , STOLCI
      CHARACTER*14 NUGENE
      CHARACTER*24 TABRIG, TABFRQ
      CHARACTER*72 TEXTE
      REAL*8 A(3)
      INTEGER*8    LONG1,LONG2,LONG3
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

      CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
      INTERF = ZK24(IADRIF+4) (1:8)
      IF (INTERF.NE.' ') THEN
         CALL BMNBMD(BASEMO,'MODE',NBMODD)
         CALL BMNBMD(BASEMO,'DEFORMEE',NBMODS)
      ELSE
         CALL JEVEUO(BASEMO//'           .UTIL','L',JVAL)
         NBMODD = ZI(JVAL+2)
         NBMODS = ZI(JVAL+3)
      ENDIF

C      NBMODE = NBMODD + NBMODS
      TABRIG = '&&OP0164.RIGM'
      TABFRQ = '&&OP0164.FREQ'
      CALL WKVECT(TABRIG,'V V R',2*NBMODS*NBMODS,JRIG)
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
        DO 1 I2 = 1,NBMODS
        DO 1 I1 = 1,NBMODS
          NSAUT = NFREQ
          IF (I1.EQ.1.AND.I2.EQ.1) NSAUT = IFREQ
          DO 2 I = 1, NSAUT
            READ(IFMIS,'(A72)') TEXTE
    2     CONTINUE
          READ(IFMIS,*) (A(J),J=1,3)
          ZR(JRIG+2*(I2-1)*NBMODS+2*I1-2) = A(2)
          ZR(JRIG+2*(I2-1)*NBMODS+2*I1-1) = -A(3)
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
        NBMODS=LONG2
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
        READ(IFMIS) ((ZR(JRIG+2*(I2-1)*NBMODS+2*I1-2),
     &                ZR(JRIG+2*(I2-1)*NBMODS+2*I1-1),
     &                I1=1,NBMODS),I2=1,NBMODS)
        DO 6 I1 = 1, NBMODS
        DO 6 I2 = 1, NBMODS
          ZR(JRIG+2*(I2-1)*NBMODS+2*I1-1)=
     &   -ZR(JRIG+2*(I2-1)*NBMODS+2*I1-1)
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
      CALL JECREC ( RESU//'.VALM', 'G V C', 'NU', 'DISPERSE',
     &                                            'CONSTANT', 2 )
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
      ZK24(JREFA-1+9) = 'MR'
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
      CALL JECROC ( JEXNUM(RESU//'.VALM',2) )
      CALL JEVEUO ( JEXNUM(RESU//'.VALM',2), 'E', LDBLO2 )
C
C ------ PROJECTION DE LA MATRICE ASSEMBLEE
C
C        BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
C
C
      DO 30 I = 1 , NUEQ
C
         II = I - NBMODD
C
C --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
C
         DO 40 J = 1 , I
           JJ = J - NBMODD
C
C ----------- PRODUIT SCALAIRE VECTASS * MODE
C
           IF (I.LE.NBMODD.OR.J.LE.NBMODD) THEN
             ZC(LDBLO+I*(I-1)/2+J-1) = DCMPLX(0.D0,0.D0)
             ZC(LDBLO2+I*(I-1)/2+J-1) = DCMPLX(0.D0,0.D0)
           ELSE
C
C ----------- STOCKAGE DANS LE .UALF A LA BONNE PLACE (1 BLOC)
C
             PARTR = ZR(JRIG+2*(II-1)*NBMODS+2*JJ-2)
             PARTI = ZR(JRIG+2*(II-1)*NBMODS+2*JJ-1)
             ZC(LDBLO+I*(I-1)/2+J-1) = DCMPLX(PARTR,PARTI)
             PARTR = ZR(JRIG+2*(JJ-1)*NBMODS+2*II-2)
             PARTI = ZR(JRIG+2*(JJ-1)*NBMODS+2*II-1)
             ZC(LDBLO2+I*(I-1)/2+J-1) = DCMPLX(PARTR,PARTI)
           ENDIF
C
 40      CONTINUE
 30   CONTINUE
      CALL JELIBE ( JEXNUM(RESU//'.VALM', 1) )
      CALL JELIBE ( JEXNUM(RESU//'.VALM', 2) )
      CALL JEDETR(TABRIG)
      CALL JEDETR(TABFRQ)
C
      CALL JEDEMA()
      END
