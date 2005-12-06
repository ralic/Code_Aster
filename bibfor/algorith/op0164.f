      SUBROUTINE OP0164 ( IERR )
C      IMPLICIT NONE
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IERR
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/11/2005   AUTEUR ACBHHCD G.DEVESA 
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
      CHARACTER*32   JEXNUM, JEXNOM
C      ---- FIN DES COMMUNS JEVEUX ------------------------------------
C
      INTEGER      IBID, N1, N2, N3, N4, N5, NBVECT, IER, NBMODE, 
     +             IADRIF, LLREFE, NEQ, LLNEQU, LLDESC, ULISOP
      REAL*8       RBID, PARTR, PARTI
      COMPLEX*16   CBID
      CHARACTER*1  TYPMAT
      CHARACTER*8  K8B, NOMRES, BASEMO, MATRAS, NUMGEN, INTERF
      CHARACTER*16 TYPRES, NOMCOM, TYPBAS, MATRI2, K16NOM
      CHARACTER*19 RESU
      CHARACTER*19 NOMNUM, NOMSTO
      CHARACTER*24 MATRIC, TABRIG
      CHARACTER*72 TEXTE
      REAL*8 A(3)
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
      K16NOM = ' '
      IF ( ULISOP ( IFMIS, K16NOM ) .EQ. 0 )  THEN 
        CALL ULOPEN ( IFMIS,' ',' ','NEW','O')
      ENDIF
      CALL IRMIFR(IFMIS,FREQ,IFREQ,NFREQ)
      CALL GETVID ( ' ', 'BASE'          , 1,1,1, BASEMO, N4 )
      CALL GETVID ( ' ', 'NUME_DDL_GENE' , 1,1,1, NUMGEN, N2 )
C
      CALL GETTCO ( BASEMO, TYPBAS )
      TYPMAT= TYPRES(16:16)
      WRITE(6,*) 'TYPMAT = ',TYPMAT
C
      NOMNUM = NUMGEN//'      .NUME'
      NOMSTO = NUMGEN//'      .SLCS'

C==================================================
C
C
C RECUPERATION DU NOMBRE DE MODES REDUIT,
C NB_VECT DONNE PAR NUME_DDL_GENE
      CALL JEVEUO(NOMSTO//'.DESC','L',LLDESC)

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
      NBMODE = NBMODD + NBMODS
      TABRIG = '&&OP0196.RIGM'
      CALL WKVECT(TABRIG,'V V R',2*NBMODS*NBMODS,JRIG)
      REWIND IFMIS
      READ(IFMIS,'(A72)') TEXTE
      IF (TEXTE(1:4).EQ.'XXXX') GOTO 4
      DO 1 I1 = 1,NBMODS
      DO 1 I2 = 1,NBMODS
        NSAUT = NFREQ
        IF (I1.EQ.1.AND.I2.EQ.1) NSAUT = IFREQ
        DO 2 I = 1, NSAUT
          READ(IFMIS,'(A72)') TEXTE
    2   CONTINUE
        READ(IFMIS,*) (A(J),J=1,3)
        ZR(JRIG+2*(I2-1)*NBMODS+2*I1-2) = A(2)
        ZR(JRIG+2*(I2-1)*NBMODS+2*I1-1) = -A(3)
    1 CONTINUE
    4 CONTINUE
C
C ----- RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
C

      CALL JEVEUO ( NOMSTO//'.DESC', 'L', LLDESC )
      NUEQ   = ZI(LLDESC)
      NTBLOC = ZI(LLDESC+1)
      NBLOC  = ZI(LLDESC+2)
      WRITE(6,*) 'NUEQ = ',NUEQ,' NBMODE = ',NBMODE
C
      RESU = ' '
      RESU(1:8) = NOMRES
      CALL JECREC ( RESU//'.VALE', 'G V C', 'NU', 'DISPERSE', 
     &                                            'CONSTANT', NBLOC )
      CALL JEECRA ( RESU//'.VALE', 'LONMAX', NTBLOC, K8B )
      CALL JEECRA ( RESU//'.VALE', 'DOCU', IBID, 'MS' )
C
      CALL WKVECT ( RESU//'.LIME', 'G V K8', 1, IALIME )
      ZK8(IALIME) = '        '
C
      CALL WKVECT ( RESU//'.CONL', 'G V C' , NUEQ, IACONL )
      DO 10 I = 1 , NUEQ
         ZC(IACONL+I-1) = DCMPLX(1.0D0,0.0D0)
 10   CONTINUE
C
      CALL WKVECT ( RESU//'.REFA', 'G V K24', 4, IAREFE )
      CALL JEECRA ( RESU//'.REFA', 'DOCU', IBID, 'SLCS' )
      ZK24(IAREFE)   = BASEMO
      ZK24(IAREFE+1) = NOMNUM
      ZK24(IAREFE+2) = NOMSTO
C
      CALL WKVECT ( RESU//'.DESC', 'G V I', 3, IADESC )
      ZI(IADESC)   = 2
      ZI(IADESC+1) = NUEQ
C   On teste la hauteur maximale des colonnes de la matrice
C   si cette hauteur vaut 1, on suppose que le stockage est diagonal
      IF (ZI(LLDESC+3).EQ.1) THEN
        ZI(IADESC+2) = 1
      ELSE
        ZI(IADESC+2) = 2
      ENDIF  
C
C
C --- RECUPERATION DE LA STRUCTURE DE LA MATR_ASSE_GENE
C
      CALL JEVEUO ( NOMSTO//'.ADIA', 'L', IADIA )
      CALL JEVEUO ( NOMSTO//'.ABLO', 'L', IABLO )
      CALL JEVEUO ( NOMSTO//'.HCOL', 'L', IHCOL )
C
      DO 20 IBLO = 1 , NBLOC
C
         CALL JECROC ( JEXNUM(RESU//'.VALE',IBLO) )
         CALL JEVEUO ( JEXNUM(RESU//'.VALE',IBLO), 'E', LDBLO )
C
C ------ PROJECTION DE LA MATRICE ASSEMBLEE
C
C        BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
C
         N1BLOC = ZI(IABLO+IBLO-1)+1
         N2BLOC = ZI(IABLO+IBLO)
C
         DO 30 I = N1BLOC , N2BLOC
C
            II = I - NBMODD
C
C --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
C
            DO 40 J = (I-ZI(IHCOL+I-1)+1) , I
              JJ = J - NBMODD
C
C ----------- PRODUIT SCALAIRE VECTASS * MODE
C
              IF (I.LE.NBMODD.OR.J.LE.NBMODD) THEN
                ZC(LDBLO+ZI(IADIA+I-1)+J-I-1) = DCMPLX(0.D0,0.D0)
              ELSE
C
C ----------- STOCKAGE DANS LE .VALE A LA BONNE PLACE (1 BLOC)
C
                PARTR = 0.5D0*(ZR(JRIG+2*(II-1)*NBMODS+2*JJ-2)
     &                       + ZR(JRIG+2*(JJ-1)*NBMODS+2*II-2))
                PARTI = 0.5D0*(ZR(JRIG+2*(II-1)*NBMODS+2*JJ-1)
     &                       + ZR(JRIG+2*(JJ-1)*NBMODS+2*II-1))
                ZC(LDBLO+ZI(IADIA+I-1)+J-I-1) = DCMPLX(PARTR,PARTI)

              ENDIF
C
 40      CONTINUE
 30      CONTINUE
         CALL JELIBE ( JEXNUM(RESU//'.VALE', IBLO) )
 20   CONTINUE
      CALL JEDETR(TABRIG)
C
      CALL JEDEMA()
      END
