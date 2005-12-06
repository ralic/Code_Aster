      SUBROUTINE OP0179 ( IERR )
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
C     OPERATEUR LIRE_FORC_MISS
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
      REAL*8       RBID, PARTR, PARTI, COEF, DPI
      COMPLEX*16   CBID
      CHARACTER*1  TYPMAT
      CHARACTER*8  K8B, NOMRES, BASEMO, MATRAS, NUMGEN, INTERF
      CHARACTER*16 TYPRES, NOMCOM, TYPBAS, MATRI2, K16NOM
      CHARACTER*19 RESU
      CHARACTER*19 NOMNUM, NOMSTO
      CHARACTER*24 MATRIC, TABRIG
      CHARACTER*72 TEXTE
      CHARACTER*2  NOMCMP
      CHARACTER*4  NOMCHA
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
      CALL GETVIS(' ','UNITE_RESU_FORC',1,1,1,IFMIS,N1)
      CALL GETVTX (' ','NOM_CMP',1,1,1,NOMCMP,IBID )
      CALL GETVTX (' ','NOM_CHAM',1,1,1,NOMCHA,IBID )
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
      DPI = 8.D0*ATAN2(1.D0,1.D0)

C==================================================
C
C
C RECUPERATION DU NOMBRE DE MODES REDUIT,
C NB_VECT DONNE PAR NUME_DDL_GENE
C      CALL JEVEUO(NOMSTO//'.DESC','L',LLDESC)
C      NBMODE   = ZI(LLDESC)

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
      TABRIG = '&&OP0179.RIGM'
      CALL WKVECT(TABRIG,'V V R',2*NBMODS,JRIG)
      REWIND IFMIS
      READ(IFMIS,'(A72)') TEXTE
      IF (TEXTE(1:4).EQ.'XXXX') GOTO 4
      NSAU0 = 0
      IF (NOMCMP.EQ.'DY') NSAU0 = (NFREQ+1)*NBMODS
      IF (NOMCMP.EQ.'DZ') NSAU0 = 2*(NFREQ+1)*NBMODS
      DO 1 I1 = 1,NBMODS
        NSAUT = NFREQ
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
    1 CONTINUE
    4 CONTINUE
C
C ----- RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
C

      CALL JEVEUO ( NOMSTO//'.DESC', 'L', LLDESC )
C      NUEQ   = ZI(LLDESC)
C      NTBLOC = ZI(LLDESC+1)
C      NBLOC  = ZI(LLDESC+2)
C
      RESU = ' '
      RESU(1:8) = NOMRES
C
C --- CREATION DE L OBJET VECT_GENE RESULTAT
C
      CALL WKVECT(RESU//'.VALE','G V C',NBMODE,IAVALE)
      CALL WKVECT(RESU//'.REFE','G V K24',2,IAREFE)
      CALL WKVECT(RESU//'.DESC','G V I',3,IADESC)
C
C --- REMPLISSAGE DU .REFE ET .VALE
C
      ZK24(IAREFE) = BASEMO
      ZK24(IAREFE+1) = NOMNUM
      ZI(IADESC) = 1
      ZI(IADESC+1) = NBMODE
C   On teste la hauteur maximale des colonnes de la matrice
C   si cette hauteur vaut 1, on suppose que le stockage est diagonal
      IF (ZI(LLDESC+3).EQ.1) THEN
        ZI(IADESC+2) = 1
      ELSE
        ZI(IADESC+2) = 2
      ENDIF  

C
      DO 20 I = 1 , NBMODE
C
         II = I - NBMODD
C
C --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
C
         IF (I.LE.NBMODD) THEN
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
