      SUBROUTINE RESGRA(SOL,MAT,SMBR,VCINE,MATF,BASE,IREP,
     &                  NITER,EPSI,CRITER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
      IMPLICIT NONE
      CHARACTER*(*) SOL,MAT,SMBR,VCINE,MATF,BASE
      INTEGER NITER,IREP
      REAL*8 EPSI
      CHARACTER*24 CRITER
C     ------------------------------------------------------------------
C     ROUTINE DE HAUT NIVEAU DE RESOLUTION PAR UNE METHODE DE GRADIENT
C     CONJUGUE (GCPC) POUR UNE MATRICE STOCKEE 'MORSE'
C     ------------------------------------------------------------------
C OUT SOL    : K* : NOM DU CHAM_NO SOLUTION
C IN  MAT    : K* : NOM DE L'OBJET MATRICE DE S.D. MATR_ASSE
C IN  SMBR   : K* : NOM DU CHAM_NO SECOND MEMBRE
C IN  MATF   : K* : NOM D'UNE MATRICE DE PRECONDITIONNEMENT
C IN  BASE   : K* : NOM DE LA BASE OU ON VEUT CREER SOL
C IN  IREP   : I     : TYPE DE L'INITIALISATION DES ALGO
C                    = 0   INITIALISATION PAR 0
C                    = 1   REPRISE INITIALISATION PAR XSOL
C IN  NITER  : I  : NOMBRE MAXIMUM D'ITERATIONS
C IN  EPSI   : R  : PARAMETRE D'ERREUR
C----------------------------------------------------------------------
C     COMMUNS JEVEUX
C----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*19 KSTOC,KSTOCF
      CHARACTER*19 B19,VCIN19,MATAS,MATFAC,X19,CBID,K19
      CHARACTER*4 PREC,ETAMAT
      CHARACTER*16 CH16
      CHARACTER*24 KVALE
      INTEGER IFM,NIV,IDB,IRET,LVCINE,JREFA,IDIN,IDIP,LDESC,NEQ,NBLC
      INTEGER IBID,IDAC,IDINPC,IDIPPC,IDACPC,IDW1,IDW2,IDW3,IDW4,IDX
      INTEGER ICRE

C----------------------------------------------------------------------
C     DEBUT
      CALL JEMARQ()

      CALL INFNIV(IFM,NIV)

      MATAS = MAT
      MATFAC = MATF
      X19 = SOL
      K19 = SMBR
      B19 = '&&RESGRA.SCMB'


C     1- DUPPLICATION DU SECOND MEMBRE :
C     -----------------------------------
      CALL COPISD('CHAMP_GD','V',K19,B19)
      CALL JEVEUO(B19//'.VALE','E',IDB)


C     2- QUELQUES VERIFICATIONS :
C     -----------------------------


C     3- SI CHARGE CINEMATIQUE :
C     -----------------------------
      IF (VCINE.NE.' ') THEN
        VCIN19 = VCINE
        CALL JEEXIN(VCIN19//'.VALE',IRET)
        IF (IRET.EQ.0) CALL UTMESS('F','RESGRA',
     &                             'LE CHAM_NO : '//VCIN19//
     &                             ' N"EXISTE PAS')
        CALL JEVEUO(VCIN19//'.VALE','L',LVCINE)
        CALL CSMBG1(MATAS,ZR(IDB),ZR(LVCINE))
      END IF


C     4- PRECONDITIONNEMENT DES "LAGR" DE LA MATRICE
C     ------------------------------------------------
      CALL MVCON1(MATAS,0,'R',ZR(IDB),1)


C     5- RECUPERATION DE LA MATRICE ASSEMBLEE :
C     ------------------------------------------------
      CALL JEEXIN(MATAS//'.REFA',IRET)
      IF (IRET.EQ.0) CALL UTMESS('F','RESGRA',
     &  'PAS DE MATRICE ASSEMBLEE :'//MATAS//' ON S''ARRETE')

      CALL JELIRA(MATAS//'.REFA','DOCU',IBID,ETAMAT)
      IF (ETAMAT.NE.'ASSE')
     +      CALL UTMESS('F','OP0084','  PAS DE RESOLUTION '//
     +       'CAR LA MATRICE ASSEMBLEE '//MATAS//
     +       ' EST  FACTORISEE.' )

      CALL JEVEUO(MATAS//'.REFA','L',JREFA)
      KSTOC = ZK24(JREFA-1+2)(1:14)//'.SMOS'
      CALL JEEXIN(KSTOC//'.ADIA',IRET)
      IF (IRET.EQ.0) CALL UTMESS('F','RESGRA',
     &  'LA MATR_ASSE '//MATAS//' N"EST '//
     &              'PAS STOCKEE "MORSE"')
      CALL JEVEUO(KSTOC//'.ADIA','L',IDIN)
      CALL JEVEUO(KSTOC//'.HCOL','L',IDIP)
      CALL JEVEUO(KSTOC//'.DESC','L',LDESC)
      NEQ = ZI(LDESC)
      IF (NITER.EQ.0) NITER = NEQ/2
      NBLC = ZI(LDESC+2)
        IF (NBLC.NE.1)  CALL UTMESS('F','RESGRA',
     &    'CONFLIT UNE MATRICE'//
     &                ' STOCKEE MORSE NE PEUT AVOIR Q''UN SEUL BLOC')
        CALL JELIRA(JEXNUM(MATAS//'.VALE',1),'TYPE',IBID,CBID)
        IF (CBID(1:1).EQ.'C')  CALL UTMESS('F','RESGRA_5',
     &    ' GCPC N"EST '//
     &                'PAS PREVU POUR UNE MATRICE COMPLEXE')

        CALL JEVEUO(JEXNUM(MATAS//'.VALE',1),'L',IDAC)


C     6- RECUPERATION DE LA MATRICE DE PRECONDITIONNEMENT:
C     -----------------------------------------------------
      CALL JEEXIN(MATFAC//'.REFA',IRET)
      IF (IRET.EQ.0) CALL UTMESS('F','RESGRA',
     &  'PAS DE MATRICE DE PRECONDITIONNEMENT : ON S''ARRETE')

      CALL JELIRA(MATFAC//'.REFA','DOCU',IBID,ETAMAT)
      IF (ETAMAT.NE.'DECT')
     +      CALL UTMESS('F','OP0084','  PAS DE RESOLUTION '//
     +       'CAR LA MATRICE DE PRECONDITIONNEMENT '//MATFAC//
     +       ' N''EST PAS FACTORISEE.' )

C     LE SEUL PRECONDITIONNEMENT UTILISABLE EST LDLT :
      PREC='LDLT'

      CALL JEVEUO(MATFAC//'.REFA','L',JREFA)
      IF (PREC.EQ.'LDLT') THEN
        KSTOCF= ZK24(JREFA-1+2)(1:14)//'.SMOS'
        CALL JEVEUO(KSTOCF//'.ADIA','L',IDINPC)
        CALL JEVEUO(KSTOCF//'.HCOL','L',IDIPPC)
        CALL JELIRA(JEXNUM(MATFAC//'.VALE',1),'TYPE',IBID,CBID)
        CALL JEVEUO(JEXNUM(MATFAC//'.VALE',1),'L',IDACPC)
      END IF


C     7- CREATION DES 4 VECTEURS DE TRAVAIL
C     ------------------------------------------------
      CALL WKVECT('&&RESGRA.W1','V V R',NEQ,IDW1)
      CALL WKVECT('&&RESGRA.W2','V V R',NEQ,IDW2)
      CALL WKVECT('&&RESGRA.W3','V V R',NEQ,IDW3)
      CALL WKVECT('&&RESGRA.W4','V V R',NEQ,IDW4)


C     8- CREATION SI BESOIN DE X (VECTEUR DE DEPART)
C     ------------------------------------------------
      CALL JEEXIN(X19//'.REFE',IRET)
      IF (IRET.EQ.0) THEN
        IF (IREP.EQ.1) THEN
          CALL UTMESS('A','RESGRA_14',' LE CHAM_NO :'//X19//
     &         ' N"EXISTE PAS ==> REPRISE IMPOSSIBLE ==> INITIALISATION'
     &                //' PAR LE VECTEUR NUL')
        END IF
        IREP = 0
        ICRE = 1
      ELSE
        IF (X19.EQ.B19) IREP = 1
        IF ((IREP.EQ.0) .AND. (X19.NE.B19)) THEN
          CALL DETRSD('CHAMP_GD',X19)
          ICRE = 1
        ELSE
          ICRE = 0
        END IF
      END IF
      IF (ICRE.EQ.1) CALL VTDEFS(X19,B19,BASE(1:1),' ')

C     -- EXTRACTION DE B ET DE X
      CALL JEVEUO(X19//'.VALE','E',IDX)
      IF (IREP.EQ.1) CALL MVCON2(MATAS,0,'R',ZR(IDX),1)


C     9- RESOLUTION EFFECTIVE ---
C     ---------------------------------

C       WRITE (6,*) '??????????? APPEL NOUVEAU GCPC ????????'
        CALL GCPC(NEQ,ZI(IDIN),ZI(IDIP),ZR(IDAC),ZI(IDINPC),ZI(IDIPPC),
     &            ZR(IDACPC),ZR(IDB),ZR(IDX),ZR(IDW1),ZR(IDW2),ZR(IDW3),
     &            IREP,NITER,EPSI,CRITER)


C     10- MISE A JOUR DES COEFFICIENTS DE LAGRANGE :
C     ---------------------------------
      CALL MVCON1(MATAS,0,'R',ZR(IDX),1)


      CALL JEDETR('&&RESGRA.W1')
      CALL JEDETR('&&RESGRA.W2')
      CALL JEDETR('&&RESGRA.W3')
      CALL JEDETR('&&RESGRA.W4')
      CALL DETRSD('CHAMP_GD','&&RESGRA.SCMB')

      CALL JEDEMA()
      END
