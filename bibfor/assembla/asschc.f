      SUBROUTINE ASSCHC(BASE,MATAS,NBCHC,LCHCI,NOMNU,MOTC)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) MATAS,LCHCI(*),NOMNU
      CHARACTER*1 BASE
      CHARACTER*4 MOTC
      INTEGER NBCHC
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C OBJET :
C        TRAITEMENT DES CHARGE CINEMATIQUE DANS LES MATRICE ASSEMBLEES
C
C-----------------------------------------------------------------------
C IN   BASE    K*1     : 'G','V' BASE SUR LAQUELLE EST MATAS
C VAR  MATAS   K*19    : NOM DE LA MATR_ASSE
C IN   NBCHC   I       : NOMBRE DE CHARGE CINEMATIQUES
C IN   LCHCI   K*19    : LISTE DES NOMS DES CHARGES CINEMATIQUES
C                        L'EFFET DE CES CHARGES EST CUMULE DANS MATAS
C IN   NOMNU   K*14    : NOM DE LA NUMEROTATION
C IN   MOTC    K*4     : 'ZERO' OU 'CUMU'
C                        'ZERO': SI .CONI EXISTE DEJA ON S'ARRETE EN
C                        ERREUR 'F' CAR SI ON VEUT ELIMINER LE .CONI
C                        DEJA EXISTANT IL FAUT REMPLIR LES COLONNES
C                        MISES A 0. DANS LA MATRICE
C                        'CUMU': SI .CONI EXISTE ON L'ENRICHI
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*2 TYPSTO
      CHARACTER*8 KBID,GD
      CHARACTER*14 NU
      CHARACTER*19 MAT,STOMAT,NOMCH
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
      CALL JEMARQ()
C----------------------------------------------------------------------
      MAT = MATAS
      NU = NOMNU
      IF (NBCHC.EQ.0) GOTO 9999
      CALL JEVEUO(NU//'.NUME.NEQU','L',IDEQU)
      NEQU = ZI(IDEQU)
      CALL DISMOI('F','NOM_GD',NU,'NUME_DDL',IBID,GD,IERD)
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',GD),NUMGD)
      CALL JEVEUO(JEXNUM('&CATA.GD.DESCRIGD',NUMGD),'L',IDDES)
      NEC = ZI(IDDES+2 )
      CALL JEEXIN(MAT//'.CONI',IER)
      IF (IER.NE.0) THEN
        IF (MOTC.EQ.'ZERO') THEN
C         CALL JEDETR(MAT//'.CONI')
C         CALL WKVECT(MAT//'.CONI',BASE//' V I ',NEQU,IDCONI)
C     QUAND ON RECONSTRUIRA LA MATRICE
          CALL UTMESS('F','ASSCHC_1','LA MATRICE POSSEDE DEJA DES '
     +    //'CHARGES CINEMATIQUES => ON N"Y TOUCHE PLUS OU ON '
     +    //'L"ENRICHIE')
        ELSE IF (MOTC.EQ.'CUMU') THEN
          CALL JEVEUO(MAT//'.CONI','E',IDCONI)
        ELSE
          CALL UTMESS('F','ASSCHC_1',' LES ARGUMENTS POSSIBLES DE MOTC'
     +    //' SONT ZERO OU CUMU')
        ENDIF
      ELSE
        CALL WKVECT(MAT//'.CONI',BASE//' V I ',NEQU,IDCONI)
      ENDIF
      CALL JEVEUO(JEXNUM(NU//'.NUME.PRNO',1),'L',IDPRNO)
      DO 1 ICH = 1,NBCHC
        NOMCH = LCHCI(ICH)
        CALL JEVEUO(NOMCH//'.DEFI','L',IDEFI)
        NIMP = ZI(IDEFI)
        DO 10 IMP = 1,NIMP
          INO = ZI( IDEFI+3*(IMP-1)+1)
          IDDL = ZI( IDEFI+3*(IMP-1)+2)
          IEQ = ZI(IDPRNO-1+(NEC+2)*(INO-1)+1)+ IDDL - 1
          IEXI = ZI(IDCONI-1+IEQ)
          IF (IEXI.EQ.0) THEN
            ZI( IDCONI-1+IEQ) = -1
          ENDIF
10      CONTINUE
1     CONTINUE
C
C --- STOCKAGE DES LIGNES A ELIMINER CONI(IEQ)=-1 A CAUSE DE CUMU
C     POUR NE PAS RESTOCKER UNE LIGNE DEJA TRAITEE
      CALL WKVECT('&&ASSCHC.STOC','V V I ',NEQU,IDSTOC)
      NSTOC = 0
      DO 2 IEQ = 1, NEQU
        IF ( ZI( IDCONI-1+IEQ).EQ.-1) THEN
          NSTOC = NSTOC+1
          ZI( IDSTOC-1+IEQ) = NSTOC
          ZI( IDCONI-1+IEQ) = 1
        ENDIF
2     CONTINUE
      CALL JEVEUO(MAT//'.REFA','L',IDREFE)
      STOMAT = ZK24(IDREFE+2)
      TYPSTO = STOMAT(17:18)
      IF (TYPSTO.EQ.'LC') THEN
        CALL ASLCHC(BASE,MAT,ZI(IDSTOC),NSTOC,MOTC)
      ELSE IF (TYPSTO.EQ.'MO') THEN
        CALL ASMCHC(BASE,MAT,STOMAT,ZI(IDSTOC),NSTOC,MOTC)
      ELSE
        CALL UTMESS('F','ASSCHC_3','TYPE DE STOCKAGE INCONNU :'//TYPSTO)
      ENDIF
C
C-----INTERROGATION SUR LE NIVEAU D'IMPRESSION
C
      CALL INFNIV(IFM,NIV)
      IF (NIV.GT.2) THEN
        CALL JEIMPO('RESULTAT',MAT//'.CONI',' ','TABLE D"ELIMINATION')
        CALL JEIMPA('RESULTAT',MAT//'.CONI',' ')
        CALL JEIMPO('RESULTAT',MAT//'.LLIG',' ','LARGEUR DES LIGNES '
     +   //'ELIMINEES')
        CALL JEIMPA('RESULTAT',MAT//'.LLIG',' ')
        CALL JEIMPO('RESULTAT',MAT//'.ALIG',' ','AD. DE DEBUT DE LIGNE')
        CALL JEIMPA('RESULTAT',MAT//'.ALIG',' ')
        CALL JEIMPO('RESULTAT',MAT//'.VALI',' ','VALEURS DES LIGNES '
     +   //'ELIMINEES')
        CALL JEIMPA('RESULTAT',MAT//'.VALI',' ')
      ENDIF
      CALL JEDETR('&&ASSCHC.STOC')
 9999 CONTINUE
      CALL JEDEMA()
      END
