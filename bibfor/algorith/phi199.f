      SUBROUTINE PHI199 ( MODEL, MATE, MA, NU, NUM, NBMODE,
     &                    SOLVEZ, INDICE, TABAD )
      IMPLICIT  NONE
      INTEGER             NBMODE, INDICE, TABAD(*)
      CHARACTER*8         MA
      CHARACTER*14        NU, NUM
      CHARACTER*(*)       MATE, SOLVEZ
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C---------------------------------------------------------------------
C
C
C CALCULS DES CONDITIONS AUX LIMITES POUR LA DETERMINATION
C DES POTENTIELS FLUCTUANTS POUR LA FORCE AJOUTEE,
C IN : K* : MODEL : TYPE DE MODELISATION FLUIDE
C IN : K* : MATE : MATERIAU FLUIDE
C IN : K* : PHIBAR : NOM DU POTENTIEL PERMANENT
C IN : K* : MA : NOM DE LA MATRICE DE RAIDEUR FLUIDE
C IN : K* : NU : NUMEROTATION DES DDLS ASSOCIES AU FLUIDE
C IN : K* : NUM : NUMEROTATION DES DDLS ASSOCIES A L'INTERFACE
C           POTENTIELS FLUCTUANTS : 1 : MASSE AJOUTEE
C                                 : 2 : AMORTISSEMENT ET RAIDEUR
C IN : K* : SOLVEZ : METHODE DE RESOLUTION 'MULT_FRONT','LDLT' OU 'GCPC'
C---------------------------------------------------------------------
C--------- DEBUT DES COMMUNS JEVEUX ----------------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16           ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     --- FIN DES COMMUNS JEVEUX ------------------------------------
      INTEGER       IBID,NBVALE,NBREFE,NBDESC,IRET,NBNO,IDNO,ID,IER
      INTEGER       ILIRES,JREF,NEQ,NBD,NBDIR,I,JVEC,JDDL,IN,NBSTA
      INTEGER       IPHI1,N3,N1,ICOR(2),N2,NDBLE,IORDR,NBTROU,IDMST
      REAL*8        RBID,XNORM,XD,DEPL(6),EPSI,R8PREM
      COMPLEX*16    C16B,CBID
      CHARACTER*2   MODEL
      CHARACTER*8   K8BID,MODMEC,MAILLA,MAFLUI,TABCMP(6),CRIT
      CHARACTER*8   MOFLUI,MOINT,TYPMCL(2),MODSTA
      CHARACTER*14  NUME
      CHARACTER*16  ACCES, MOTCLE(2)
      CHARACTER*19  VECSO1,VESTO1,MAPREC,SOLVEU,CHSOL,CHAMNO
      CHARACTER*24  NOMCHA,NOCHAM,CRITER
      CHARACTER*24 VALK(3)
C
      DATA MAPREC   /'&&OP0199.MAPREC'/
      DATA CHSOL    /'&&OP0199.SOLUTION'/
      DATA   TABCMP / 'DX' , 'DY' , 'DZ' , 'DRX' , 'DRY' , 'DRZ' /
      DATA NDBLE /0/
C -----------------------------------------------------------------
C
      CALL JEMARQ()
      EPSI = R8PREM( )
      IER = 0
      SOLVEU = SOLVEZ
      CRITER = '&&RESGRA_GCPC'
      INDICE=0
C
      CALL GETVID(' ', 'MODELE_FLUIDE'   , 0,1,1, MOFLUI, N1 )
      CALL GETVID(' ', 'MODELE_INTERFACE', 0,1,1, MOINT , N2 )
      CALL GETVID(' ', 'MODE_MECA'       , 0,1,1, MODMEC, N3 )
C
C --- TEST POUR DETERMINER SI FLUIDE ET STRUCTURE S APPUIENT SUR
C     DES MAILLAGES COMMUNS
C
      IF ( N3.GT.0 ) THEN
         CALL RSORAC ( MODMEC,'LONUTI',IBID,RBID,K8BID,CBID,RBID,
     &                 'ABSOLU',NBMODE,1,IBID)
         CALL RSEXCH ( MODMEC, 'DEPL', 1, NOMCHA, IRET )
         CALL DISMOI('F','NOM_MAILLA',NOMCHA,'CHAM_NO',IBID,MAILLA,IER)
         CALL DISMOI('F','NOM_MAILLA',MOINT ,'MODELE' ,IBID,MAFLUI,IER)
         IF ( MAFLUI .NE. MAILLA ) THEN
            CALL TABCOR(MODEL,MATE,MAILLA,MAFLUI,MOINT,NUM,NDBLE,ICOR)
            CALL MAJOU (MODEL,MODMEC,SOLVEU,NUM,NU,MA,MATE,MOINT,
     &                  NDBLE,ICOR,TABAD)
            INDICE=1
         ENDIF
      ENDIF
C
C=====================================================================
C---------------- ALTERNATIVE MODE_MECA OU---------
C-----------------------------MODELE-GENE--------------------
C=====================================================================
C DANS LE CAS OU ON N A PAS CALCUL DE MASSE AJOUTEE SUR UN
C MAILLAGE SQUELETTE

      IF ((N3.GT.0).AND.(INDICE.NE.1)) THEN

C
C----- -RECUPERATION DU NB DE MODES DU CONCEPT MODE_MECA
C
        CALL RSORAC(MODMEC,'LONUTI',IBID,RBID,K8BID,CBID,RBID,'ABSOLU',
     &              NBMODE,1,IBID)

        CALL WKVECT('&&OP0199.PHI1','V V K24',1,IPHI1)

C======================================================================
C BOUCLE SUR LE NOMBRE DE MODES: CALCUL DU FLUX FLUIDE MODAL
C======================================================================
        ILIRES = 0
        NOMCHA = '&&PHI199.CHAMREF'
        CALL RSEXCH(MODMEC,'DEPL',1,NOCHAM,IRET)
        NOCHAM = NOCHAM(1:19)//'.REFE'
        CALL JEVEUO(NOCHAM,'L',JREF)
        NUME = ZK24(JREF+1)(1:14)
        CALL VTCREB(NOMCHA,NUME,'V','R',NEQ)
C
C --- QUELLE EST LA DIRECTION ?
C
        CALL GETVR8 ( ' ', 'DIRECTION',0,1,0,DEPL,NBD)
        NBDIR = -NBD
        CALL GETVR8 ( ' ', 'DIRECTION',0,1,NBDIR,DEPL,NBD)
C
C     --- ON NORMALISE LE VECTEUR ---
        XNORM = 0.D0
        DO 10 I = 1,NBDIR
          XNORM = XNORM + DEPL(I) * DEPL(I)
 10     CONTINUE
        XNORM = SQRT(XNORM)
        IF (XNORM.LT.0.D0) THEN
           CALL U2MESS('F','ALGORITH9_81')
        ENDIF
        DO 12 I = 1,NBDIR
          DEPL(I) = DEPL(I) / XNORM
 12     CONTINUE
C
        CALL JEVEUO(NOMCHA(1:19)//'.VALE','E',JVEC)
        CALL WKVECT('&&PHI199.DDL','V V I',NEQ*NBDIR,JDDL)
        CALL PTEDDL('NUME_DDL',NUME,NBDIR,TABCMP,NEQ,ZI(JDDL))

        DO 21 IN = 0,NEQ-1
           ZR(JVEC+IN) = 0.D0
 21     CONTINUE
C
C     --- ON RECUPERE LES MODES STATIQUES ---
C
        CALL GETVID ( ' ', 'MODE_STAT', 0,1,1, MODSTA, NBSTA )
        IF (NBSTA.EQ.0) GOTO 41
C
C     --- ON RECUPERE LES POINTS D'ANCRAGE ---
C
        MOTCLE(1) = 'NOEUD'
        MOTCLE(2) = 'GROUP_NO'
        TYPMCL(1) = 'NOEUD'
        TYPMCL(2) = 'GROUP_NO'
        CALL RELIEM(' ',MAILLA,'NO_NOEUD',' ',1, 2, MOTCLE, TYPMCL,
     &                                   '&&PHI199.NOEUD', NBNO )
        CALL JEVEUO ( '&&PHI199.NOEUD', 'L', IDNO )
C
C     --- ON BOUCLE SUR LES NOEUDS ---
C
        DO 25 ID = 1,NBDIR
          XD = DEPL(ID)
          IF (ABS(XD).GT.EPSI) THEN
            DO 26 IN = 1 , NBNO
               ACCES(1:8 ) = ZK8(IDNO+IN-1)
               ACCES(9:16) = TABCMP(ID)
C
C              --- ON RECUPERE LE MODE STATIQUE ASSOCIE AU NOEUD ---
               CALL RSORAC(MODSTA,'NOEUD_CMP',IBID,RBID,ACCES,C16B,
     &                     EPSI,CRIT,IORDR,1,NBTROU)
               IF (NBTROU.NE.1) THEN
                  IER = IER + 1
                  VALK (1) = ACCES(1:8)
                  VALK (2) = ACCES(9:16)
                  CALL U2MESG('E', 'ALGORITH13_88',2,VALK,0,0,0,0.D0)
                  GOTO 26
               ENDIF
               CALL RSVPAR(MODSTA,IORDR,'TYPE_DEFO',IBID,RBID,
     &                                  'DEPL_IMPO',IRET)
               IF (IRET.NE.100) THEN
                  IER = IER + 1
                  VALK (1) = 'DDL_IMPO'
                  VALK (2) = ACCES(1:8)
                  VALK (3) = ACCES(9:16)
                  CALL U2MESG('E', 'ALGORITH13_89',3,VALK,0,0,0,0.D0)
                  GOTO 26
               ENDIF
               CALL RSEXCH(MODSTA,'DEPL',IORDR,CHAMNO,IRET)
               IF (IRET.NE.0) THEN
                  IER = IER + 1
                  VALK (1) = CHAMNO
                  VALK (2) = ACCES(1:8)
                  VALK (3) = ACCES(9:16)
                  CALL U2MESG('E', 'ALGORITH13_90',3,VALK,0,0,0,0.D0)
                  GOTO 26
               ELSE
                  CALL JEVEUO(CHAMNO//'.VALE','L',IDMST)
C
                  DO 27 I = 0,NEQ-1
                     ZR(JVEC+I) = ZR(JVEC+I)
     &                - ZI(JDDL+(ID-1)*NEQ+I)*XD*ZR(IDMST+I)
 27               CONTINUE
                  CALL JELIBE(CHAMNO//'.VALE')
               ENDIF
 26         CONTINUE
          ENDIF
 25     CONTINUE
        IF (IER.NE.0) THEN
          CALL U2MESS('F','ALGORITH5_24')
        ENDIF
C
        GOTO 42
C
 41     CONTINUE
        DO 23 I = 1 , NBDIR
          DO 24 IN = 0,NEQ-1
            ZR(JVEC+IN) = ZR(JVEC+IN) - ZI(JDDL+(I-1)*NEQ+IN)*DEPL(I)
 24       CONTINUE
 23     CONTINUE
 42     CONTINUE
C
        NOMCHA = NOMCHA(1:19)
        VECSO1 = '&&OP0199.VECSOL1'

        CALL CALFLU ( NOMCHA, MOFLUI, MATE, NU, VECSO1, NBDESC, NBREFE,
     &                NBVALE, 'R' )

        ILIRES = ILIRES + 1

C------------- RESOLUTION  DU LAPLACIEN EN 2D-----------------------

        CALL RESOUD(MA,MAPREC,VECSO1,SOLVEU,' ','V',CHSOL,CRITER)
        CALL JEDUPC('V',CHSOL(1:19),1,'V',VECSO1(1:19),.FALSE.)
        CALL JEDETC('V',CHSOL,1)

C------------ CREATION DU VECTEUR PRESSION MODAL-------------------
C
C- FORMATION DU TABLEAU CONTENANT LA PRESSION POUR CHAQUE MODE-----
C
C------------------------------------------------------------------
        VESTO1 = '&&OP0199.VEST1'
        CALL PRSTOC ( VECSO1, VESTO1,
     &                ILIRES, ILIRES, IPHI1, NBVALE, NBREFE, NBDESC )
C
        CALL JEDETC('V',VECSO1,1)
C
      ENDIF
      CALL JEDETC('V','&&PHI199',1)
C
      CALL JEEXIN (CRITER(1:19)//'.CRTI',IRET)
      IF ( IRET .NE. 0 ) THEN
         CALL JEDETR ( CRITER(1:19)//'.CRTI' )
         CALL JEDETR ( CRITER(1:19)//'.CRTR' )
         CALL JEDETR ( CRITER(1:19)//'.CRDE' )
      ENDIF
C----------------------------------------------------------------
      CALL JEDEMA()
      END
