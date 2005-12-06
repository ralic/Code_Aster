      SUBROUTINE TLDLC8(NOMMAT,HCOL,ADIA,ABLO,NPIVOT,NEQ,NBBLOC,ILDEB,
     +                  ILFIN,EPS)
      IMPLICIT NONE
C
      CHARACTER*(*) NOMMAT
      INTEGER HCOL(*),ADIA(*),ABLO(*)
      INTEGER NPIVOT,NEQ,NBBLOC,ILDEB,ILFIN
      REAL*8 EPS
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 22/03/99   AUTEUR VABHHTS J.PELLET 
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
C COMPIL PARAL
C     FACTORISATION DE GAUSS PAR LA VARIANTE DE CROUT SOUS FORME L*D*LT
C     D'UNE MATRICE SYMETRIQUE A COEFFICIENTS COMPLEXES
C                     LA FACTORISATION EST EN PLACE
C     ------------------------------------------------------------------
C
C     IN  NOMMAT  :    : NOM UTILISATEUR DE LA MATRICE A FACTORISER
C     IN  HCOL    : IS : HCOL DE LA MATRICE
C                        HCOL(I) RENVOIE LA HAUTEUR DE LA I-EME COLONNE
C     IN  ADIA    : IS : ADRESSE DU TERME DIAGONALE DANS SON BLOC
C                        ADIA(I) RENVOIE L'ADRESSE DE LA I-EME LIGNE
C                        DANS SON BLOC
C     IN  ABLO    :    : POINTEUR DE BLOC
C                        ABLO(I+1) RENVOIE LE NO DE LA DERNIERE
C                        LIGNE DU I-EME BLOC
C
C     VAR PIVOT   : IS :
C                 : SORTIE : NPIVOT = 0 ==> R.A.S.
C                 :   NPIVOT > 0 ==> MATRICE SINGULIERE
C                                    POUR L'EQUATION DE NUMERO NPIVOT
C
C     IN  NEQ     : IS : NOMBRE TOTAL D'EQUATION
C     IN  NBBLOC  : IS : NOMBRE DE BLOC DE LA MATRICE
C     IN  ILDEB   : IS : NUMERO DE LA LIGNE DE DEPART DE FACTORISATION
C     IN  ILFIN   : IS : NUMERO DE LA LIGNE DE FIN    DE FACTORISITION
C
C     ------------------------------------------------------------------
C
C     CREATION DE DEUX OBJETS DE TRAVAIL (SUR LA VOLATILE)
C        1)  UN TABLEAU POUR LA DIAGONALE
C        2)  UN TABLEAU POUR LA COLONNE COURANTE
C
C     --- RAPPEL SOMMAIRE DE L'ALGORITHME ------------------------------
C
C     POUR S = 2,3, ... ,N
C     !  POUR I = 1,2, ... ,S-1
C     !  !  POUR M = 1,2, ... ,I-1
C     !  !  !  K(S,I) = K(S,I) - K(S,M)*K(M,I) % MODIFIE   LA LIGNE
C     !  !  !  K(I,S) = K(I,S) - K(I,M)*K(M,S) % MODIFIE   LA COLONNE
C     !  !  FIN_POUR
C     !  !  K(S,I) = K(S,I)/K(I,I)           % NORMALISATION DE LA LIGNE
C     !  FIN_POUR
C     !  POUR M = 1,2, ... ,S-1
C     !  !  K(S,S) = K(S,S) - K(S,M) * K(M,S)  % MODIFICATION DU PIVOT
C     !  FIN_POUR
C     FIN_POUR
C
C     ------------------------------------------------------------------
C
C     REFERENCE (HISTORIQUE) :
C     (1) P.D. CROUT,
C         A SHORT METHOD FOR EVALUATING DETERMINANTS AND SOLVING SYSTEMS
C         OF LINEAR EQUATIONS WITH REAL OR COMPLEX COEFFICIENTS.
C         AIEE TRANSACTION VOL 60, PP 1235-1240  (1941)
C
C     ------------------------------------------------------------------
C
      INTEGER IFM,NIV,IER,LDIAG,LTRAV
      INTEGER IBLOC,IL1,IL2,IAA,IL,KL1
      INTEGER IMINI,IEQUA,I,JBLMIN,JBLOC,JL1,JL2
      INTEGER IAB,ILONG,IADIA,IDE,IDL
      INTEGER JNMINI,JEQUA,JLONG,JADIA,JDE,JDL,IBCL1
      INTEGER LM,ICA,ICB,ICD
      INTEGER IADIGS
C
C     ----- CNJ --------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- CNJ --------------------------------------------------------
C
      COMPLEX*16 VAL
      CHARACTER*1 VNPES
      CHARACTER*19 NOMA19
      CHARACTER*24 NOMDIA,VALE,NOMTRA
      CHARACTER*32 JEXNUM
C
C     ------------------------------------------------------------------
C
      DATA VALE/'                   .VALE'/
      DATA NOMDIA/'                   .&VDI'/
      DATA NOMTRA/'                   .&TRA'/
C
C     ------------------------------------------------------------------
C
      CALL INFNIV(IFM,NIV)
      CALL JEMARQ()
C
C
C     --- INITIALISATIONS ----------------------------------------------
C
      NOMA19 = NOMMAT
C
      VALE(1:19) = NOMMAT
      NOMDIA(1:19) = NOMMAT
      NOMTRA(1:19) = NOMMAT
C
      NPIVOT = 0
C
C     --- CREATION/RAPPEL D'UN TABLEAU POUR STOCKER LA DIAGONALE -------
C
      CALL JEEXIN(NOMDIA,IER)
      IF (IER.EQ.0) THEN
        CALL JECREO(NOMDIA,'V V C')
        CALL JEECRA(NOMDIA,'LONMAX',NEQ,'  ')
      END IF
      CALL JEVEUO(NOMDIA,'E',LDIAG)
      CALL JEVEUO(NOMA19//'.DIGS','E',IADIGS)
C
C     --- CREATION/RAPPEL D'UN TABLEAU INTERMEDIAIRE (D'UNE COLONNE) ---
C
      CALL JEEXIN(NOMTRA,IER)
      IF (IER.EQ.0) THEN
        CALL JECREO(NOMTRA,'V V C')
        CALL JEECRA(NOMTRA,'LONMAX',NEQ,'  ')
      END IF
      CALL JEVEUO(NOMTRA,'E',LTRAV)
C
C     --- BOUCLE SUR LES BLOCS A TRANSFORMER ---------------------------
C
      DO 160 IBLOC = 1,NBBLOC
C
        IL1 = ABLO(IBLOC) + 1
        IL2 = ABLO(IBLOC+1)
C
        IF (IL2.LT.ILDEB) THEN
C           --- C'EST TROP TOT : MAIS ON REMPLIT LA DIAGONALE ----------
          CALL JEVEUO(JEXNUM(VALE,IBLOC),'L',IAA)
CCDIR$ IVDEP
          DO 10 IL = IL1,IL2
            ZC(LDIAG+IL-1) = ZC(IAA+ADIA(IL)-1)
   10     CONTINUE
          CALL JELIBE(JEXNUM(VALE,IBLOC))
          GO TO 160
        ELSE IF (IL1.GT.ILFIN) THEN
C           --- C'EST FINI ---------------------------------------------
          GO TO 170
        ELSE
          CALL JEVEUO(JEXNUM(VALE,IBLOC),'E',IAA)
          IF (IL1.LT.ILDEB) THEN
            KL1 = ILDEB
CCDIR$ IVDEP
            DO 20 IL = IL1,KL1 - 1
              ZC(LDIAG+IL-1) = ZC(IAA+ADIA(IL)-1)
   20       CONTINUE
          ELSE
            KL1 = IL1
          END IF
          IF (IL2.GT.ILFIN) IL2 = ILFIN
        END IF
C
C        --- RECHERCHE DE LA PLUS PETITE EQUATION EN RELATION AVEC -----
C        --- UNE EQUATION DES LIGNES EFFECTIVES DU BLOC COURANT --------
C
        IMINI = IL2 - 1
        DO 30 IEQUA = KL1,IL2
          IMINI = MIN(IEQUA-HCOL(IEQUA),IMINI)
   30   CONTINUE
        IMINI = IMINI + 1
C
C        --- RECHERCHE DU BLOC D'APPARTENANCE DE L'EQUATION IMINI ------
C
        DO 40 I = 1,IBLOC
          JBLMIN = I
          IF (ABLO(1+I).GE.IMINI) GO TO 50
   40   CONTINUE
   50   CONTINUE
C
C        --- BOUCLE  SUR  LES  BLOCS  DEJA  TRANSFORMES ----------------
        DO 90 JBLOC = JBLMIN,IBLOC - 1
C
          JL1 = MAX(IMINI,ABLO(JBLOC)+1)
          JL2 = ABLO(JBLOC+1)
          CALL JEVEUO(JEXNUM(VALE,JBLOC),'L',IAB)
C
CMIC$ DO ALL SHARED (ADIA, HCOL, IAA, IAB, IL2, JL1, JL2, KL1)
CMIC$*       SHARED (ZC)
CMIC$*       PRIVATE (I, IADIA, IBCL1, ICA, ICB, IDE, IDL, IEQUA)
CMIC$*       PRIVATE (ILONG, JADIA, JDE, JDL, JEQUA, JLONG, JNMINI)
CMIC$*       PRIVATE (LM, VAL)
          DO 80 IEQUA = KL1,IL2
C
C              --- RECUPERATION ADRESSE ET LONGUEUR DE LA LIGNE --------
            ILONG = HCOL(IEQUA) - 1
            IADIA = IAA + ADIA(IEQUA) - 1
            IDE = IADIA - ILONG
            IDL = IEQUA - ILONG
C
C              --- UTILISATION DES LIGNES (IDL+1) A (JL2) --------------
            JNMINI = MAX(IDL,JL1)
            DO 70 JEQUA = JNMINI,JL2
              JLONG = HCOL(JEQUA) - 1
              JADIA = IAB + ADIA(JEQUA) - 1
              JDE = JADIA - JLONG
              JDL = JEQUA - JLONG
              IBCL1 = MAX(IDL,JDL)
              LM = JEQUA - IBCL1
              ICA = IDE + IBCL1 - IDL
              ICB = JDE + IBCL1 - JDL
              VAL = ZC(ICA+LM)
              DO 60 I = 0,LM - 1
                VAL = VAL - ZC(ICA+I)*ZC(ICB+I)
   60         CONTINUE
              ZC(ICA+LM) = VAL
   70       CONTINUE
   80     CONTINUE
          CALL JELIBE(JEXNUM(VALE,JBLOC))
   90   CONTINUE
C
C        --- UTILISATION DU BLOC EN COURS DE TRANSFORMATION ------------
C
        JL1 = MAX(IMINI,IL1)
        DO 150 IEQUA = KL1,IL2
C
C           --- RECUPERATION DE L ADRESSE ET LA LONGUEUR DE LA LIGNE ---
C           IADIA : ADRESSE DU TERME DIAGONAL COURANT
C           IDE   : ADRESSE DU DEBUT DE LA LIGNE COURANTE
C           IDL   : 1-ER DDL A VALEUR NON NULLE DANS LA LIGNE
C
          ILONG = HCOL(IEQUA) - 1
          IADIA = IAA + ADIA(IEQUA) - 1
          IDE = IADIA - ILONG
          IDL = IEQUA - ILONG
C
C           --- UTILISATION DES LIGNES (IDL+1) A (IEQUA-1) -------------
C
          JNMINI = MAX(IEQUA-ILONG,JL1)
C
          DO 110 JEQUA = JNMINI,IEQUA - 1
            JLONG = HCOL(JEQUA) - 1
            JADIA = IAA + ADIA(JEQUA) - 1
            JDE = JADIA - JLONG
            JDL = JEQUA - JLONG
            IBCL1 = MAX(IDL,JDL)
            LM = JEQUA - IBCL1
            ICA = IDE + IBCL1 - IDL
            ICB = JDE + IBCL1 - JDL
            VAL = ZC(ICA+LM)
            DO 100 I = 0,LM - 1
              VAL = VAL - ZC(ICA+I)*ZC(ICB+I)
  100       CONTINUE
            ZC(ICA+LM) = VAL
  110     CONTINUE
C
C
C           --- UTILISATION DE LA LIGNE IEQUA (CALCUL DU PIVOT) --------
  120     CONTINUE
          LM = ILONG - 1
          ICA = IADIA - ILONG
C
C           --- SAUVEGARDE COLONNE ET NORMALISATION LIGNE --------------
          ICD = LDIAG + IEQUA - ILONG - 1
          DO 130 I = 0,LM
            ZC(LTRAV+I) = ZC(ICA+I)
            ZC(ICA+I) = ZC(ICA+I)/ZC(ICD+I)
  130     CONTINUE
C
C           --- CALCUL DU TERME DIAGONAL -------------------------------
C
          VAL = ZC(IADIA)
          DO 140 I = 0,LM
            VAL = VAL - ZC(ICA+I)*ZC(LTRAV+I)
  140     CONTINUE
          ZC(IADIA) = VAL
          ZR(IADIGS-1+NEQ+IEQUA) = ABS(ZC(IADIA))
          ZC(LDIAG+IEQUA-1) = VAL
C
C           --- LE PIVOT EST-IL NUL ? ----------------------------------
C
          IF (ABS(VAL).LE.EPS) THEN
            NPIVOT = IEQUA
            GO TO 9999
          END IF
C
  150   CONTINUE
        CALL JELIBE(JEXNUM(VALE,IBLOC))
        IF (NIV.EQ.2) THEN
          WRITE (IFM,*) '>>> FACTORISATION (LDLT_C8):',' FIN DU BLOC ',
     +      IBLOC,' SUR ',NBBLOC,' SOIT DU DDL ',IL1,' AU  DDL ',IL2,
     +      ' INCLUS'
        END IF
  160 CONTINUE
C
  170 CONTINUE
 9999 CONTINUE
      CALL JEDEMA()
      END
