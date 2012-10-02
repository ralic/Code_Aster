      SUBROUTINE CRICHO (NBMODE,RIGGEN,NBCHOC,PARCHO,NOECHO,INFO,
     &    FIMPO,RFIMPO,TRLOC,SOUPL,INDIC,NEQ,BMODAL,SEUIL,MARIG,
     &    NBNLI)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 02/10/2012   AUTEUR DESOZA T.DESOZA 
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
C
C     CALCUL DES TAUX DE RECONSTITUTION
C     ------------------------------------------------------------------
C IN  : NBMODE : NOMBRE DE MODES
C IN  : RIGGEN : RAIDEURS GENERALISES
C IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
C IN  : PARCHO : TABLEAU DES PARAMETRES DE CHOC
C IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
C OUT : SEUIL  :
C ----------------------------------------------------------------------
C



      INCLUDE 'jeveux.h'
      INTEGER            NBCHOC, INFO, NBMODE,IRIGI,INDIC(NBMODE),IBID
      INTEGER VALI
      INTEGER            NEQ,NBNLI,ISTOAV,IRET,IFAC
      INTEGER            JSLVI
      REAL*8             RIGGEN(*),SEUIL,
     &                   PARCHO(NBNLI,*)
      CHARACTER*8        NOECHO(NBNLI,*)
      REAL*8             TRLOC(NBMODE),SOUPL(NBMODE),TRLOCJ
      REAL*8 VALR(3)
      REAL*8             FIMPO(NEQ),RFIMPO(NEQ)
      REAL*8             BMODAL(NEQ,NBMODE),SOUP,CEF,TX
      CHARACTER*19       MARIG
      CHARACTER*19 SOLVEU,MATPRE
      CHARACTER*24 VALK
      COMPLEX*16         CBID
      INTEGER I,J,JJ,K,IA,IC,JM,IDDLX,IDDLY,IDDLZ,NUNOE
      REAL*8 CC,CS,CT,SCF,RSCF,USR,NORMX

      LOGICAL     MATUV
      INTEGER     NM, M, N, IERR, NBLIG, ICOLC
      INTEGER     NBCH1,NBCH2,NEQCH1,NEQCH2
      INTEGER     JEFLOC,JRFIMP,JNORMX,JNORMY,JA,JW,JU,JV
      REAL*8      MMAX,MMIN,SCOND,EPS

C
C      SEUIL=1.D0
C-----------------------------------------------------------------------
      REAL*8 DDOT ,R8PREM 
C-----------------------------------------------------------------------
      SEUIL=0.D0
C      EPS=1.D-50
      EPS = R8PREM( )
      IFAC = 0
      NBLIG = NEQ
      ICOLC = 0
C
      NBCH1 = 1+2*NBCHOC
      NBCH2 = 2*NBCHOC
      NEQCH1 = NEQ*NBCH1
      NEQCH2 = NEQ*NBCH2
C
      CALL MTDSCR(MARIG)
      CALL JEVEUO(MARIG//'.&INT','E',IRIGI)
C
      CALL WKVECT('&&CRICHO.EFLOC','V V R',NBMODE,JEFLOC)
      CALL WKVECT('&&CRICHO.RFIMPOX','V V R',NEQCH2,JRFIMP)
      CALL WKVECT('&&CRICHO.NORMXX','V V R',NBCH2,JNORMX)
      CALL WKVECT('&&CRICHO.NORMY','V V R',NBMODE,JNORMY)
      CALL WKVECT('&&CRICHO.A','V V R',NEQCH1,JA)
      CALL WKVECT('&&CRICHO.W','V V R',NBCH1,JW)
      CALL WKVECT('&&CRICHO.U','V V R',NEQCH1,JU)
      CALL WKVECT('&&CRICHO.V','V V R',NEQCH1,JV)
C
      IF ( NBCHOC.GT.0 ) THEN
        DO 20 I = 1,NBCHOC
          JM=1
          IF (NOECHO(I,9)(1:2).EQ.'BI') JM=2
          DO 21 JJ=1,JM
            ICOLC = ICOLC+1
            CT=0.D0
            CEF=0.D0
            IC=4*JJ-3
            CALL U2MESS('I','VIDE_1')
            IF (INFO.GE.2) THEN
               VALK = NOECHO(I,IC)
               CALL U2MESK('I','SOUSTRUC_85',1,VALK)
            ENDIF
C     CREATION DE FIMPO : FORCE UNITAIRE AU NOEUD DE CHOC (N)
            CALL U2MESS('I','VIDE_1')
            DO 11 K=1,NEQ
              FIMPO(K)=0.D0
   11       CONTINUE
            CALL POSDDL('NUME_DDL',NOECHO(I,IC+2),NOECHO(I,IC),
     &                  'DX',NUNOE,IDDLX)
            CALL POSDDL('NUME_DDL',NOECHO(I,IC+2),NOECHO(I,IC),
     &                  'DY',NUNOE,IDDLY)
            CALL POSDDL('NUME_DDL',NOECHO(I,IC+2),NOECHO(I,IC),
     &                  'DZ',NUNOE,IDDLZ)
            FIMPO(IDDLX)=PARCHO(I,45)
            FIMPO(IDDLY)=PARCHO(I,46)
            FIMPO(IDDLZ)=PARCHO(I,47)
C
C           CALCUL DE RFIMPO : K*N
            CALL MRMULT('ZERO',IRIGI,FIMPO,RFIMPO,1,.TRUE.)

            IF (IFAC.EQ.0) THEN
              CALL DISMOI('F','SOLVEUR',MARIG,'MATR_ASSE',IBID,
     &                    SOLVEU,IBID)
              CALL ASSERT(SOLVEU.EQ.'&&OP0074.SOLVEUR')
              MATPRE='&&OP0074.BIDON'
C
C             ISTOP MIS A 2 POUR NE PAS ARRETER L'EXECUTION EN CAS
C             DE MATRICE SINGULIERE (MODES STATIQUES)
              CALL JEVEUO(SOLVEU//'.SLVI','E',JSLVI)
              ISTOAV=ZI(JSLVI-1+3)
              ZI(JSLVI-1+3)=2
              CALL PRERES(SOLVEU,'V',IRET,MATPRE,MARIG,IBID,-9999)
C             -- ON RETABLIT ISTOP
              ZI(JSLVI-1+3)=ISTOAV
              IF (IRET.EQ.2) THEN
                CALL U2MESS('A','SOUSTRUC_7')
                GOTO 9999
              ELSE IF (IRET.EQ.1) THEN
                CALL U2MESS('A','SOUSTRUC_8')
                GOTO 9999
              ENDIF
              IFAC=1
            ENDIF

C           FIMPO : DEFORMEE STATIQUE (K-1*N)
            CALL RESOUD(MARIG ,' '   ,' '   ,' '   ,1     ,
     &                  ' '   ,' '   ,' '   ,FIMPO ,CBID  ,
     &                  ' '   ,.TRUE.,0     ,IRET  )
C           NORMX : NORME K-1*N
            NORMX=DDOT(NEQ,FIMPO,1,FIMPO,1)
            ZR(JNORMX-1+ICOLC)=NORMX
C           RFIMPOX : K-1*N (SAUVEGARDE DEFORMEE STATIQUE)
            DO 41 K=1,NEQ
              ZR(JRFIMP-1+K+NEQ*(ICOLC-1))=FIMPO(K)
   41       CONTINUE
C
C     CALCUL DE SOUP : TN*K-1*N
            SOUP = PARCHO(I,45)*FIMPO(IDDLX)
            SOUP = SOUP + PARCHO(I,46)*FIMPO(IDDLY)
            SOUP = SOUP + PARCHO(I,47)*FIMPO(IDDLZ)
            DO 12 K=1,NEQ
              FIMPO(K)=0.D0
   12       CONTINUE
            FIMPO(IDDLX)=PARCHO(I,45)
            FIMPO(IDDLY)=PARCHO(I,46)
            FIMPO(IDDLZ)=PARCHO(I,47)
            DO 22 J = 1,NBMODE
               IF (RIGGEN(J).LE.0.D0) THEN
                 USR=0.D0
               ELSE
                 USR=1.D0/RIGGEN(J)
               ENDIF
C     RSCF : TYNU*K*N
C     SCF : TYNU*N
               RSCF=DDOT(NEQ,BMODAL(1,J),1,RFIMPO,1)
               SCF=DDOT(NEQ,BMODAL(1,J),1,FIMPO,1)
               CC=SCF*RSCF*USR
               CS=SCF**2*USR
               IF (INFO.GE.2) THEN
                 SOUPL(J)=CS
                 IF (SOUP.NE.0.D0) THEN
                   TRLOC(J)=CS/SOUP
                 ELSE
                   TRLOC(J)=0.D0
                 ENDIF
                 INDIC(J)=J
                 TRLOCJ=TRLOC(J)
               ELSE
                 IF (SOUP.NE.0.D0) THEN
                   TRLOCJ=CS/SOUP
                 ELSE
                   TRLOCJ=0.D0
                 ENDIF
               ENDIF
               CT=CT+TRLOCJ
               ZR(JEFLOC-1+J)=CC
               CEF = CEF + CC
 22         CONTINUE
            PARCHO(I,48+JJ-1)=CT
C            IF (CT.NE.0.D0) SEUIL=MIN(SEUIL,CT)
C
            IF (INFO.GE.2) THEN
C      ON ORDONNE SELON LES SOUPLESSES DECROISSANTES
               CALL MDTRIB (INDIC,SOUPL,NBMODE)
               DO 32 J = 1,NBMODE
                  VALI = INDIC(J)
                  VALR (1) = TRLOC(INDIC(J))
                  VALR (2) = SOUPL(INDIC(J))
                  VALR (3) = ZR(JEFLOC-1+INDIC(J))
                  CALL U2MESG('I','SOUSTRUC_93',0,' ',1,VALI,3,VALR)
 32            CONTINUE
            ENDIF
            VALK = NOECHO(I,IC)
            VALR (1) = CT
            VALR (2) = CEF
            CALL U2MESG('I','SOUSTRUC_94',1,VALK,0,0,2,VALR)
            TX = SOUP*PARCHO(I,2)*(1.D0-CT)
            VALR (1) = TX
            CALL U2MESR('I','SOUSTRUC_95',1,VALR)
            SEUIL=MAX(SEUIL,TX)
            TX = SOUP*CT*PARCHO(I,2)
            VALR (1) = TX
            CALL U2MESR('I','SOUSTRUC_96',1,VALR)
 21       CONTINUE
C
 20     CONTINUE

        IF (INFO.GE.2) THEN
          CALL U2MESS('I','VIDE_1')
          MATUV = .FALSE.
          NM = NBLIG
          M = NEQ
C
C LA MATRICE A CONTIENT LES DEFORMEES STATIQUES
C ICOLC : NB DE CHOC A CONSIDERER
          N = ICOLC
          DO 80 K = 1,NEQ
            DO 82 IA = 1,ICOLC
             IF (ZR(JNORMX-1+IA).GT.EPS) THEN
              ZR(JA-1+K+NEQ*(IA-1)) =
     &             ZR(JRFIMP-1+K+NEQ*(IA-1))/SQRT(ZR(JNORMX-1+IA))
             ELSE
              ZR(JA-1+K+NEQ*(IA-1)) = 0.D0
             ENDIF
82          CONTINUE
80        CONTINUE
C
          CALL CALSVD(NM,M,N,ZR(JA),ZR(JW),
     &            MATUV,ZR(JU),MATUV,ZR(JV),IERR)
          IF ( IERR.NE.0 ) GO TO 9999
          MMAX = 0.D0
          MMIN = 1.D10
          DO 83 IA = 1,N
            MMAX = MAX(MMAX,ZR(JW-1+IA))
            MMIN = MIN(MMIN,ZR(JW-1+IA))
83        CONTINUE
C CONDITIONNEMENT
          IF ( MMIN .LE. EPS ) THEN
            VALR (1) = MMIN
            VALR (2) = EPS
            CALL U2MESR('I','SOUSTRUC_98',2,VALR)
            MMIN = EPS
          ENDIF
          SCOND = MMAX/MMIN
C
          VALR (1) = SCOND
          CALL U2MESR('I','SOUSTRUC_99',1,VALR)
          DO 51 JJ = 1,NBMODE
            ZR(JNORMY-1+JJ)=DDOT(NEQ,BMODAL(1,JJ),1,BMODAL(1,JJ),1)
51        CONTINUE
C
          N = ICOLC+1
          DO 42 J = 1,NBMODE
C
C LA MATRICE A CONTIENT LES DEFORMEES STATIQUES ET MODE
            DO 60 K = 1,NEQ
              DO 62 IA = 1,ICOLC
               IF (ZR(JNORMX-1+IA).GT.EPS) THEN
                ZR(JA-1+K+NEQ*(IA-1)) =
     &               ZR(JRFIMP-1+K+NEQ*(IA-1))/SQRT(ZR(JNORMX-1+IA))
               ELSE
                ZR(JA-1+K+NEQ*(IA-1)) = 0.D0
               ENDIF
62            CONTINUE
              ZR(JA-1+K+NEQ*(ICOLC+1-1)) =
     &               BMODAL(K,J)/SQRT(ZR(JNORMY-1+J))
60          CONTINUE
C
            CALL CALSVD(NM,M,N,ZR(JA),ZR(JW),
     &            MATUV,ZR(JU),MATUV,ZR(JV),IERR)
            IF ( IERR.NE.0 ) GO TO 9999
            MMAX = 0.D0
            MMIN = 1.D10
            DO 53 IA = 1,N
              MMAX = MAX(MMAX,ZR(JW-1+IA))
              MMIN = MIN(MMIN,ZR(JW-1+IA))
53          CONTINUE
C CONDITIONNEMENT
            IF ( MMIN .LE. EPS ) THEN
              VALI = J
              VALR (1) = MMIN
              VALR (2) = EPS
              CALL U2MESG('I','SOUSTRUC2_1',0,' ',1,VALI,2,VALR)
              MMIN = EPS
            ENDIF
            ZR(JEFLOC-1+J) = MMAX/MMIN
C NORMALISATION PAR RAPPORT DEF STATIQUE
            IF ( SCOND .LE. EPS ) SCOND = EPS
            ZR(JEFLOC-1+J) = ZR(JEFLOC-1+J)/SCOND
C
            INDIC(J)=J
 42       CONTINUE
C
C      ON ORDONNE SELON LA PARTICIPATION DECROISSANTE
          CALL MDTRIB (INDIC,ZR(JEFLOC),NBMODE)
          DO 72 J = 1,NBMODE
            VALI = INDIC(J)
            VALR (1) = ZR(JEFLOC-1+INDIC(J))
            CALL U2MESG('I','SOUSTRUC2_2',0,' ',1,VALI,1,VALR)
 72       CONTINUE
C
          CALL U2MESS('I','VIDE_1')
        ENDIF

C
      ENDIF
C
 9999 CONTINUE
C
C --- MENAGE
C
      CALL JEDETR('&&CRICHO.EFLOC')
      CALL JEDETR('&&CRICHO.RFIMPOX')
      CALL JEDETR('&&CRICHO.NORMXX')
      CALL JEDETR('&&CRICHO.NORMY')
      CALL JEDETR('&&CRICHO.A')
      CALL JEDETR('&&CRICHO.W')
      CALL JEDETR('&&CRICHO.U')
      CALL JEDETR('&&CRICHO.V')

      END
