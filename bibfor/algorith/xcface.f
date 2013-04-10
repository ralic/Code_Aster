      SUBROUTINE XCFACE(ELREF,LSN,LST,JGRLSN,IGEOM,ENR,
     &                  NFISS,IFISS,FISCO,NFISC,NOMA,NMAABS,
     &                  PINTER,NINTER,AINTER,NFACE,NPTF,CFACE)
      IMPLICIT NONE

      INCLUDE 'jeveux.h'

      REAL*8        LSN(*),LST(*),PINTER(*),AINTER(*)
      INTEGER       JGRLSN,IGEOM,NINTER,NFACE,CFACE(5,3),NPTF
      INTEGER       NFISS,IFISS,FISCO(*),NFISC,NMAABS
      CHARACTER*8   ELREF,NOMA
      CHARACTER*16  ENR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/04/2013   AUTEUR JAUBERT A.JAUBERT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE GENIAUT S.GENIAUT
C TOLE CRS_1404
C                     TROUVER LES PTS D'INTERSECTION ENTRE LES ARETES
C                      ET LE PLAN DE FISSURE ET DÉCOUPAGE EN FACETTES
C
C     ENTREE
C       ELREF    : ELEMENT DE REFERENCE
C       LSN      : VALEURS DE LA LEVEL SET NORMALE
C       LST      : VALEURS DE LA LEVEL SET TANGENTE
C       JGRLSN   : ADRESSE DU GRADIENT DE LA LEVEL SET NORMALE
C       IGEOM    : ADRESSE DES COORDONNEES DES NOEUDS DE L'ELT PARENT
C       ENR      : VALEUR DE L'ATTRIBUT DE L'ELEMENT
C       NFISS    : NOMBRE DE FISSURES VUES DANS L'ÉLÉMENT
C       IFISS    : NUMÉRO DE LA FISSURE EN COURS
C       FISCO    : NUM ET COEF DES FISS SUR LESQUELLES IFISS SE BRANCHE
C       NFISC    : NOMBRE DE FISSURES SUR LESQUELLES IFISS SE BRANCHE
C       NOMA     : NOM DU MAILLAGE
C       NMAABS   : INDICE DE LA MAILLE
C
C     SORTIE

C       PINTER  : COORDONNEES DES POINTS D'INTERSECTION POUR IFISS
C       NINTER  : NOMBRE DE POINTS D'INTERSECTION POUR IFISS
C       AINTER  : INFOS ARETE ASSOCIEE AU POINTS D'INTERSECTI POUR IFISS
C       NFACE   : NOMBRE DE FACETTES POUR IFISS
C       NPTF    : NOMBRE DE "NOEUDS" DES FACETTES (SOMMETS ET MILIEUX)
C       CFACE   : CONNECTIVITE DES NOEUDS DES FACETTES POUR IFISS
C
C     ------------------------------------------------------------------
C
      REAL*8          A(3),B(3),C(3),LSNA,LSNB,PADIST,LONGAR,TAMPOR(5)
      REAL*8          ALPHA,BAR(3),OA(3),M(3),AM(3),ND(3),PS,PS1,LAMBDA
      REAL*8          H(3),OH(3),NOH,COS,NOA,TRIGOM,R3(3),THETA(6),EPS
      REAL*8          R8PI,DDOT,AB(2),LSTA,LSTB,LSTC,ABPRIM(2),PREC,PRE2
      REAL*8          LSJA(NFISC+1),LSJB(NFISC+1),LSJC,BETA,R8PREM
      REAL*8          R8MAEM,MINLSN
      INTEGER         J,AR(12,3),NBAR,NA,NB,NC,INS
      INTEGER         IA,I,IPT,IBID,PP,PD,NNO,K,NNOS
      INTEGER         IADZI,IAZK24,NDIM,PTMAX
      CHARACTER*8     TYPMA,ELP
      INTEGER         ZXAIN,XXMMVD
      LOGICAL         LCONT,LAJPA,LAJPB,LAJPC
C ----------------------------------------------------------------------

      CALL JEMARQ()

      EPS=-1.0D-10

C   PREC PERMET D"EVITER LES ERREURS DE PRÉCISION CONDUISANT
C   A IA=IN=0 POUR LES MAILLES DU FRONT
      PREC = 1000*R8PREM()
      MINLSN = 1*R8MAEM()

      ZXAIN = XXMMVD('ZXAIN')
      CALL ELREF1(ELP)
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,IBID,IBID,IBID,IBID,IBID)

      IF (NDIM .EQ. 3) THEN
         PTMAX=6
      ELSEIF (NDIM .EQ. 2) THEN
         PTMAX=2
      ENDIF
      LCONT = (ENR(3:3).EQ.'C').OR.(ENR(4:4).EQ.'C')
      PRE2 = 0
      IF (LCONT) PRE2 = PREC

C     1) RECHERCHE DES POINTS D'INTERSECTION
C     --------------------------------------

C     VECTEUR REEL À ZXAIN COMPOSANTES, POUR CHAQUE PT D'INTER :
C     - NUMÉRO ARETE CORRESPONDANTE         (0 SI C'EST UN NOEUD SOMMET)
C     - NUMÉRO NOEUD SI NOEUD SOMMET        (0 SINON)
C     - LONGUEUR DE L'ARETE
C     - POSITION DU PT SUR L'ARETE          (0 SI C'EST UN NOEUD SOMMET)
C     - ARETE VITALE                        (0 SI NON)

      CALL TECAEL(IADZI,IAZK24)
      TYPMA=ZK24(IAZK24-1+3+ZI(IADZI-1+2)+3)(1:8)

      IPT=0
C       COMPTEUR DE POINT INTERSECTION = NOEUD SOMMENT
      INS=0
      CALL CONARE(TYPMA,AR,NBAR)

C       BOUCLE SUR LES ARETES POUR DETERMINER LES POINTS D'INTERSECTION
      DO 100 IA=1,NBAR

C       NUM NO DE L'ELEMENT
        NA=AR(IA,1)
        NB=AR(IA,2)
        NC=IA
        LSNA=LSN((NA-1)*NFISS+IFISS)
        LSNB=LSN((NB-1)*NFISS+IFISS)
        IF (LSNA.LT.MINLSN) MINLSN=LSNA
        IF (LSNB.LT.MINLSN) MINLSN=LSNB
        IF ((LSNA*LSNB).LE.0.D0) THEN
          LSTA=LST((NA-1)*NFISS+IFISS)
          LSTB=LST((NB-1)*NFISS+IFISS)
          DO 110 I=1,NDIM
            A(I)=ZR(IGEOM-1+NDIM*(NA-1)+I)
            B(I)=ZR(IGEOM-1+NDIM*(NB-1)+I)
 110      CONTINUE
          IF (NDIM.LT.3) THEN
            A(3)=0.D0
            B(3)=0.D0
            C(3)=0.D0
          ENDIF
          LONGAR=PADIST(NDIM,A,B)
C
          LAJPA = .FALSE.
          LAJPB = .FALSE.
          LAJPC = .FALSE.
C
          IF (LSNA.EQ.0.D0.AND.LSTA.LE.PRE2) THEN
C           ON AJOUTE A LA LISTE LE POINT A
            LAJPA = .TRUE.
            IF (LCONT.AND.LSTA.GE.0.D0) NA=0
          ENDIF
          IF (LSNB.EQ.0.D0.AND.LSTB.LE.PRE2) THEN
C           ON AJOUTE A LA LISTE LE POINT B
            LAJPB = .TRUE.
            IF (LCONT.AND.LSTB.GE.0.D0) NB=0
          ENDIF
          IF (LSNA.NE.0.D0.AND.LSNB.NE.0.D0) THEN
            BETA = LSNA/(LSNB-LSNA)
            DO 120 I=1,NDIM
               C(I)=A(I)-BETA*(B(I)-A(I))
 120        CONTINUE
C           POSITION DU PT D'INTERSECTION SUR L'ARETE
            ALPHA=PADIST(NDIM,A,C)
            LSTC=LSTA-BETA*(LSTB-LSTA)
            IF (LSTC.LE.PREC) THEN
              LAJPC = .TRUE.
              IF (LCONT.AND.LSTC.GE.0.D0) NC = 0
            ENDIF
          ENDIF
C         MODIFICATION EN TENANT COMPTE DE LA LEVEL SET JONCTION
          IF (NFISC.GT.0) THEN
C           POUR LES FISSURES SUR LESQUELLES IFISS SE BRANCHE
            CALL ASSERT(NA.GT.0.AND.NB.GT.0.AND.NC.GT.0)
            DO 130 J=1,NFISC
              LSJA(J)=LSN((NA-1)*NFISS+FISCO(2*J-1))*FISCO(2*J)
              LSJB(J)=LSN((NB-1)*NFISS+FISCO(2*J-1))*FISCO(2*J)
 130        CONTINUE
            DO 140 J=1,NFISC
              IF (LAJPA) THEN
                IF (LSJA(J).GT.PRE2) LAJPA = .FALSE.
                IF (LCONT.AND.LSJA(J).GE.0) NA = 0
              ENDIF
              IF (LAJPB) THEN
                IF (LSJB(J).GT.PRE2) LAJPB = .FALSE.
                IF (LCONT.AND.LSJB(J).GE.0) NB = 0
              ENDIF
              IF (LAJPC) THEN
                LSJC=LSJA(J)-BETA*(LSJB(J)-LSJA(J))
                IF (LSJC.GT.PREC) LAJPC = .FALSE.
                IF (LCONT.AND.LSJC.GE.0) NC = 0
              ENDIF
 140        CONTINUE
          ENDIF
          DO 150 J = NFISC+1,NFISS
C           POUR LES FISSURES QUI SE BRANCHENT SUR IFISS
            K = FISCO(2*J-1)
            IF (K.GT.0) THEN
              LSJA(1) = LSN((NA-1)*NFISS+K)
              LSJB(1) = LSN((NB-1)*NFISS+K)
              IF (LAJPA.AND.ABS(LSJA(1)).LT.1D-12) THEN
                NA = 0
              ENDIF
              IF (LAJPB.AND.ABS(LSJB(1)).LT.1D-12) THEN
                NB = 0
              ENDIF
              IF (LAJPC) THEN
                LSJC=LSJA(1)-BETA*(LSJB(1)-LSJA(1))
C             ON RETIENT LE NUM D'ARETE AVEC LE SIGNE -
C             POUR REPÉRER L'ARETE DANS XAINT2
                IF (ABS(LSJC).LT.1D-12) NC = -ABS(NC)
              ENDIF
            ENDIF
 150      CONTINUE
C
          IF (LAJPA) CALL XAJPIN(NDIM,PINTER,PTMAX,IPT,INS,A,LONGAR,
     &                           AINTER,0,NA,0.D0)
          IF (LAJPB) CALL XAJPIN(NDIM,PINTER,PTMAX,IPT,INS,B,LONGAR,
     &                           AINTER,0,NB,0.D0)
          IF (LAJPC) CALL XAJPIN(NDIM,PINTER,PTMAX,IPT,IBID,C,LONGAR,
     &                           AINTER,NC,0,ALPHA)
        ENDIF

 100  CONTINUE

C     RECHERCHE SPECIFIQUE POUR LES ELEMENTS INTERSECTÉES
      IF (NFISC.GT.0) THEN
        CALL XCFACJ(PINTER,PTMAX,IPT,AINTER,LSN,IGEOM,NNO,NDIM,
     &               NFISS,IFISS,FISCO,NFISC,TYPMA)
      ENDIF
C     RECHERCHE SPECIFIQUE POUR LES ELEMENTS EN FOND DE FISSURE
      IF (ENR(2:2).EQ.'T'.OR.ENR(3:3).EQ.'T') THEN

C       ON A DROIT A 1 POINT EN PLUS   
        CALL XCFACF(PINTER,PTMAX+1,IPT,AINTER,LSN,LST,IGEOM,NNO,NDIM,
     &                                        TYPMA,NOMA,NMAABS)
      ENDIF
      NINTER=IPT

C     2) DECOUPAGE EN FACETTES TRIANGULAIRES DE LA SURFACE DEFINIE
C     ------------------------------------------------------------

C                  (BOOK IV 09/09/04)

C     CAS 3D
      IF (NDIM .EQ. 3) THEN
        IF (NINTER.LT.3) GOTO 500
        
        DO 200 I=1,5
         DO 201 J=1,3
           CFACE(I,J)=0
 201     CONTINUE
 200    CONTINUE

C       NORMALE A LA FISSURE (MOYENNE DE LA NORMALE AUX NOEUDS)
        CALL VECINI(3,0.D0,ND)
        DO 210 I=1,NNO
         DO 211 J=1,3
           ND(J)=ND(J)+ZR(JGRLSN-1+3*(NFISS*(I-1)+IFISS-1)+J)/NNO
 211     CONTINUE
 210    CONTINUE

C       PROJECTION ET NUMEROTATION DES POINTS COMME DANS XORIFF
        CALL VECINI(3,0.D0,BAR)
        DO 220 I=1,NINTER
         DO 221 J=1,3
           BAR(J)=BAR(J)+PINTER((I-1)*3+J)/NINTER
 221     CONTINUE
 220    CONTINUE
        DO 230 J=1,3
         A(J)=PINTER((1-1)*3+J)
         OA(J)=A(J)-BAR(J)
 230    CONTINUE
        NOA=SQRT(OA(1)*OA(1) + OA(2)*OA(2)  +  OA(3)*OA(3))

C       BOUCLE SUR LES POINTS D'INTERSECTION POUR CALCULER L'ANGLE THETA
        DO 240 I=1,NINTER
         DO 241 J=1,3
           M(J)=PINTER((I-1)*3+J)
           AM(J)=M(J)-A(J)
 241     CONTINUE
         PS=DDOT(3,AM,1,ND,1)

         PS1=DDOT(3,ND,1,ND,1)
         LAMBDA=-PS/PS1
         DO 242 J=1,3
           H(J)=M(J)+LAMBDA*ND(J)
           OH(J)=H(J)-BAR(J)
 242     CONTINUE
         PS=DDOT(3,OA,1,OH,1)

         NOH=SQRT(OH(1)*OH(1) + OH(2)*OH(2)  +  OH(3)*OH(3))
         COS=PS/(NOA*NOH)

         THETA(I)=TRIGOM('ACOS',COS)
C        SIGNE DE THETA (06/01/2004)
         CALL PROVEC(OA,OH,R3)
         PS=DDOT(3,R3,1,ND,1)
         IF (PS.LT.EPS) THETA(I) = -1 * THETA(I) + 2 * R8PI()

 240    CONTINUE

C       TRI SUIVANT THETA CROISSANT
        DO 250 PD=1,NINTER-1
         PP=PD
         DO 251 I=PP,NINTER
           IF (THETA(I).LT.THETA(PP)) PP=I
 251     CONTINUE
         TAMPOR(1)=THETA(PP)
         THETA(PP)=THETA(PD)
         THETA(PD)=TAMPOR(1)
         DO 252 K=1,3
           TAMPOR(K)=PINTER(3*(PP-1)+K)
           PINTER(3*(PP-1)+K)=PINTER(3*(PD-1)+K)
           PINTER(3*(PD-1)+K)=TAMPOR(K)
 252     CONTINUE
         DO 253 K=1,ZXAIN
           TAMPOR(K)=AINTER(ZXAIN*(PP-1)+K)
           AINTER(ZXAIN*(PP-1)+K)=AINTER(ZXAIN*(PD-1)+K)
           AINTER(ZXAIN*(PD-1)+K)=TAMPOR(K)
 253     CONTINUE
 250    CONTINUE

 500    CONTINUE

C       NOMBRE DE POINTS D'INTERSECTION IMPOSSIBLE.
C       NORMALEMENT, ON A DEJE FAIT LA VERIF DANS XAJPIN
C       CEINTURE ET BRETELLE
        CALL ASSERT(NINTER.LE.7)

        IF (NINTER.EQ.7) THEN
          NFACE=5
          NPTF=3
          CFACE(1,1)=1
          CFACE(1,2)=2
          CFACE(1,3)=3
          CFACE(2,1)=1
          CFACE(2,2)=3
          CFACE(2,3)=5
          CFACE(3,1)=3
          CFACE(3,2)=4
          CFACE(3,3)=5
          CFACE(4,1)=1
          CFACE(4,2)=5
          CFACE(4,3)=7
          CFACE(5,1)=5
          CFACE(5,2)=6
          CFACE(5,3)=7
        ELSEIF (NINTER.EQ.6) THEN
          NFACE=4
          NPTF=3
          CFACE(1,1)=1
          CFACE(1,2)=2
          CFACE(1,3)=3
          CFACE(2,1)=1
          CFACE(2,2)=3
          CFACE(2,3)=5
          CFACE(3,1)=1
          CFACE(3,2)=5
          CFACE(3,3)=6
          CFACE(4,1)=3
          CFACE(4,2)=4
          CFACE(4,3)=5
        ELSEIF (NINTER.EQ.5) THEN
          NFACE=3
          NPTF=3
          CFACE(1,1)=1
          CFACE(1,2)=2
          CFACE(1,3)=3
          CFACE(2,1)=1
          CFACE(2,2)=3
          CFACE(2,3)=4
          CFACE(3,1)=1
          CFACE(3,2)=4
          CFACE(3,3)=5
        ELSEIF (NINTER.EQ.4) THEN
          NFACE=2
          NPTF=3
          CFACE(1,1)=1
          CFACE(1,2)=2
          CFACE(1,3)=3
          CFACE(2,1)=1
          CFACE(2,2)=3
          CFACE(2,3)=4
        ELSEIF (NINTER.EQ.3) THEN
          NFACE=1
          NPTF=3
          CFACE(1,1)=1
          CFACE(1,2)=2
          CFACE(1,3)=3
        ELSE
          NPTF=0
          NFACE=0
        ENDIF

C     CAS 2D
      ELSEIF (NDIM .EQ. 2) THEN

        DO 800 I=1,5
          DO 801 J=1,3
            CFACE(I,J)=0
 801      CONTINUE
 800    CONTINUE
        IF (NINTER .EQ. 2) THEN
C         NORMALE A LA FISSURE (MOYENNE DE LA NORMALE AUX NOEUDS)
          CALL VECINI(2,0.D0,ND)
          DO 810 I=1,NNO
            DO 811 J=1,2
           ND(J)=ND(J)+ZR(JGRLSN-1+2*(NFISS*(I-1)+IFISS-1)+J)/NNO
 811        CONTINUE
 810      CONTINUE

          DO 841 J=1,2
            A(J)=PINTER(J)
            B(J)=PINTER(2+J)
            AB(J)=B(J)-A(J)
 841      CONTINUE

          ABPRIM(1)=-AB(2)
          ABPRIM(2)=AB(1)

          IF (DDOT(2,ABPRIM,1,ND,1) .LT. 0.D0) THEN
            DO 852 K=1,2
              TAMPOR(K)=PINTER(K)
              PINTER(K)=PINTER(2+K)
              PINTER(2+K)=TAMPOR(K)
 852        CONTINUE
            DO 853 K=1,4
              TAMPOR(K)=AINTER(K)
              AINTER(K)=AINTER(ZXAIN+K)
              AINTER(ZXAIN+K)=TAMPOR(K)
 853        CONTINUE
          ENDIF
          NFACE=1
          NPTF=2
          CFACE(1,1)=1
          CFACE(1,2)=2
        ELSE
          NPTF=0
          NFACE=0
        ENDIF

      ELSE
C       PROBLEME DE DIMENSION : NI 2D, NI 3D
        CALL ASSERT(NDIM.EQ.2 .OR. NDIM.EQ.3)
      ENDIF
      IF (NFISS.GT.1.AND.MINLSN.EQ.0) NFACE = 0
      IF (NFACE.EQ.0) NINTER = 0
      CALL JEDEMA()
      END
