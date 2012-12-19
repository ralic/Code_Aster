        SUBROUTINE XMMAA1(NDIM  ,JNNE ,NDEPLE ,NNC   ,JNNM   ,
     &                NFAES ,CFACE ,HPG   ,FFC   ,FFE   ,
     &                FFM   ,JACOBI,JPCAI ,COEFCR,COEFCP,
     &                LPENAC,NORM  ,TYPMAI,NSINGE,
     &                NSINGM,RRE   ,RRM   ,NCONTA,JDDLE,
     &                JDDLM,NFHE,NFHM,LMULTI,HEAVNO,HEAVFA,MMAT  )
      IMPLICIT NONE
      INTEGER     NDIM,JNNE(3),JNNM(3)
      INTEGER     NSINGE,NSINGM,NCONTA
      INTEGER     NFAES,JPCAI,CFACE(5,3),NFHE,NFHM,HEAVNO(8),HEAVFA(*)
      REAL*8      MMAT(336,336),NORM(3)
      REAL*8      HPG,FFC(8),FFE(20),FFM(20),JACOBI
      REAL*8      COEFCR,COEFCP,RRE,RRM
      CHARACTER*8 TYPMAI
      INTEGER     NDEPLE,NNC,JDDLE(2),JDDLM(2)
      LOGICAL     LPENAC,LMULTI

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_21
C
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
C
C CALCUL DE A ET DE AT
C
C
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
C TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C ----------------------------------------------------------------------
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  JNNE   : MAILLE ESCL : (1) NB NDS
C                            (2) NB NDS SOMMETS
C                            (3) NB NDS MILIEU
C OUT NDEPLE : NOMBRE DE NOEUDS ESCL POSSEDANT DES DDLS DE DEPLACEMENT
C IN  NNC    : NOMBRE DE NOUEDS DE CONTACT
C IN  JNNM   : MAILLE MAIT : (1) NB NDS
C                            (2) NB NDS SOMMETS
C                            (3) NB NDS MILIEU
C IN  NFAES  : NUMERO DE LA FACETTE DE CONTACT ESCLAVE
C IN  CFACE  : MATRICE DE CONECTIVITE DES FACETTES DE CONTACT
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFC    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ELC
C IN  FFE    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ESC
C IN  FFM    : FONCTIONS DE FORME DE LA PROJECTION DU PTC DANS MAIT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  JPCAI  : POINTEUR VERS LE VECTEUR DES ARRETES ESCLAVES
C              INTERSECTEES
C IN  COEFCA : COEF_REGU_CONT
C IN  NORM   : VALEUR DE LA NORMALE AU POINT DE CONTACT
C IN  MAILLE : NOM DE LA MAILLE ESCLAVE D'ORIGINE (QUADRATIQUE)
C IN  NSINGE : NOMBRE DE FONCTION SINGULIERE ESCLAVE
C IN  NSINGM : NOMBRE DE FONCTION SINGULIERE MAITRE
C IN  RRE    : SQRT LST ESCLAVE
C IN  RRM    : SQRT LST MAITRE
C IN  NCONTA : TYPE DE CONTACT (1=P1P1, 2=P1P1A, 3=P2P1)
C IN  JDDLE  : MAILLE ESCL : (1) DDLS D'UN NOEUD SOMMET
C                            (2) DDLS D'UN NOEUD MILIEU
C IN  JDDLM  : MAILLE MAIT : (1) DDLS D'UN NOEUD SOMMET
C                            (2) DDLS D'UN NOEUD MILIEU
C I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
C
C ----------------------------------------------------------------------
C
      INTEGER I,J,K,L,II,JJ,IN,PL,XOULA,JJN,IIN,NDDLE
      INTEGER NNE,NNES,NNM,NNMS,DDLES,DDLEM,DDLMS,DDLMM
      INTEGER INI,INJ,PLI,PLJ,IFH,IDDL,JDDL
      REAL*8  MM,IESCL(6),JESCL(6),IMAIT(6),JMAIT(6)
C
C ----------------------------------------------------------------------
C
C
C --- INITIALISATION
C
      IESCL(1) = 1
      IESCL(2) =-1
      IESCL(2+NFHE)=-RRE
      JESCL(1) = 1
      JESCL(2) =-1
      JESCL(2+NFHE)=-RRE
      IMAIT(1) = 1
      IMAIT(2) = 1
      IMAIT(2+NFHM)= RRM
      JMAIT(1) = 1
      JMAIT(2) = 1
      JMAIT(2+NFHM)= RRM

C --------------------- CALCUL DE [A] ----------------------------------
C
      NNE=JNNE(1)
      NNES=JNNE(2)
      NNM=JNNM(1)
      NNMS=JNNM(2)
      DDLES=JDDLE(1)
      DDLEM=JDDLE(2)
      DDLMS=JDDLM(1)
      DDLMM=JDDLM(2)
      NDDLE = DDLES*NNES+DDLEM*(NNE-NNES)
C
      IF (NNM.NE.0) THEN
      DO 10 K = 1,NDIM
        DO 20 I = 1,NNC
          CALL XPLMA2(NDIM,NNE,NNES,DDLES,I,NFHE,PL)
          IF (LMULTI) PL = PL + (HEAVNO(I)-1)*NDIM
          DO 30 J = 1,NDEPLE
            MM = HPG*FFC(I)*FFE(J)*JACOBI*NORM(K)
            CALL INDENT(J,DDLES,DDLEM,NNES,JJN)
            IF (LMULTI) THEN
              DO 35 IFH = 1,NFHE
                JESCL(1+IFH)=HEAVFA(NFHE*(J-1)+IFH)
   35         CONTINUE
            ENDIF
            DO 40 JDDL=1,1+NFHE+NSINGE
              JJ = JJN + (JDDL-1)*NDIM + K
              MMAT(PL,JJ) = -JESCL(JDDL)*MM
              MMAT(JJ,PL) = -JESCL(JDDL)*MM
   40       CONTINUE
   30     CONTINUE
          DO 50 J = 1,NNM
            MM = HPG*FFC(I)*FFM(J)*JACOBI*NORM(K)
            CALL INDENT(J,DDLMS,DDLMM,NNMS,JJN)
            JJN = JJN + NDDLE
            IF (LMULTI) THEN
              DO 55 IFH = 1,NFHM
                JMAIT(1+IFH)=HEAVFA(NFHE*NDEPLE+NFHM*(J-1)+IFH)
   55         CONTINUE
            ENDIF
            DO 60 JDDL=1,1+NFHM+NSINGM
              JJ = JJN + (JDDL-1)*NDIM + K
              MMAT(PL,JJ) = JMAIT(JDDL)*MM
              MMAT(JJ,PL) = JMAIT(JDDL)*MM
   60       CONTINUE
   50     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
C --------------------- CALCUL DE [AU]----------------------------------
C
      DO 100 K = 1,NDIM
        DO 110 L = 1,NDIM
          DO 200 I = 1,NDEPLE
            CALL INDENT(I,DDLES,DDLEM,NNES,IIN)
            DO 210 J = 1,NDEPLE
              CALL INDENT(J,DDLES,DDLEM,NNES,JJN)
              IF(LPENAC) THEN
                MM = 0.D0
              ELSE
                MM = HPG*COEFCR*FFE(I)*NORM(L)*FFE(J)*JACOBI*NORM(K)
              ENDIF
              IF (LMULTI) THEN
                DO 220 IFH = 1,NFHE
                  IESCL(1+IFH)=HEAVFA(NFHE*(I-1)+IFH)
                  JESCL(1+IFH)=HEAVFA(NFHE*(J-1)+IFH)
  220           CONTINUE
              ENDIF
              DO 230 IDDL=1,1+NFHE+NSINGE
                II = IIN + (IDDL-1)*NDIM + L
                DO 240 JDDL=1,1+NFHE+NSINGE
                  JJ = JJN + (JDDL-1)*NDIM + K
                  MMAT(II,JJ) = IESCL(IDDL)*JESCL(JDDL)*MM
  240           CONTINUE
  230         CONTINUE
  210       CONTINUE
            DO 250 J = 1,NNM
              CALL INDENT(J,DDLMS,DDLMM,NNMS,JJN)
              JJN = JJN + NDDLE
              IF(LPENAC) THEN
                MM = 0.D0
              ELSE
                MM = HPG*COEFCR*FFE(I)*NORM(L)*FFM(J)*JACOBI*NORM(K)
              ENDIF
              IF (LMULTI) THEN
                DO 260 IFH = 1,NFHE
                  IESCL(1+IFH)=HEAVFA(NFHE*(I-1)+IFH)
  260           CONTINUE
                DO 270 IFH = 1,NFHM
                  JMAIT(1+IFH)=HEAVFA(NFHE*NDEPLE+NFHM*(J-1)+IFH)
  270           CONTINUE
              ENDIF
              DO 280 IDDL=1,1+NFHE+NSINGE
                II = IIN + (IDDL-1)*NDIM + L
                DO 290 JDDL=1,1+NFHM+NSINGM
                  JJ = JJN + (JDDL-1)*NDIM + K
                  MMAT(II,JJ) = -IESCL(IDDL)*JMAIT(JDDL)*MM
                  MMAT(JJ,II) = -IESCL(IDDL)*JMAIT(JDDL)*MM
  290           CONTINUE
  280         CONTINUE
  250       CONTINUE
  200     CONTINUE
          DO 300 I = 1,NNM
            CALL INDENT(I,DDLMS,DDLMM,NNMS,IIN)
            IIN = IIN + NDDLE
            DO 320 J = 1,NNM
              CALL INDENT(J,DDLMS,DDLMM,NNMS,JJN)
              JJN = JJN + NDDLE
              IF(LPENAC) THEN
                MM = 0.D0
              ELSE
                MM = HPG*COEFCR*FFM(I)*NORM(L)*FFM(J)*JACOBI*NORM(K)
              ENDIF
              IF (LMULTI) THEN
                DO 330 IFH = 1,NFHM
                  IMAIT(1+IFH)=HEAVFA(NFHE*NDEPLE+NFHM*(I-1)+IFH)
                  JMAIT(1+IFH)=HEAVFA(NFHE*NDEPLE+NFHM*(J-1)+IFH)
  330           CONTINUE
              ENDIF
              DO 340 IDDL=1,1+NFHM+NSINGM
                II = IIN + (IDDL-1)*NDIM + L
                DO 350 JDDL=1,1+NFHM+NSINGM
                  JJ = JJN + (JDDL-1)*NDIM + K
                  MMAT(II,JJ) = IMAIT(IDDL)*JMAIT(JDDL)*MM
  350           CONTINUE
  340         CONTINUE
  320       CONTINUE
  300     CONTINUE
C
  110   CONTINUE
  100 CONTINUE
      ELSE
C
C --------------------- CALCUL DE [A] ----------------------------------
C
      DO 510 K = 1,NDIM
        DO 520 I = 1,NNC
          CALL XPLMA2(NDIM,NNE,NNES,DDLES,I,NFHE,PL)
          IF (LMULTI) PL = PL + (HEAVNO(I)-1)*NDIM
          DO 530 J = 1,NDEPLE
C --- BLOCS ES:CONT, CONT:ES
            CALL INDENT(J,DDLES,DDLEM,NNES,JJN)
            JJ = JJN + K
            MM = HPG*FFC(I)*FFE(J)*JACOBI*NORM(K)
            MMAT(PL,JJ) = RRE * MM
            MMAT(JJ,PL) = RRE * MM
  530     CONTINUE
  520   CONTINUE
  510 CONTINUE
C
C --------------------- CALCUL DE [AU]----------------------------------
C
      DO 600 K = 1,NDIM
        DO 610 L = 1,NDIM
          DO 620 I = 1,NDEPLE
            DO 630 J = 1,NDEPLE
C --- BLOCS ES:ES
              IF(LPENAC) THEN
                MM = 0.D0
              ELSE
                MM = HPG*COEFCR*FFE(I)*NORM(L)*FFE(J)*JACOBI*NORM(K)
              ENDIF
              CALL INDENT(I,DDLES,DDLEM,NNES,IIN)
              CALL INDENT(J,DDLES,DDLEM,NNES,JJN)
              II = IIN + L
              JJ = JJN + K
              MMAT(II,JJ) =  RRE * RRE * MM
  630       CONTINUE
  620     CONTINUE
  610   CONTINUE
  600 CONTINUE
      ENDIF
C --------------------- CALCUL DE [C] ----------------------------------
C
C-------------- SEULEUMENT EN METHODE PENALISEE ------------------------

      IF(LPENAC)THEN
        DO 710 I = 1,NNC
          DO 720 J = 1,NNC
            CALL XPLMA2(NDIM,NNE,NNES,DDLES,I,NFHE,PLI)
            CALL XPLMA2(NDIM,NNE,NNES,DDLES,J,NFHE,PLJ)
            MMAT(PLI,PLJ) = -HPG*FFC(J)*FFC(I)*JACOBI/COEFCP
 720      CONTINUE
 710    CONTINUE
      ENDIF
C
      END
