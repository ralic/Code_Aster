      SUBROUTINE XMMAA1(NDIM  ,JNNE ,NDEPLE ,NNC   ,JNNM   ,
     &                NFAES ,CFACE ,HPG   ,FFC   ,FFE   ,
     &                FFM   ,JACOBI,JPCAI ,COEFCA,NORM  ,
     &                TYPMAI,NSINGE,NSINGM,RRE   ,RRM   ,
     &                NCONTA,JDDLE,JDDLM,MMAT  )

      IMPLICIT NONE
      INTEGER     NDIM,JNNE(3),JNNM(3)
      INTEGER     NSINGE,NSINGM,NCONTA
      INTEGER     NFAES,JPCAI,CFACE(5,3)
      REAL*8      MMAT(168,168),NORM(3)
      REAL*8      HPG,FFC(9),FFE(9),FFM(9),JACOBI
      REAL*8      COEFCA,RRE,RRM
      CHARACTER*8 TYPMAI
      INTEGER     NDEPLE,NNC,JDDLE(2),JDDLM(2)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER I,J,K,L,M,II,JJ,IN,PL,XOULA,JJN,IIN,NDDLE
      INTEGER NNE,NNES,NNM,NNMS,DDLES,DDLEM,DDLMS,DDLMM
      REAL*8  MM
C
C ----------------------------------------------------------------------
C
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
          IN=XOULA(CFACE,NFAES,I,JPCAI,TYPMAI,NCONTA)
          CALL XPLMA2(NDIM,NNE,NNES,DDLES,IN,PL)
          DO 30 J = 1,NDEPLE
C --- BLOCS ES:CONT, CONT:ES
            MM = HPG*FFC(I)*FFE(J)*JACOBI*NORM(K)
            CALL INDENT(J,DDLES,DDLEM,NNES,JJN)
            JJ = JJN+K
            MMAT(PL,JJ) = -MM
            MMAT(JJ,PL) = -MM
            JJ = JJ + NDIM
            MMAT(PL,JJ) =  MM
            MMAT(JJ,PL) =  MM
            DO 40 M = 1,NSINGE
              JJ = JJ + NDIM
              MMAT(PL,JJ) = RRE * MM
              MMAT(JJ,PL) = RRE * MM
   40       CONTINUE
   30     CONTINUE
          DO 50 J = 1,NNM
C --- BLOCS MA:CONT, CONT:MA
            MM = HPG*FFC(I)*FFM(J)*JACOBI*NORM(K)
            CALL INDENT(J,DDLMS,DDLMM,NNMS,JJN)
            JJ = NDDLE + JJN + K
            MMAT(PL,JJ) = MM
            MMAT(JJ,PL) = MM
            JJ = JJ + NDIM
            MMAT(PL,JJ) = MM
            MMAT(JJ,PL) = MM
            DO 60 M = 1,NSINGM
              JJ = JJ + NDIM
              MMAT(PL,JJ) = RRM * MM
              MMAT(JJ,PL) = RRM * MM
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
            DO 210 J = 1,NDEPLE
C --- BLOCS ES:ES
              MM = HPG*COEFCA*FFE(I)*NORM(L)*FFE(J)*JACOBI*NORM(K)
              CALL INDENT(I,DDLES,DDLEM,NNES,IIN)
              CALL INDENT(J,DDLES,DDLEM,NNES,JJN)
              II = IIN + L
              JJ = JJN + K
              MMAT(II,JJ) =  MM
              JJ = JJ + NDIM
              MMAT(II,JJ) = -MM
              MMAT(JJ,II) = -MM
              II = II + NDIM
              MMAT(II,JJ) =  MM
              DO 215 M = 1,NSINGE
                JJ = JJ + NDIM
                II = II - NDIM
                MMAT(II,JJ) = -RRE * MM
                MMAT(JJ,II) = -RRE * MM
                II = II + NDIM
                MMAT(II,JJ) =  RRE * MM
                MMAT(JJ,II) =  RRE * MM
                II = II + NDIM
                MMAT(II,JJ) =  RRE * RRE * MM
  215         CONTINUE
  210       CONTINUE
            DO 220 J = 1,NNM
C --- BLOCS ES:MA, MA:ES
              MM = HPG*COEFCA*FFE(I)*NORM(L)*FFM(J)*JACOBI*NORM(K)
              CALL INDENT(I,DDLES,DDLEM,NNES,IIN)
              CALL INDENT(J,DDLMS,DDLMM,NNMS,JJN)
              II = IIN + L
              JJ = NDDLE + JJN + K
              MMAT(II,JJ) = -MM
              MMAT(JJ,II) = -MM
              JJ = JJ + NDIM
              MMAT(II,JJ) = -MM
              MMAT(JJ,II) = -MM
              II = II + NDIM
              JJ = JJ - NDIM
              MMAT(II,JJ) =  MM
              MMAT(JJ,II) =  MM
              JJ = JJ + NDIM
              MMAT(II,JJ) =  MM
              MMAT(JJ,II) =  MM
              DO 230 M = 1,NSINGM
                II = II - NDIM
                JJ = JJ + NDIM
                MMAT(II,JJ) = -RRM * MM
                MMAT(JJ,II) = -RRM * MM
                II = II + NDIM
                MMAT(II,JJ) =  RRM * MM
                MMAT(JJ,II) =  RRM * MM
                JJ = JJ - NDIM
  230         CONTINUE
              DO 240 M = 1,NSINGE
                II = II + NDIM
                JJ = JJ - NDIM
                MMAT(II,JJ) =  RRE * MM
                MMAT(JJ,II) =  RRE * MM
                JJ = JJ + NDIM
                MMAT(II,JJ) =  RRE * MM
                MMAT(JJ,II) =  RRE * MM
                II = II - NDIM
  240         CONTINUE
              DO 250 M = 1,NSINGE*NSINGM
                II = II + NDIM
                JJ = JJ + NDIM
                MMAT(II,JJ) =  RRE * RRM * MM
                MMAT(JJ,II) =  RRE * RRM * MM
  250         CONTINUE
  220       CONTINUE
  200     CONTINUE

          DO 300 I = 1,NNM
            DO 320 J = 1,NNM
C --- BLOCS MA:MA
              MM = HPG*COEFCA*FFM(I)*NORM(L)*FFM(J)*JACOBI*NORM(K)
              CALL INDENT(I,DDLMS,DDLMM,NNMS,IIN)
              CALL INDENT(J,DDLMS,DDLMM,NNMS,JJN)
              II = NDDLE + IIN + L
              JJ = NDDLE + JJN + K

              MMAT(II,JJ) =  MM
              JJ = JJ + NDIM
              MMAT(II,JJ) =  MM
              MMAT(JJ,II) =  MM
              II = II + NDIM
              MMAT(II,JJ) =  MM
              DO 330 M = 1,NSINGM
                JJ = JJ + NDIM
                II = II - NDIM
                MMAT(II,JJ) =  RRM * MM
                MMAT(JJ,II) =  RRM * MM
                II = II + NDIM
                MMAT(II,JJ) =  RRM * MM
                MMAT(JJ,II) =  RRM * MM
                II = II + NDIM
                MMAT(II,JJ) =  RRM * RRM * MM
  330         CONTINUE
  320       CONTINUE
  300     CONTINUE
  110   CONTINUE
  100 CONTINUE
      ELSE
C
C --------------------- CALCUL DE [A] ----------------------------------
C
      DO 510 K = 1,NDIM
        DO 520 I = 1,NNC
          IN=XOULA(CFACE,NFAES,I,JPCAI,TYPMAI,NCONTA)
          CALL XPLMA2(NDIM,NNE,NNES,DDLES,IN,PL)
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
              MM = HPG*COEFCA*FFE(I)*NORM(L)*FFE(J)*JACOBI*NORM(K)
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
C
      END
