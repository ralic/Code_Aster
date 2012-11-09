      SUBROUTINE ACGRDO(JVECTN, JVECTU, JVECTV, NBORDR,ORDINI, KWORK,
     &                  SOMPGW, JRWORK, TSPAQ, IPG, JVECPG,JDTAUM,
     &                  JRESUN,NOMMET,NOMMAT,NOMCRI,VALA,COEFPA,
     &                  NOMFOR,GRDVIE,FORVIE,POST,VALPAR, VRESU)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      INTEGER    JVECTN, JVECTU, JVECTV, NBORDR, KWORK
      INTEGER    SOMPGW, JRWORK, TSPAQ, IPG, JVECPG, JDTAUM,JRESUN
      CHARACTER*16  NOMMET, NOMCRI, NOMFOR,FORVIE
      CHARACTER*8   NOMMAT,GRDVIE
      LOGICAL    POST
      REAL*8     VRESU(24),VALPAR(22),VALA,COEFPA

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRP_20 CRP_21 CRS_512 CRS_1404
C ---------------------------------------------------------------------
C BUT: POUR LA FATIGUE A AMPLITUDE CONSTANTE
C      CALCULER DES GRANDEURS SERVANT A EVALUER LES CRITERES D'AMORCAGE
C      ET CALCULER LA GRANDEUR EQUIVALENT
C
C REMARQUE: CETTE SUBROUTINE EST APPLICABLE POUR UN NOEUD OU IPG EGALE
C           A 1 ET SOMPGW = SOMNOW,JVECPG = JVECNO
C ---------------------------------------------------------------------
C ARGUMENTS :
C     JVECTN  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS NORMAUX.
C     JVECTU  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS u DU PLAN DE CISAILLEMENT.
C     JVECTV  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS v DU PLAN DE CISAILLEMENT.
C     NBORDR  : IN  : NOMBRE DE NUMEROS D'ORDRE.
C     ORDINI     IN    I  : ORDRE INITIAL POUR LE CHARGEMENT CYCLIQUE
C     KWORK   : IN  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET DE
C                               MAILLES ;
C                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
C                               MAILLES.
C     SOMPGW  : IN  : SOMME DES POINTS DE GAUSS DES N MAILLES PRECEDANT
C                     LA MAILLE COURANTE.
C     JRWORK  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                     DU <<PAQUET>> DE MAILLES.
C     TSPAQ   : IN  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                     COURANT.
C     IPG     : IN  : IEME POINT DE GAUSS.
C     JVECPG  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                     LES COMPOSANTES u ET v DU VECTEUR TAU
C                     (CISAILLEMENT), POUR TOUS LES NUMEROS
C                     D'ORDRE DE CHAQUE VECTEUR NORMAL.
C    JDTAU      IN    ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                     LES VALEURS DE DELTA_TAU_MAX POUR CHAQUE VECTEUR.
C    JVECN      IN    ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                     LA VALEUR DU POINTEUR PERMETTANT D'ACCEDER AU
C                     VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX.
C    NOMMET     IN    NOM DE METHOD D'APPROCHEMENT DE CERCLE ("CERCLE
C                     EXACT" ET "CERCLE APPROCHE")
C    VALA       IN    VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
C    COEFPA     IN    COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
C    VRESU      OUT   TABLEAU DES RESULTATS (GRANDEURS ET DOMMAGE).
C                     POUR L'INSTANT, LA DIMENSION DE VRESU EST 24
C ---------------------------------------------------------------------
C23456

      REAL*8     RESUPC(24), GRDEQ(2), DTAUM(2), NXM(2), NYM(2), COEPRE
      REAL*8     NZM(2), NRUPT(2), DOM(2), SIGEQ(2)
      REAL*8     NORMAX(2), NORMOY(2),EPNMAX(2), EPNMOY(2), VALPU(22)
      REAL*8     PHYDRO, PHYDRM, SIGM(NBORDR*6)
      REAL*8     SIG(6),EPS(6), EPSE(6), EPSP(6),VEPSP(6)
      REAL*8     EPSL(6), EPSEL(6), EPSPL(6),EQEPSP, JACAUX(3)
      REAL*8     VSIG(6), SIGL(6), EQSIG, VSIGE, EQUI(17)
      REAL*8     R8MAEM, PHYMIN, C1, C2, RBID(6), VEPSPE , LCIV2E
      REAL*8     NM1X, NM1Y, NM1Z, BR(6),VECPRO(3,3),VALPRO(3)
      REAL*8     EPRMAX, EPRMIN, SIGNM1, TOL, TOLDYN, AR(6)
      REAL*8     FXM, FYM, FZM, SINM1M, SOMDEF
      REAL*8     DEVSIG(6), DVEPSE(6), DENDIS, DSIGL(6), RAYSPH
      REAL*8     DEPSL(6), SOMDEN,DENDIE, ETREMA, ETREMI
      REAL*8     SIGMAX, EXM, EYM, EZM,EPSNM1,EPNM1M, SIGMIN
      REAL*8     STREMA, STREMI, VEPS(6), EQEPS,VEPST , EPSPAC
      INTEGER    NPERM,ITYPE,IORDRE, NVP, NITJAC, ORDINI, ORDFIN
      INTEGER    I,J,K,L,IBID,JPROF, NPARMA, NP,ICODRE,ADR,IRET,IPAR
      INTEGER    DECAL, PARACT(30), ADRL, IARG, NBF, NBTOT
      CHARACTER*24 CHNOM, CBID
      CHARACTER*16 PHENOM, TYPCHA
      CHARACTER*8  NOMPF(22), NOMPAR(22), NOMGRD
      LOGICAL      ENDUR, PLCICR, LBID
C-----------------------------------------------------------------------
C       DATA  LSIG/ 'SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ', 'SIYZ' /
C C
C       DATA  LEPS/ 'EPXX', 'EPYY', 'EPZZ', 'EPXY', 'EPXZ', 'EPYZ' /
C C
C       DATA  LGRD/  'DTAUM1', 'VNM1X', 'VNM1Y', 'VNM1Z', 'SINMAX1',
C      &             'SINMOY', 'EPNMAX', 'EPNMOY', 'SIGEQ1', 'NBRUP1',
C      &             'ENDO1', 'DTAUM2', 'VNM2X', 'VNM2Y', 'VNM2Z',
C      &        'SINMAX2', 'SINMOY2', 'EPNMAX2', 'EPNMOY2', 'SIGEQ2',
C      &             'NBRUP2', 'ENDO2' ,'VMIS', 'TRESCA' /

C     ---------------------------------------------------------------
        DATA  NOMPAR/   'DTAUMA', 'PHYDRM', 'NORMAX', 'NORMOY',
     &                  'EPNMAX', 'EPNMOY', 'DEPSPE', 'EPSPR1',
     &                  'SIGNM1', 'DENDIS', 'DENDIE', 'APHYDR',
     &                  'MPHYDR', 'DSIGEQ', 'SIGPR1', 'EPSNM1',
     &                  'INVA2S', 'DSITRE', 'DEPTRE', 'EPSPAC',
     &                  'RAYSPH', 'AMPCIS'   /
C     ---------------------------------------------------------------

C -------------------------------------------------------------------
C
C RECUPERER LA LISTE DE GRANDEURS ACTIVES

        TYPCHA = 'PERIODIQUE'

C  INITIALISER

        CALL ANACRI( NOMCRI,NOMFOR,TYPCHA,'NON', PARACT,
     &   LBID, LBID, LBID, LBID, LBID)

C ------------------------------------------------------------------
C---  CALCULER DES GRANDEURS ACTIVES
C-------------------------------------------------------------------
C INITIALISATION
      PLCICR = .FALSE.
      PHYDRM = 0.D0
      PHYMIN = 0.D0
      PHYDRO = 0.D0
      VEPSPE = 0.D0
      VSIGE = 0.D0
      EPRMAX = 0.D0
      EPRMIN = R8MAEM()
      NM1X = 0.D0
      NM1Y = 0.D0
      NM1Z = 0.D0
      SIGNM1 = 0.D0
      SINM1M = 0.D0
      DENDIS = 0.D0
      DENDIE = 0.D0
      ETREMA = 0.D0
      ETREMI = 0.D0
      STREMA = 0.D0
      STREMI = 0.D0
      EPNM1M = 0.D0
      SIGMAX = 0.D0
      SIGMIN = R8MAEM()
      VEPST  = 0.D0
      EPSPAC = 0.D0
      RAYSPH = 0.D0
      DOM(1) = 0.D0
      DOM(2) = 0.D0
      NRUPT(1) = 1.D7
      NRUPT(2) = 1.D7

      DO 20 I = 1, 24
         RESUPC(I) = 0.0D0
20    CONTINUE

C       DO 2100 I = 1, 22
C          VALPAR(I) = 0.0D0
C C         VALPU(I) = 0.0D0
C 2100  CONTINUE

      DO 35 I = 1, 6
         SIG(I) = 0.0D0
         EPS(I) = 0.0D0
         EPSE(I)= 0.0D0
         EPSP(I)= 0.0D0
         VEPSP(I)= 0.0D0
         VSIG(I)= 0.0D0
         EPSL(I)= 0.0D0
         EPSEL(I)= 0.0D0
         EPSPL(I)= 0.0D0
         SIGL(I) = 0.0D0
35    CONTINUE

C FIN D'INITILISATION
C
C ---------------------------------------------------------------
C RECUPER LES CONTRAINTES ET DEFORMATION
C       CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'E       ',
C      &            VALE,ICODRE,0)
C       IF (ICODRE .EQ. 1) THEN
C          CALL U2MESS('F','PREPOST_11')
C       ENDIF
C       CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'NU      ',
C      &            VALNU,ICODRE,0)
C       IF (ICODRE .EQ. 1) THEN
C          CALL U2MESS('F','PREPOST_12')
C       ENDIF
C       C1 = (1+VALNU)/VALE
C       C2 = VALNU/VALE

C ---------------------------------------------------------------

C POUR LE POST-FATIGUE, ON CONSIDERE QUE L'HISTOIRE EST
C POUR UN CYCLE ENTIER - COMPLET

C C      IF (POST) THEN
C       ORDINI = 1
C       ORDFIN = NBORDR
C C      ENDIF
C       IF ((PARACT(11) .EQ. 1) .OR. (PARACT(10) .EQ. 1) ) THEN
C C ANALYSER LES CHARGEMENTS APLIQUES
C
C          CALL ACANCY(NBORDR, KWORK, SOMPGW, JRWORK, TSPAQ, IPG, C1,
C      &                C2, ORINIE, ORDFIE, NBCYAD,CYFERM, EPSPA)
C          ORDINI = ORINIE
C          ORDFIN = ORDFIE
C
C       ENDIF

C
C ---------------------------------------------------------------
C ANALYSER LES CHARGEMENTS APLIQUES

C        CALL ACANCY(NBORDR, KWORK, SOMPGW, JRWORK, TSPAQ, IPG, C1,
C      &                C2, ORDINI, ORDFIN, NBCYAD,CYFERM, EPSPA)
C ---------------------------------------------------------------
C CALCULER LES GRANDEURS

C POUR LE POST-FATIGUE, ON CONSIDERE QUE L'HISTOIRE EST
C POUR UN CYCLE ENTIER - COMPLET
C
C       IF (POST) THEN
C          ORDINI = 1
C       ENDIF

C ---------------------------------------------------------------
C CALCULER LES GRANDEURS
      ORDFIN = NBORDR

      DO 10 J= ORDINI, ORDFIN
         DECAL = 18
C         ADR = (J-1)*TSPAQ+KWORK*SOMPGW*6+(IPG-1)*6

         ADR = (J-1)*TSPAQ+KWORK*SOMPGW*DECAL+(IPG-1)*DECAL

C RECUPERER LES TENSEURS A CE NUMERO D'ORDRE
C ORDRE DU TENSEUR SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ,
C SIMILAIRE POUR
C   EPS  : DEFORMATION TOTALE
C   EPSE : DEFORMATION ELASTIQUE
C   EPSP : DEFORMATION PLASTIQUE


         CALL TENEPS( JRWORK,ADR, C1, C2, SIG, EPS, EPSE, EPSP)

C ---------------------------------------------------------------
C ON CALCULE PHYDRM QU'UNE FOIS, LA PRESSION HYDROSTATIQUE
C EST CONSTANTE PAR RAPPORT AU vect_n.

C -- CALCULER LES GRANDEURS PRESSION HYDROSTATIQUE

C    CALCULER PRESSION HYDROSTATIQUE MAXIMALE = Max_t(1/3 Tr[SIG])
         IF  ( (PARACT(2) .EQ. 1) .OR. (PARACT(12) .EQ. 1)
     &                  .OR. (PARACT(13) .EQ. 1) ) THEN

            PHYDRO = (SIG(1) + SIG(2) + SIG(3))/3.0D0

            IF (PHYDRO .GT. PHYDRM) THEN
               PHYDRM = PHYDRO
            ENDIF

            IF (PHYDRO .LT. PHYMIN) THEN
               PHYMIN = PHYDRO
            ENDIF

         ENDIF

C ---------------------------------------------------------------
C -- CALCULER LA DEMI-AMPLITUDE DE LA DEFORMATION PLASIQUE EQVA
C POUR LE CRIETRE MANSON_COFF

         IF (PARACT(7) .EQ. 1)  THEN

            DO 11 L = J, ORDFIN
               ADRL = (L-1)*TSPAQ+KWORK*SOMPGW*DECAL+(IPG-1)*DECAL

               CALL TENEPS(JRWORK,ADRL,C1,C2,
     &                         RBID,RBID, RBID, EPSPL)

               DO 12 K = 1, 6
                  VEPSP(K)= EPSP(K) - EPSPL(K)
12             CONTINUE
               CALL FGEQUI(VEPSP,'EPSI',3,EQUI)
               EQEPSP =  EQUI(1)
C               EQEPSP = LCIV2E(VEPSP)

               IF (VEPSPE .LT. EQEPSP) THEN
                  VEPSPE = EQEPSP
               ENDIF
11          CONTINUE

         ENDIF

C SIG PRIN MAX-----------------------------------------------
      IF ((PARACT(8) .EQ. 1) .OR. (PARACT(9) .EQ. 1) .OR.
     &          (PARACT(19) .EQ. 1) ) THEN
            NVP = 3
            NPERM = 12
            TOL = 1.D-10
            TOLDYN = 1.D-2
            ITYPE = 0
            IORDRE = 1
            AR(1) = EPS(1)
            AR(2) = EPS(4)
            AR(3) = EPS(5)
            AR(4) = EPS(2)
            AR(5) = EPS(6)
            AR(6) = EPS(3)
            BR(1) = 1.D0
            BR(2) = 0.D0
            BR(3) = 0.D0
            BR(4) = 1.D0
            BR(5) = 0.D0
            BR(6) = 1.D0

            CALL JACOBI(NVP,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,
     &                        JACAUX,NITJAC,ITYPE,IORDRE)

            IF (EPRMAX .LT. VALPRO(1)) THEN
               EPRMAX = VALPRO(1)
               NM1X = VECPRO (1,1)
               NM1Y = VECPRO (2,1)
               NM1Z = VECPRO (3,1)
C CALCvect_F = [SIG].vect_n

               FXM = SIG(1)*NM1X + SIG(4)*NM1Y + SIG(5)*NM1Z
               FYM = SIG(4)*NM1X + SIG(2)*NM1Y + SIG(6)*NM1Z
               FZM = SIG(5)*NM1X + SIG(6)*NM1Y + SIG(3)*NM1Z

C CALCNORM = vect_F.vect_n

               SIGNM1 = FXM*NM1X + FYM*NM1Y + FZM*NM1Z

               IF (ABS(SIGNM1) .GT. SINM1M) THEN
                  SINM1M = ABS(SIGNM1)
               ENDIF

            ENDIF

            IF (EPRMIN .GT. VALPRO(1)) THEN
               EPRMIN = VALPRO(1)
            ENDIF

            IF (ETREMA .LT. (VALPRO(1)-VALPRO(3))) THEN
               ETREMA = (VALPRO(1)-VALPRO(3))
            ENDIF

            IF (ETREMI .GT. (VALPRO(1)-VALPRO(3))) THEN
               ETREMI = VALPRO(1)-VALPRO(3)
            ENDIF
         ENDIF

C ---------------------------------------------------------------
C CALCULER DENSITE D'ENERGIE DISTORSION ELASTIQUE
C
         IF (PARACT(11) .EQ. 1)  THEN

            CALL LCDEVI(SIG,DEVSIG)
            CALL LCDEVI(EPSE, DVEPSE)


            IF (J .LT. ORDFIN) THEN
               ADRL = (J+1-1)*TSPAQ+KWORK*SOMPGW*DECAL+(IPG-1)*DECAL

               CALL TENEPS(JRWORK,ADRL,C1,C2,
     &                         SIGL,EPSL, EPSEL, EPSPL)

               CALL LCDEVI(SIGL, DSIGL)
               CALL LCDEVI(EPSEL, DEPSL)

               SOMDEN = 0.D0
               DO 32 I = 1, 6

                  SOMDEN= SOMDEN + 0.5D0*(DEVSIG(I)+DSIGL(I))
     &                             *(DEPSL(I)- DVEPSE(I))
32             CONTINUE

              IF (SOMDEN .GT. 0) THEN
                  DENDIE = DENDIE + SOMDEN
              ENDIF

            ENDIF

         ENDIF

C ---------------------------------------------------------------
C CALCULER DENSITE D'ENERGIE DISSIPISE PLASTIQUE
C
         IF (PARACT(10) .EQ. 1)  THEN

            IF (J .LT. ORDFIN) THEN
               ADRL = (J+1-1)*TSPAQ+KWORK*SOMPGW*DECAL+(IPG-1)*DECAL

               CALL TENEPS(JRWORK,ADRL,C1,C2,
     &                         SIGL,EPSL, EPSEL, EPSPL)

               SOMDEN = 0.D0
               DO 33 I = 1, 6
                  SOMDEN= SOMDEN + 0.5D0*(SIG(I)+SIGL(I))
     &                             *(EPSPL(I)- EPSP(I))

33             CONTINUE

               DENDIS = DENDIS + SOMDEN

            ENDIF

         ENDIF

C ---------------------------------------------------------------
C -- CALCULER LA DEMI-AMPLITUDE DE LA CONTRAINTE EQVALENTE
C POUR LE CRIETRE MANSON_COFF

         IF ((PARACT(14) .EQ. 1) .OR. (PARACT(22) .EQ. 1)) THEN

            DO 21 L = J, ORDFIN
               ADRL = (L-1)*TSPAQ+KWORK*SOMPGW*DECAL+(IPG-1)*DECAL

               CALL TENEPS(JRWORK,ADRL,C1,C2,
     &                         SIGL,RBID, RBID, RBID)

               DO 22 I = 1, 6
                  VSIG(I)= SIG(I) - SIGL(I)
22             CONTINUE
               CALL FGEQUI(VSIG,'SIGM',3,EQUI)
               EQSIG = EQUI(1)

               IF (VSIGE .LT. EQSIG) THEN
                  VSIGE = EQSIG
               ENDIF
21          CONTINUE

         ENDIF

C ---------------------------------------------------------------
C CALCULER CONTRAINTES PRINCIPALES MAX ET LA DEF TRACTION DU PLAN
         IF ((PARACT(15) .EQ. 1) .OR. (PARACT(16) .EQ. 1) .OR.
     &          (PARACT(18) .EQ. 1) ) THEN
            NVP = 3
            NPERM = 12
            TOL = 1.D-10
            TOLDYN = 1.D-2
            ITYPE = 0
            IORDRE = 1
            AR(1) = SIG(1)
            AR(2) = SIG(4)
            AR(3) = SIG(5)
            AR(4) = SIG(2)
            AR(5) = SIG(6)
            AR(6) = SIG(3)
            BR(1) = 1.D0
            BR(2) = 0.D0
            BR(3) = 0.D0
            BR(4) = 1.D0
            BR(5) = 0.D0
            BR(6) = 1.D0

            CALL JACOBI(NVP,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,
     &                        JACAUX,NITJAC,ITYPE,IORDRE)

            IF (SIGMAX .LT. VALPRO(1)) THEN
               SIGMAX = VALPRO(1)
               NM1X = VECPRO (1,1)
               NM1Y = VECPRO (2,1)
               NM1Z = VECPRO (3,1)
C CALCvect_F = [SIG].vect_n

               EXM = EPS(1)*NM1X + EPS(4)*NM1Y + EPS(5)*NM1Z
               EYM = EPS(4)*NM1X + EPS(2)*NM1Y + EPS(6)*NM1Z
               EZM = EPS(5)*NM1X + EPS(6)*NM1Y + EPS(3)*NM1Z

C CALCNORM = vect_F.vect_n

               EPSNM1 = EXM*NM1X + EYM*NM1Y + EZM*NM1Z

               IF (ABS(EPSNM1) .GT. EPNM1M) THEN
                  EPNM1M = ABS(EPSNM1)
               ENDIF

            ENDIF

            IF (SIGMIN .GT. VALPRO(1)) THEN
               SIGMIN = VALPRO(1)
            ENDIF

            IF (STREMA .LT. (VALPRO(1)-VALPRO(3))) THEN
               STREMA = (VALPRO(1)-VALPRO(3))
            ENDIF

            IF (STREMI .GT. (VALPRO(1)-VALPRO(3))) THEN
               STREMI = VALPRO(1)-VALPRO(3)
            ENDIF
         ENDIF

C ---------------------------------------------------------------
C CALCULER CONTRAINTES PRINCIPALES MAX ET LA TRACTION DE CE PLAN
         IF (PARACT(17) .EQ. 1)  THEN

            DO 41 L = J, ORDFIN
               ADRL = (L-1)*TSPAQ+KWORK*SOMPGW*DECAL+(IPG-1)*DECAL

               CALL TENEPS(JRWORK,ADRL,C1,C2,
     &                         RBID,EPSL, EPSEL, EPSPL)

               DO 42 I = 1, 6
                  VEPS(I)= EPS(I) - EPSL(I)
42             CONTINUE
               EQEPS = LCIV2E(VEPS)

               IF (VEPST .LT. EQEPS) THEN
                  VEPST = EQEPS
               ENDIF
41          CONTINUE

         ENDIF

C ---------------------------------------------------------------
C CALCULER DEFORMATION PLASTIQUE ACCUMULEE
         IF (PARACT(20) .EQ. 1)  THEN

            IF (J .LT. ORDFIN) THEN
               ADRL = (J+1-1)*TSPAQ+KWORK*SOMPGW*DECAL+(IPG-1)*DECAL

               CALL TENEPS(JRWORK,ADRL,C1,C2,
     &                         SIGL,EPSL, EPSEL, EPSPL)

               SOMDEF = 0.D0
               DO 43 K=1,6
                  SOMDEF = SOMDEF + (EPSPL(K)-EPSP(K))*
     &                               (EPSPL(K)-EPSP(K))
43             CONTINUE
               EPSPAC = EPSPAC + SOMDEF**0.5D0
            ENDIF

         ENDIF

10    CONTINUE


C ---------------------------------------------------------------
C CALCULER LE RAYON DE SPHERE CIRCONCRITE
      IF (PARACT(21) .EQ. 1)  THEN

         DO 16 J= 1, NBORDR*6
            SIGM(J) = 0.D0
16       CONTINUE

         DO 15 J= ORDINI, ORDFIN
            DECAL = 18

            ADR = (J-1)*TSPAQ+KWORK*SOMPGW*DECAL+(IPG-1)*DECAL

            CALL TENEPS( JRWORK,ADR, C1, C2, SIG, EPS, EPSE, EPSP)

            DO 17 K= 1, 6
               SIGM((J-1)*6+ K) = SIG(K)
17          CONTINUE
15       CONTINUE

         NBF = 6
         NBTOT = ORDFIN - ORDINI +1
         CALL FMRAYO (NBF, NBTOT, SIGM, RAYSPH)

      ENDIF

C ---------------------------------------------------------------
C POUR LES GRANDEURS DES CRITERERS "CISSAILEMENT PLAN CRITIQUE",
C ACMATA  CALCULE LES 8 PREMIER GRANDEURS ET CEUX DE 13-20
C
      IF  ( (PARACT(1) .EQ. 1) .OR. (PARACT(3) .EQ. 1) .OR.
     &      (PARACT(4) .EQ. 1) .OR. (PARACT(5) .EQ. 1)
     &                  .OR. (PARACT(6) .EQ. 1)) THEN

         PLCICR = .TRUE.
      ENDIF

      DO 109 K = 1,2
              DTAUM(K) = 0.D0
              NXM(K)   = 0.D0
              NYM(K)   = 0.D0
              NZM(K)   = 0.D0
              NORMAX(K)= 0.D0
              NORMOY(K)= 0.D0
              EPNMAX(K)= 0.D0
              EPNMOY(K)= 0.D0
109   CONTINUE

      IF (PLCICR) THEN

           CALL ACMATA ( JVECTN, JVECTU, JVECTV, NBORDR, KWORK,
     &                SOMPGW, JRWORK, TSPAQ, IPG, JVECPG,
     &                JDTAUM,JRESUN, NOMMET, NOMMAT, RESUPC)

           DO 110 K = 1,2
              DTAUM(K) =    RESUPC(1+(K-1)*11)
              NXM(K)   =    RESUPC(2+(K-1)*11)
              NYM(K)   =    RESUPC(3+(K-1)*11)
              NZM(K)   =    RESUPC(4+(K-1)*11)
              NORMAX(K)=    RESUPC(5+(K-1)*11)
              NORMOY(K)=    RESUPC(6+(K-1)*11)
              EPNMAX(K)=    RESUPC(7+(K-1)*11)
              EPNMOY(K)=    RESUPC(8+(K-1)*11)

110        CONTINUE

      ENDIF

C -----------------------------------------------------------------
C --  EVALUER LES CRITERES EXISTANTS
C -----------------------------------------------------------------
C

      DO 120 K = 1,2
C
       IF (NOMCRI(1:7) .NE. 'FORMULE') THEN

C RECUPERATION DU COEFFICIENT DE PRE-ECROUISSAGE DONNE
C PAR L'UTILISATE

         CALL GETVR8(' ','COEF_PREECROU',1,IARG,1,COEPRE,IRET)

C        1/ C DE MATAKE
         IF (NOMCRI(1:14) .EQ. 'MATAKE_MODI_AC') THEN
            IF ( NORMAX(K) .GT. 0.0D0 ) THEN
               SIGEQ(K) = COEPRE*DTAUM(K) + (VALA*NORMAX(K))
               SIGEQ(K) = SIGEQ(K)*COEFPA
            ELSE
               SIGEQ(K) = COEPRE*DTAUM(K)
               SIGEQ(K) = SIGEQ(K)*COEFPA
            ENDIF
            GRDEQ(K) = SIGEQ(K)

         ENDIF

C        2/ C DE DANG VAN
         IF (NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AC') THEN
            IF ( PHYDRM .GT. 0.0D0 ) THEN
               SIGEQ(K)  = COEPRE*DTAUM(K) + (VALA*PHYDRM)
               SIGEQ(K)  = SIGEQ(K)*COEFPA
            ELSE
               SIGEQ(K)  = COEPRE*DTAUM(K)
               SIGEQ(K)  = SIGEQ(K)*COEFPA
            ENDIF
            GRDEQ(K) = SIGEQ(K)

         ENDIF

C        CALC NOMBRE DE CYCLES A LA RUPTURE ET DU DOMMAGE
         CALL RCCOME ( NOMMAT, 'FATIGUE', PHENOM, ICODRE )
         IF ( ICODRE .EQ. 1 ) CALL U2MESS('F','FATIGUE1_24')
C        POUR CRITERE= DANG_VAN OU MATAKE


         CALL LIMEND( NOMMAT,GRDEQ,'WOHLER',' ', ENDUR)
         IF (ENDUR) THEN
            NRUPT(K)=R8MAEM()
         ELSE


            CALL RCVALE(NOMMAT,'FATIGUE',1,'SIGM    ',GRDEQ(K),
     &           1,'WOHLER  ',NRUPT(K),ICODRE,1)
         ENDIF
         DOM(K) = 1.D0/NRUPT(K)
         NRUPT(K)= NINT(NRUPT(K))

      ENDIF

 120  CONTINUE


C ---------------------------------------------------------------
C           EVALUER CRITERES FOURNIS PAR FORMULE
C---------------------------------------------------------------
      DO 100 K = 1,2

          IF (NOMCRI(1:7) .EQ. 'FORMULE') THEN
C        NOMBRE DE PARAMETRES DISPONIBLES
             NPARMA = 22
C        VALEURS DE CES PARAMETRES, CORRESSPOND A NOMPAR
             VALPAR(1) = DTAUM(K)
             VALPAR(2) = PHYDRM
             VALPAR(3) = NORMAX(K)
             VALPAR(4) = NORMOY(K)
             VALPAR(5) = EPNMAX(K)
             VALPAR(6) = EPNMOY(K)
             VALPAR(7) = VEPSPE/2.D0
             VALPAR(8) = (EPRMAX - EPRMIN)/2.D0
             VALPAR(9) = SINM1M
             VALPAR(10) = DENDIS
             VALPAR(11) = DENDIE
             VALPAR(12) = (PHYDRM - PHYMIN)/2.D0
             VALPAR(13) = (PHYDRM + PHYMIN)/2.D0
             VALPAR(14) = VSIGE/2.D0
             VALPAR(15) = (SIGMAX - SIGMIN)/2.D0
             VALPAR(16) = EPNM1M
             VALPAR(17) = VEPST/2.D0
             VALPAR(18) = (STREMA -STREMI)/4.D0
             VALPAR(19) = (ETREMA -ETREMI)/4.D0
             VALPAR(20) = EPSPAC
             VALPAR(21) = RAYSPH
             VALPAR(22) = VSIGE/(2.D0*1.732051D0)

C  RECUPERER LES NOMS DE PARAMETRES FOURNIS PAR L'UTILISATEUR
             CHNOM(20:24) = '.PROL'
             CHNOM(1:19) = NOMFOR

             CALL JEVEUO(CHNOM,'L',JPROF)
             CALL FONBPA ( NOMFOR, ZK24(JPROF), CBID, NPARMA,
     &                     NP, NOMPF )

             DO 30 J = 1, NP
                DO 25 IPAR = 1, NPARMA
                   IF (NOMPF(J).EQ.NOMPAR(IPAR)) THEN
                      VALPU(J) =  VALPAR(IPAR)
                      GOTO 30
                   ENDIF
25              CONTINUE
30           CONTINUE

             CALL FOINTE('F',NOMFOR, NP, NOMPF,VALPU, GRDEQ(K),IBID)

C PAS DE CRITERE DE FATEMI ET SOCIE EN ELASTIQUE ET AMPLITUDE CONSTANTE,
C CELAAS DE SENS.

C        CALC NOMBRE DE CYCLES A LA RUPTURE ET DU DOMMAGE
            CALL RCCOME ( NOMMAT, 'FATIGUE', PHENOM, ICODRE )
            IF ( ICODRE .EQ. 1 ) CALL U2MESS('F','FATIGUE1_24')

C        POUR CRITERE= FORMULE


            CALL LIMEND( NOMMAT,GRDEQ(K),GRDVIE,FORVIE, ENDUR)

            IF (ENDUR) THEN
               NRUPT(K)=R8MAEM()
            ELSE

               IF (GRDVIE(1:6) .EQ. 'WOHLER') THEN
                  NOMGRD = 'SIGM    '
                  GRDVIE(7:8) = '  '
                  CALL RCVALE(NOMMAT,'FATIGUE',1,NOMGRD,GRDEQ(K),
     &              1,GRDVIE,NRUPT(K),ICODRE,1)
               ENDIF

               IF (GRDVIE(1:8) .EQ. 'MANSON_C') THEN
                  NOMGRD = 'EPSI    '
                  CALL RCVALE(NOMMAT,'FATIGUE',1,NOMGRD,GRDEQ(K),
     &              1,GRDVIE,NRUPT(K),ICODRE,1)
               ENDIF

               IF (GRDVIE(1:8) .EQ. 'FORM_VIE') THEN

                  CALL RENRFA(FORVIE, GRDEQ(K), NRUPT(K),ICODRE)

               ENDIF

               DOM(K) = 1.D0/NRUPT(K)
               NRUPT(K) = NINT(NRUPT(K))
            ENDIF

         ENDIF

100    CONTINUE

C ------------------------------------------------------------------
C---  SORTIE LES RESULTAT
C-------------------------------------------------------------------

C        CONSON D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
C        POURE POINT DE GAUSS DE CHAQUE MAILLE MAX DE DTAU_MAX ET LE
C        VECTRMAL ASSOCIE.
      DO 40 I = 1, 24
         VRESU(I) = 0.0D0
40    CONTINUE

      DO 101 K = 1,2

         VRESU(1+(K-1)*11)  = DTAUM(K)
         VRESU(2+(K-1)*11)  = NXM(K)
         VRESU(3+(K-1)*11)  = NYM(K)
         VRESU(4+(K-1)*11)  = NZM(K)
         VRESU(5+(K-1)*11)  = NORMAX(K)
         VRESU(6+(K-1)*11)  = NORMOY(K)
         VRESU(7+(K-1)*11)  = EPNMAX(K)
         VRESU(8+(K-1)*11)  = EPNMOY(K)
         VRESU(9+(K-1)*11)  = GRDEQ(K)
         VRESU(10+(K-1)*11) = NRUPT(K)
         VRESU(11+(K-1)*11) = DOM(K)
101   CONTINUE
         VRESU(23) = 0.0D0
         VRESU(24) = 0.0D0

C      CALL JEDEMA()
      END
