      SUBROUTINE  FNEIHM(FNOEVO,DELTAT,PERMAN,NNO1,NNO2,
     >                   NPI,NPG,WREF,IU,IP,IPF,IQ,
     +                   VFF1,VFF2,DFFR2,GEOM,ANG,CONGEM,
     +                   R,VECTU,MECANI,PRESS1,PRESS2,
     +                   TEMPE,DIMDEF,DIMCON,DIMUEL,
     +                   NDIM,AXI)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/12/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C TOLE CRP_21 CRS_1404
C ======================================================================
       IMPLICIT NONE
       LOGICAL      FNOEVO,PERMAN,AXI
       INTEGER      DIMDEF,DIMCON,NNO1,NNO2
       INTEGER      DIMUEL,NDIM
       INTEGER      NPI,NPG,MECANI(8),PRESS1(9),PRESS2(9)
       INTEGER      TEMPE(5),YAMEC,YAP1,YAP2,YATE
       INTEGER      ADDEME,ADDEP1,ADDEP2,ADDETE
       INTEGER      IU(3,18),IP(2,9),IPF(2,2,9),IQ(2,2,9)
       REAL*8       DELTAT,GEOM(NDIM,NNO2),DFFR2(NDIM-1,NNO2,NPI)
       REAL*8       CONGEM(DIMCON,NPI),VFF1(NNO1,NPI),VFF2(NNO2,NPI)
       REAL*8       VECTU(DIMUEL),R(DIMDEF),ANG(24),WREF(NPG)

C ======================================================================
C     BUT:  CALCUL  DE L'OPTION FORC_NODA POUR JOINT AVEC COUPLAGE HM
C  SI FNOEVO = VRAI
C  C EST QUE L'ON APPELLE DEPUIS STAT NON LINE  :
C  ET ALORS LES TERMES DEPENDANT DE DELTAT SONT EVALUES
C
C  SI  FNOEVO = FAUX
C  C EST QUE L'ON APPELLE DEPUIS CALCNO  :
C  ET ALORS LES TERMES DEPENDANT DE DELTAT NE SONT PAS EVALUES
C ======================================================================
C IN
C ======================================================================
C AXI       AXISYMETRIQUE ?
C TYPMOD    MODELISATION (D_PLAN, AXI, 3D ?)
C MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
C NNO       NB DE NOEUDS DE L'ELEMENT
C NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
C NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
C NDDLS     NB DE DDL SUR LES SOMMETS
C NDDLM     NB DE DDL SUR LES MILIEUX
C NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT

C NDIM      DIMENSION DE L'ESPACE
C DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
C DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
C DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
C IVF       FONCTIONS DE FORMES QUADRATIQUES
C IVF2      FONCTIONS DE FORMES LINEAIRES
C ======================================================================
C OUT
C ======================================================================
C OUT R       : TABLEAU DES RESIDUS
C OUT VECTU   : FORCES NODALES
C ======================================================================
      INTEGER      ADCOME,ADCP11,ADCP12,ADCP21,ADCP22,ADCOTE
      INTEGER      ADDLH1
      INTEGER      ADCOP1,ADCOP2,NBPHA1,NBPHA2
      INTEGER      KPI,I,N
      REAL*8       DT,WI,Q(DIMDEF,DIMUEL)

C ======================================================================
C --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU --------------
C ======================================================================

      YAMEC  = MECANI(1)
      ADDEME = MECANI(2)
      ADCOME = MECANI(3)
      YAP1   = PRESS1(1)
      NBPHA1 = PRESS1(2)
      ADDEP1 = PRESS1(3)
      ADDLH1 = PRESS1(4)
      ADCP11 = PRESS1(5)
      ADCP12 = PRESS1(6)
      ADCOP1 = PRESS1(7)
      YAP2   = PRESS2(1)
      NBPHA2 = PRESS2(2)
      ADDEP2 = PRESS2(3)
      ADCP21 = PRESS2(4)
      ADCP22 = PRESS2(5)
      ADCOP2 = PRESS2(6)
      YATE   = TEMPE(1)
      ADDETE = TEMPE(2)
      ADCOTE = TEMPE(3)

      IF ( PERMAN ) THEN
        DT = 1.D0
      ELSE
        DT = DELTAT
      ENDIF

C ======================================================================
C --- INITIALISATION DE VECTU ------------------------------------------
C ======================================================================
      DO 1 I=1,DIMUEL
         VECTU(I)=0.D0
 1    CONTINUE
C ======================================================================
C --- CALCUL POUR CHAQUE POINT DE GAUSS : BOUCLE SUR KPG ---------------
C ======================================================================
      DO 10 KPI=1,NPG

C ======================================================================
C --- INITIALISATION DE R ----------------------------------------------
C ======================================================================
         DO 22 I=1,DIMDEF
            R(I)=0.D0
 22      CONTINUE

C ======================================================================
C --- CALCUL DE LA MATRICE Q AU POINT DE GAUSS -------------------------
C ======================================================================

         CALL MATTHM(NDIM,AXI,NNO1,NNO2,DIMUEL,DIMDEF,IU,IP,IPF,
     &                  IQ,YAP1,YAP2,YATE,ADDEP1,ADDEP2,ADDLH1,
     &                  VFF1(1,KPI),VFF2(1,KPI),DFFR2(1,1,KPI),
     &                  WREF(KPI),GEOM,ANG,WI,Q)

C ======================================================================
         CALL FONOEI(NDIM,DT,FNOEVO,DIMDEF,DIMCON,YAMEC,YAP1,YAP2,YATE,
     &               ADDEME,ADDEP1,ADDEP2,ADDETE,ADDLH1,ADCOME,
     &               ADCP11,ADCP12,ADCP21,ADCP22,ADCOTE,ADCOP1,
     &               ADCOP2,NBPHA1,NBPHA2,CONGEM(1,KPI),
     &               R)

C ======================================================================
C --- CONTRIBUTION DU POINT D'INTEGRATION KPI AU RESIDU ----------------
C ======================================================================

         DO 117 I=1,DIMUEL
            DO 118 N=1,DIMDEF
               VECTU(I)=VECTU(I)+Q(N,I)*R(N)*WI
 118        CONTINUE
 117     CONTINUE

C ======================================================================
 10   CONTINUE
C ======================================================================
      END
