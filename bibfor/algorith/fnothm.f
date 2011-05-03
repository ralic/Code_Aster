      SUBROUTINE  FNOTHM(FNOEVO,DELTAT,PERMAN,
     >                   NNO,NNOS,NNOM,NPI,NPG,IPOIDS,
     +                   IPOID2,IVF,IVF2,IDFDE,IDFDE2,GEOM,CONGEM,B,
     +                   DFDI,DFDI2,R,VECTU,IMATE,MECANI,PRESS1,PRESS2,
     +                   TEMPE,DIMDEF,DIMCON,NDDLS,NDDLM,DIMUEL,
     +                   NMEC,NP1,NP2,NDIM,AXI)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 02/05/2011   AUTEUR DELMAS J.DELMAS 
C RESPONSABLE GRANET S.GRANET
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C ======================================================================
       IMPLICIT     NONE
       LOGICAL      FNOEVO,PERMAN,AXI
       INTEGER      NNO,NNOS,NPG,IMATE,DIMDEF,DIMCON,NDDLS,NDDLM,NNOM
       INTEGER      DIMUEL,NMEC,NP1,NP2,NDIM,IPOIDS,IPOID2,IVF,IVF2
       INTEGER      IDFDE,IDFDE2,NPI,MECANI(5),PRESS1(7),PRESS2(7)
       INTEGER      TEMPE(5),YAMEC,YAP1,YAP2,YATE
       INTEGER      ADDEME,ADDEP1
       INTEGER      ADDEP2,ADDETE
       REAL*8       DELTAT,DFDI(NNO,3),DFDI2(NNOS,3),GEOM(NDIM,NNO)
       REAL*8       CONGEM(1:NPI*DIMCON),POIDS,POIDS2
       REAL*8       VECTU(DIMUEL),B(DIMDEF,DIMUEL),R(1:DIMDEF+1)
C ======================================================================
C     BUT:  CALCUL  DE L'OPTION FORC_NODA EN MECANIQUE
C           DES MILIEUX POREUX AVEC COUPLAGE THM
C
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
C NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
C                 SOMMETS             POUR LUMPEE   (=NPI=NNOS)
C                 POINTS DE GAUSS     POUR REDUITE  (<NPI)
C NDIM      DIMENSION DE L'ESPACE
C DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
C DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
C DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
C IVF       FONCTIONS DE FORMES QUADRATIQUES
C IVF2      FONCTIONS DE FORMES LINEAIRES
C ======================================================================
C OUT
C ======================================================================
C OUT DFDI    : DERIVEE DES FCT FORME
C OUT R       : TABLEAU DES RESIDUS
C OUT VECTU   : FORCES NODALES
C ======================================================================
      INTEGER      KPI,I,N
      REAL*8       DT
C ======================================================================
C --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU --------------
C ======================================================================
      YAMEC  = MECANI(1)
      ADDEME = MECANI(2)
      YAP1   = PRESS1(1)
      ADDEP1 = PRESS1(3)
      IF ( PERMAN ) THEN
        I = 1
      ELSE
        I = 0
      ENDIF
      YAP2   = PRESS2(1)
      ADDEP2 = PRESS2(3)
      YATE   = TEMPE(1)
      ADDETE = TEMPE(2)
C
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
         DO 22 I=1,DIMDEF+1
            R(I)=0.D0
 22      CONTINUE
C ======================================================================
C --- CALCUL DE LA MATRICE B AU POINT DE GAUSS -------------------------
C ======================================================================
         CALL CABTHM(NDDLS,NDDLM,NNO,NNOS,NNOM,DIMUEL,
     +               DIMDEF,NDIM,KPI,IPOIDS,IPOID2,IVF,IVF2,
     +               IDFDE,IDFDE2,DFDI,DFDI2,
     +               GEOM,POIDS,POIDS2,B,NMEC,YAMEC,ADDEME,YAP1,
     +               ADDEP1,YAP2,ADDEP2,YATE,ADDETE,NP1,NP2,AXI)
C ======================================================================
         CALL FONODA(IMATE,PERMAN,MECANI,PRESS1,PRESS2,TEMPE,
     &               DIMDEF,DIMCON,NDIM,DT,FNOEVO,
     &               CONGEM((KPI-1)*DIMCON+1),R)
C ======================================================================
C --- CONTRIBUTION DU POINT D'INTEGRATION KPI AU RESIDU ----------------
C ======================================================================
         DO 117 I=1,DIMUEL
            DO 118 N=1,DIMDEF
               VECTU(I)=VECTU(I)+B(N,I)*R(N)*POIDS
 118        CONTINUE
 117     CONTINUE
C ======================================================================
 10   CONTINUE
C ======================================================================
      END
