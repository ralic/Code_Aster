      SUBROUTINE CABTHM(NDDLS,NDDLM,NNO,NNOS,NNOM,DIMUEL,DIMDEF,NDIM,
     +               NPI,KPI,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,DFDI,
     +               DFDI2,GEOM,POIDS,POIDS2,B,NMEC,YAMEC,ADDEME,YAP1,
     +               ADDEP1,YAP2,ADDEP2,YATE,ADDETE,NP1,NP2,AXI)
C
       IMPLICIT NONE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/02/2011   AUTEUR GRANET S.GRANET 
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
C     BUT:  CALCUL  DE LA MATRICE B EN MODE D'INTEGRATION MIXTE
C              AVEC ELEMENTS P2P1
C     EN MECANIQUE DES MILIEUX POREUX PARTIELLEMENT SATURE
C     AVEC COUPLAGE THM
C ======================================================================
C.......................................................................
C ARGUMENTS D'ENTREE
C
C ======================================================================
C AXI       AXISYMETRIQUE?
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
C
C                     sommets              |    milieux
C           u v p t u v p t u v p t u v p t u v u v u v u v
C          ------------------------------------------------
C        u|                                |               |
C        v|     Fonctions de forme         |               |
C        E|              P2                |       P2      |
C          ------------------------------------------------
C        P|                                |               |
C       DP|                                |         0     |
C        T|              P1                |               |
C       DT|                                |               |
C          ------------------------------------------------
C
C ======================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C ======================================================================
      LOGICAL      AXI
      INTEGER      NDDLS,NDDLM,NMEC,NP1,NP2,NDIM,NNO,I,N,KK,YAMEC
      INTEGER      NNOS,NNOM,NPI,KPI,DIMDEF,DIMUEL,IPOIDS,IDFDE,IVF
      INTEGER      ADDEME,YAP1,YAP2,ADDEP1,ADDEP2,YATE,ADDETE
      INTEGER      IPOID2,IDFDE2,IVF2
      REAL*8       DFDI(NNO,3),DFDI2(NNOS,3),GEOM(NDIM,NNO),POIDS,POIDS2
      REAL*8       B(DIMDEF,DIMUEL),RAC,R,RMAX
C ======================================================================
C --- CALCUL DE CONSTANTES UTILES --------------------------------------
C ======================================================================
      RAC= SQRT(2.D0)
C ======================================================================
C --- INITIALISATION DE LA MATRICE B -----------------------------------
C ======================================================================
      CALL MATINI(DIMDEF,DIMUEL,0.D0,B)
C ======================================================================
C --- RECUPERATION DES DERIVEES DES FONCTIONS DE FORME -----------------
C ======================================================================
C --- EN 3D ------------------------------------------------------------
C ======================================================================
      IF (NDIM.EQ.3) THEN
C ======================================================================
C --- CAS QUADRATIQUES -------------------------------------------------
C ======================================================================
         CALL DFDM3D (NNO,KPI,IPOIDS,IDFDE,
     +                         GEOM,DFDI(1,1),DFDI(1,2),DFDI(1,3),POIDS)
C ======================================================================
C --- CAS LINEAIRES ----------------------------------------------------
C ======================================================================
         CALL DFDM3D (NNOS,KPI,IPOID2,IDFDE2,GEOM,DFDI2(1,1),
     +                                     DFDI2(1,2),DFDI2(1,3),POIDS2)
C ======================================================================
C --- EN 2D ------------------------------------------------------------
C ======================================================================
      ELSE
C ======================================================================
C --- CAS QUADRATIQUES -------------------------------------------------
C ======================================================================
         CALL DFDM2D(NNO,KPI,IPOIDS,IDFDE,GEOM,DFDI(1,1),
     +                                                  DFDI(1,2),POIDS)
C ======================================================================
C --- CAS LINEAIRES ----------------------------------------------------
C ======================================================================
         CALL DFDM2D(NNOS,KPI,IPOID2,IDFDE2,GEOM,DFDI2(1,1),
     +                                                DFDI2(1,2),POIDS2)

         DO 200 N=1,NNOS
            DFDI2(N,3)=0.D0
 200     CONTINUE
         DO 201 N=1,NNO
            DFDI(N,3)=0.D0
 201     CONTINUE
      ENDIF
C ======================================================================
C --- MODIFICATION DU POIDS POUR LES MODELISATIONS AXIS ----------------
C ======================================================================
      IF (AXI) THEN
         KK = (KPI-1)*NNO
         R  = 0.D0
         DO 10 N=1,NNO
            R  = R + ZR(IVF + N + KK - 1)*GEOM(1,N)
 10      CONTINUE
C ======================================================================
C --- DANS LE CAS OU R EGAL 0, ON A UN JACOBIEN NUL --------------------
C --- EN UN POINT DE GAUSS, ON PREND LE MAX DU RAYON -------------------
C --- SUR L ELEMENT MULTIPLIE PAR 1E-3 ---------------------------------
C ======================================================================
         IF (R .EQ. 0.D0) THEN
            RMAX=GEOM(1,1)
            DO 15 N=2,NNO
               RMAX=MAX(GEOM(1,N),RMAX)
 15         CONTINUE
            POIDS = POIDS*1.D-03*RMAX
         ELSE
            POIDS = POIDS*R
         ENDIF
      ENDIF
C ======================================================================
C --- REMPLISSAGE DE L OPERATEUR B -------------------------------------
C ======================================================================
C --- ON COMMENCE PAR LA PARTIE GAUCHE DE B CORRESPONDANT --------------
C --- AUX NOEUDS SOMMETS -----------------------------------------------
C ======================================================================
      DO 102 N=1,NNOS
C ======================================================================
         IF (YAMEC.EQ.1) THEN
            DO 103 I=1,NDIM
               B(ADDEME-1+I,(N-1)*NDDLS+I)=
     +         B(ADDEME-1+I,(N-1)*NDDLS+I)+ZR(IVF+N+(KPI-1)*NNO-1)
 103        CONTINUE
C ======================================================================
C --- CALCUL DE DEPSX, DEPSY, DEPSZ (DEPSZ INITIALISE A 0 EN 2D) -------
C ======================================================================
            DO 104 I=1,NDIM
               B(ADDEME+NDIM-1+I,(N-1)*NDDLS+I)=
     +         B(ADDEME+NDIM-1+I,(N-1)*NDDLS+I)+DFDI(N,I)
 104        CONTINUE
C ======================================================================
C --- TERME U/R DANS EPSZ EN AXI ---------------------------------------
C ======================================================================
            IF (AXI) THEN
               IF (R .EQ. 0.D0) THEN
                  B(ADDEME+4,(N-1)*NDDLS+1)= DFDI(N,1)
               ELSE
                  KK=(KPI-1)*NNO
                  B(ADDEME+4,(N-1)*NDDLS+1)=ZR(IVF+N+KK-1)/R
               ENDIF
            ENDIF
C ======================================================================
C --- CALCUL DE EPSXY --------------------------------------------------
C ======================================================================
            B(ADDEME+NDIM+3,(N-1)*NDDLS+1)=
     +      B(ADDEME+NDIM+3,(N-1)*NDDLS+1)+DFDI(N,2)/RAC

            B(ADDEME+NDIM+3,(N-1)*NDDLS+2)=
     +      B(ADDEME+NDIM+3,(N-1)*NDDLS+2)+DFDI(N,1)/RAC
C ======================================================================
C --- CALCUL DE EPSXZ ET EPSYZ EN 3D -----------------------------------
C ======================================================================
            IF (NDIM .EQ. 3) THEN
               B(ADDEME+NDIM+4,(N-1)*NDDLS+1)=
     +         B(ADDEME+NDIM+4,(N-1)*NDDLS+1)+DFDI(N,3)/RAC

               B(ADDEME+NDIM+4,(N-1)*NDDLS+3)=
     +         B(ADDEME+NDIM+4,(N-1)*NDDLS+3)+DFDI(N,1)/RAC

               B(ADDEME+NDIM+5,(N-1)*NDDLS+2)=
     +         B(ADDEME+NDIM+5,(N-1)*NDDLS+2)+DFDI(N,3)/RAC

               B(ADDEME+NDIM+5,(N-1)*NDDLS+3)=
     +         B(ADDEME+NDIM+5,(N-1)*NDDLS+3)+DFDI(N,2)/RAC
            ENDIF
         ENDIF
C ======================================================================
C --- TERMES THERMO-HYDRAULIQUES (FONCTIONS DE FORMES P1) --------------
C ======================================================================
C --- SI PRESS1 --------------------------------------------------------
C ======================================================================
         IF (YAP1.EQ.1) THEN
            B(ADDEP1,(N-1)*NDDLS+NMEC+1)=
     +      B(ADDEP1,(N-1)*NDDLS+NMEC+1)+ZR(IVF2+N+(KPI-1)*NNOS-1)
            DO 105 I=1,NDIM
               B(ADDEP1+I,(N-1)*NDDLS+NMEC+1)=
     +         B(ADDEP1+I,(N-1)*NDDLS+NMEC+1)+DFDI2(N,I)
 105        CONTINUE
         ENDIF
C ======================================================================
C --- SI PRESS2 --------------------------------------------------------
C ======================================================================
         IF (YAP2.EQ.1) THEN
            B(ADDEP2,(N-1)*NDDLS+NMEC+NP1+1)=
     +      B(ADDEP2,(N-1)*NDDLS+NMEC+NP1+1)+ZR(IVF2+N+(KPI-1)*NNOS-1)
            DO 106 I=1,NDIM
               B(ADDEP2+I,(N-1)*NDDLS+NMEC+NP1+1)=
     +         B(ADDEP2+I,(N-1)*NDDLS+NMEC+NP1+1)+DFDI2(N,I)
 106        CONTINUE
         ENDIF
C ======================================================================
C --- SI TEMPE ---------------------------------------------------------
C ======================================================================
         IF (YATE.EQ.1) THEN
            B(ADDETE,(N-1)*NDDLS+NMEC+NP1+NP2+1)=
     +      B(ADDETE,(N-1)*NDDLS+NMEC+NP1+NP2+1)
     +                                        +ZR(IVF2+N+(KPI-1)*NNOS-1)
            DO 107 I=1,NDIM
               B(ADDETE+I,(N-1)*NDDLS+NMEC+NP1+NP2+1)=
     +         B(ADDETE+I,(N-1)*NDDLS+NMEC+NP1+NP2+1)+DFDI2(N,I)
 107        CONTINUE
         ENDIF
 102  CONTINUE
C ======================================================================
C --- ON REMPLIT MAINTENANT LE COIN SUPERIEUR DROIT DE B CORRESPONDANT -
C --- AUX NOEUDS MILIEUX (MECANIQUE - FONCTIONS DE FORMES P2) ----------
C ======================================================================
      IF (YAMEC.EQ.1) THEN
         DO 300 N= 1,NNOM
            DO 301 I=1,NDIM
               B(ADDEME-1+I,NNOS*NDDLS+(N-1)*NDDLM+I)=
     +         B(ADDEME-1+I,NNOS*NDDLS+(N-1)*NDDLM+I)
     +                                    +ZR(IVF+N+NNOS+(KPI-1)*NNO-1)
 301        CONTINUE
C ======================================================================
C --- CALCUL DE DEPSX, DEPSY, DEPSZ (DEPSZ INITIALISE A 0 EN 2D) -------
C ======================================================================
            DO 304 I=1,NDIM
               B(ADDEME+NDIM-1+I,NNOS*NDDLS+(N-1)*NDDLM+I)=
     +         B(ADDEME+NDIM-1+I,NNOS*NDDLS+(N-1)*NDDLM+I)
     +                                                   +DFDI(N+NNOS,I)
 304        CONTINUE
C ======================================================================
C --- TERME U/R DANS EPSZ EN AXI ---------------------------------------
C ======================================================================
            IF (AXI) THEN
               IF (R .EQ. 0.D0) THEN
                  B(ADDEME+4,NNOS*NDDLS+(N-1)*NDDLM+1)=DFDI(N+NNOS,1)
               ELSE
                  KK=(KPI-1)*NNO
                  B(ADDEME+4,NNOS*NDDLS+(N-1)*NDDLM+1)=
     +                                             ZR(IVF+N+NNOS+KK-1)/R
               ENDIF
            ENDIF
C ======================================================================
C --- CALCUL DE EPSXY POUR LES NOEUDS MILIEUX --------------------------
C ======================================================================
            B(ADDEME+NDIM+3,NNOS*NDDLS+(N-1)*NDDLM+1)=
     +      B(ADDEME+NDIM+3,NNOS*NDDLS+(N-1)*NDDLM+1)+DFDI(N+NNOS,2)/RAC

            B(ADDEME+NDIM+3,NNOS*NDDLS+(N-1)*NDDLM+2)=
     +      B(ADDEME+NDIM+3,NNOS*NDDLS+(N-1)*NDDLM+2)+DFDI(N+NNOS,1)/RAC
C ======================================================================
C --- CALCUL DE EPSXZ ET EPSYZ EN 3D POUR NOEUDS MILIEUX ---------------
C ======================================================================
            IF (NDIM .EQ. 3) THEN
               B(ADDEME+NDIM+4,NNOS*NDDLS+(N-1)*NDDLM+1)=
     +         B(ADDEME+NDIM+4,NNOS*NDDLS+(N-1)*NDDLM+1)
     +                                               +DFDI(N+NNOS,3)/RAC

               B(ADDEME+NDIM+4,NNOS*NDDLS+(N-1)*NDDLM+3)=
     +         B(ADDEME+NDIM+4,NNOS*NDDLS+(N-1)*NDDLM+3)
     +                                               +DFDI(N+NNOS,1)/RAC

               B(ADDEME+NDIM+5,NNOS*NDDLS+(N-1)*NDDLM+2)=
     +         B(ADDEME+NDIM+5,NNOS*NDDLS+(N-1)*NDDLM+2)
     +                                               +DFDI(N+NNOS,3)/RAC

               B(ADDEME+NDIM+5,NNOS*NDDLS+(N-1)*NDDLM+3)=
     +         B(ADDEME+NDIM+5,NNOS*NDDLS+(N-1)*NDDLM+3)
     +                                               +DFDI(N+NNOS,2)/RAC
            ENDIF
 300     CONTINUE
      ENDIF
C ======================================================================
C --- LE COIN INFERIEUR DROIT EST NUL ----------------------------------
C ======================================================================
      END
