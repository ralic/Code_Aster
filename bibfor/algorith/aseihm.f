      SUBROUTINE  ASEIHM(OPTION,AXI,NDIM,NNO1,NNO2,NPI,NPG,
     &                    DIMUEL,DIMDEF,DIMCON,NBVARI,IMATE, 
     &                    IU,IP,IPF,IQ,
     &                    MECANI,PRESS1,PRESS2,TEMPE,
     &                    VFF1,VFF2,DFFR2,INSTAM,INSTAP,DEPLM,
     &                    DEPLP,SIGM,SIGP,VARIM,VARIP,NOMAIL,
     &                    WREF,GEOM,ANG,COMPOR,PERMAN,
     &                    CRIT,VECTU,MATUU,RETCOM)

      IMPLICIT NONE 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/04/2010   AUTEUR JAUBERT A.JAUBERT 
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
C TOLE CRP_21
C ======================================================================
C =====================================================================
C......................................................................
C
C     BUT:  CALCUL DU VECTEUR FORCES INTERNES ELEMENTAIRE, DES
C           CONTRAINTES GENERALISEES, DES VARIABLES INTERNES
C           ET/OU DE L'OPERATEUR TANGENT ELEMENTAIRE 
C......................................................................
C =====================================================================
C IN OPTION  : OPTION DE CALCUL
C IN AXI     : AXISYMETRIQUE ?
C IN NDIM    : DIMENSION DE L'ESPACE
C IN NNO1    : NOMBRE DE NOEUDS DE LA FAMILLE 1
C IN NNO2    : NOMBRE DE NOEUDS DE LA FAMILLE 2
C IN NPI     : NOMBRE DE POINTS D'INTEGRATION
C IN NPG     : NOMBRE DE POINTS DE GAUSS
C IN DIMUEL  : NOMBRE DE DDL
C IN DIMDEF  : DIMENSION DU VECTEUR DEFORMATIONS GENERALISEES
C IN DIMCON  : DIMENSION DU VECTEUR CONTRAINTES GENERALISEES
C IN IMATE   : MATERIAU CODE
C IN IU      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
C IN IP      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION MILIEU
C IN IPF     : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION FACES
C IN IQ      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE HYDRO
C IN MECANI  : INFOS MECANIQUE
C IN PRESS1  : INFOS CONSTITUANT 1
C IN PRESS2  : INFOS CONSTITUANT 2
C IN TEMPE   : INFOS TEMPERATURE
C IN VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE 1)
C IN VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE 2)
C IN DFFR2   : DERIVEES DES FONCTIONS DE FORME (FAMILLE 2)
C IN INSTAM  : INSTANT PRECEDENT
C IN INSTAP  : INSTANT ACTUEL
C IN DEPLM   : DDL A L'INSTANT PRECEDENT
C IN DEPLP   : DDL A L'INSTANT ACTUEL
C IN SIGM    : CONTRAINTES GENERALISEES AUX POINTS D'INTEGRATION
C IN VARIM   : VARIABLES INTERNES AUX POINTS D'INTEGRATION
C IN NOMAIL  : NUMERO DE LA MAILLE
C IN WREF    : POIDS DE REFERENCE DES POINTS D'INTEGRATIONS
C IN GEOM    : COORDONNEES DES NOEUDS (FAMILLE 1)
C IN ANG     : ANGLES D'EULER NODAUX (FAMILLE 1)
C IN COMPOR  : COMPORTEMENT
C IN PERMAN  : REGIME PERMANENT ?
C IN CRIT    : CRITERES DE CONVERGENCE LOCAUX 
C =====================================================================
C OUT RETCOM : CODE RETOUR LOI DE COMPORTEMENT
C OUT VECTU  : VECTEUR FORCE INTERNE ELEMENTAIRE
C OUT MATUU  : OPERATEUR TANGENT ELEMENTAIRE
C OUT SIGP   : CONTRAINTES GENERALISEES AU TEMPS PLUS AUX POINTS 
C              D'INTEGRATION
C OUT VARIP  : VARIABLES INTERNES AU TEMPS PLUS AUX POINTS D'INTEGRATION
C --- POUR L'HYDRAULIQUE : VAR. INT. 1 : RHO_LIQUIDE - RHO_0 
C --- POUR LE COUPLAGE   : VAR. INT. 1 : PHI - PHI_0 
C ---                    : VAR. INT. 2 : PVP - PVP_0 SI VAPEUR 
C ---                    : VAR. INT. 3 : SATURATION SI LOI NON SATUREE 
C                        : VAR. INT. 4 : OUVH 
C --- POUR LA MECANIQUE  : VAR. INT. 1 : TLINT
C                        
C......................................................................
C


C - VARIABLES ENTREE
       INTEGER NDIM,NNO1,NNO2,NPI,NPG,DIMUEL,DIMDEF,DIMCON,NBVARI
       INTEGER MECANI(8),PRESS1(9),PRESS2(9),TEMPE(5)
       INTEGER IMATE
       INTEGER IU(3,18),IP(2,9),IPF(2,2,9),IQ(2,2,9)
       REAL*8 VFF1(NNO1,NPI),VFF2(NNO2,NPI),DFFR2(NDIM-1,NNO2,NPI)
       REAL*8 WREF(NPI),ANG(24),CRIT(*)
       REAL*8 INSTAM,INSTAP,DEPLM(DIMUEL),DEPLP(DIMUEL),GEOM(NDIM,NNO2)
       REAL*8 SIGM(DIMCON,NPI),VARIM(NBVARI,NPI)
       CHARACTER*8 NOMAIL       
       CHARACTER*16 OPTION,COMPOR(*)
       LOGICAL AXI,PERMAN

C - VARIABLES SORTIE
       INTEGER RETCOM
       REAL*8 VECTU(DIMUEL),VARIP(NBVARI,NPI),SIGP(DIMCON,NPI)
       REAL*8 MATUU(DIMUEL*DIMUEL)

C - VARIABLES LOCALES
       INTEGER YAMEC,YAP1,YAP2,YATE,ADDEME,ADCOME,ADDEP1,ADDEP2,ADDETE
       INTEGER ADCP11,ADCP12,ADCP21,ADCP22,ADCOTE,ADCOP1,ADCOP2
       INTEGER I,J,M,K,KM,KPI,NBPHA1,NBPHA2,ADDLH1
       REAL*8 Q(DIMDEF,DIMUEL),RES(DIMDEF),DRDE(DIMDEF,DIMDEF),WI
       REAL*8 DEFGEM(DIMDEF),DEFGEP(DIMDEF),MATRI
       LOGICAL RESI,RIGI

    
C =====================================================================
C --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU ET OPTION ---
C =====================================================================
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

      RESI = OPTION(1:4).EQ.'FULL' .OR. OPTION(1:4).EQ.'RAPH'
      RIGI = OPTION(1:4).EQ.'FULL' .OR. OPTION(1:4).EQ.'RIGI'

C ======================================================================
C --- INITIALISATION DE VECTU, MATUU A 0 SUIVANT OPTION ----------------
C ======================================================================
      IF (RESI) THEN
        DO 1 I=1,DIMUEL
          VECTU(I)=0.D0
 1      CONTINUE
      END IF  

      IF (RIGI) THEN
         DO 3 I=1,DIMUEL*DIMUEL
               MATUU(I)=0.D0
 3       CONTINUE

      END IF

C =====================================================================
C --- BOUCLE SUR LES POINTS D'INTEGRATION -----------------------------
C =====================================================================

      DO 10 KPI=1,NPI

C =====================================================================
C --- CALCUL DE LA MATRICE DE PASSAGE DDL -> DEFORMATIONS GENERALISEES
C =====================================================================
       
       CALL MATTHM(NDIM,AXI,NNO1,NNO2,DIMUEL,DIMDEF,IU,IP,IPF,
     &             IQ,YAP1,YAP2,YATE,ADDEP1,ADDEP2,ADDLH1,
     &             VFF1(1,KPI),VFF2(1,KPI),DFFR2(1,1,KPI),WREF(KPI),
     &              GEOM,ANG,WI,Q)

C =====================================================================
C --- CALCUL DES DEFORMATIONS GENERALISEES E=QU -----------------------
C =====================================================================
         DO 108 I=1,DIMDEF
           DEFGEM(I)=0.D0
           DEFGEP(I)=0.D0
           DO 109 J=1,DIMUEL
             DEFGEM(I)=DEFGEM(I)+Q(I,J)*DEPLM(J)
             DEFGEP(I)=DEFGEP(I)+Q(I,J)*DEPLP(J)
 109       CONTINUE
 108     CONTINUE

      
C =====================================================================
C --- INTEGRATION DES LOIS DE COMPORTEMENT ----------------------------
C =====================================================================
         
         CALL COEIHM(OPTION,PERMAN,RESI,RIGI,IMATE,COMPOR,
     &                    CRIT,INSTAM,INSTAP,NOMAIL,
     &                    NDIM,DIMDEF,DIMCON,NBVARI,YAMEC,YAP1,
     &                    YAP2,YATE,NBPHA1,NBPHA2,ADDEME,ADCOME,
     &                    ADDEP1,ADCP11,ADCP12,ADDLH1,ADCOP1,
     &                    ADDEP2,ADCP21,ADCP22,ADCOP2,ADDETE,ADCOTE,
     &                    DEFGEM,DEFGEP,KPI,NPG,NPI,SIGM(1,KPI),
     &                    SIGP(1,KPI),VARIM(1,KPI),VARIP(1,KPI),RES,
     &                    DRDE,RETCOM)
         
C =====================================================================
C --- CALCUL DES FORCES INTERIEURES ET DE L'OPERATEUR TANGENT ---------
C =====================================================================

         IF (RESI) THEN
           DO 699 K=1,DIMUEL     
             DO 700 I=1,DIMDEF
               VECTU(K)=VECTU(K)+WI*Q(I,K)*RES(I)      
 700         CONTINUE          
 699       CONTINUE 
         END IF

         IF (RIGI) THEN 
           KM = 1    
           DO 702 K=1,DIMUEL
             DO 703 M=1,DIMUEL 
               MATRI=0.D0
               DO 704 I=1,DIMDEF
                 DO 705 J=1,DIMDEF
                   MATRI = MATRI + WI*Q(I,K)*DRDE(I,J)*Q(J,M)
 705             CONTINUE
 704           CONTINUE
               MATUU(KM) = MATUU(KM) + MATRI 
               KM = KM + 1
 703         CONTINUE  
 702       CONTINUE 
         END IF

 10   CONTINUE

      END
