      SUBROUTINE  FNOTHM(FNOEVO,DT,NNO,NNOS,NNOM,NPI,NPG,IPOIDS,
     +                   IPOID2,IVF,IVF2,IDFDE,IDFDE2,GEOM,CONGEM,B,
     +                   DFDI,DFDI2,R,VECTU,IMATE,MECANI,PRESS1,PRESS2,
     +                   TEMPE,DIMDEF,DIMCON,NDDLS,NDDLM,DIMUEL,
     +                   NMEC,NP1,NP2,NDIM,AXI)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 16/08/2005   AUTEUR ROMEO R.FERNANDES 
C RESPONSABLE UFBHHLL C.CHAVANT
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
C TOLE CRP_20
C TOLE CRP_21
C ======================================================================
       IMPLICIT     NONE
       LOGICAL      FNOEVO,AXI
       INTEGER      NNO,NNOS,NPG,IMATE,DIMDEF,DIMCON,NDDLS,NDDLM,NNOM
       INTEGER      DIMUEL,NMEC,NP1,NP2,NDIM,IPOIDS,IPOID2,IVF,IVF2
       INTEGER      IDFDE,IDFDE2,NPI,MECANI(5),PRESS1(7),PRESS2(7)
       INTEGER      TEMPE(5),YAMEC,YAP1,YAP2,YATE,ADCOME,NBPHA1,ADCP11
       INTEGER      ADCP12,NBPHA2,ADCP21,ADCP22,ADCOTE,ADDEME,ADDEP1
       INTEGER      ADDEP2,ADDETE
       REAL*8       DT,DFDI(NNO,3),DFDI2(NNOS,3),GEOM(NDIM,NNO)
       REAL*8       CONGEM(1:NPI*DIMCON),POIDS,POIDS2
       REAL*8       VECTU(DIMUEL),B(DIMDEF,DIMUEL),R(1:DIMDEF+1)  
C ======================================================================
C     BUT:  CALCUL  DE L'OPTION FORC_NODA EN MECANIQUE
C           DES MILIEUX POREUX AVEC COUPLAGE THM 
C  
C  SI FNOEVO = VRAI
C  C EST QUE L ON APPELLE DEPUIS STAT NON LINE  : 
C  ET ALORS LES TERMES DEPENDANT DE DT SONT EVALUES
C
C  SI  FNOEVO = FAUX
C  C EST QUE L ON APPELLE DEPUIS CALCNO  :
C  ET ALORS LES TERMES DEPENDANT DE DT SONT PAS EVALUES
C ======================================================================
C IN
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
C ======================================================================
C OUT
C ======================================================================
C OUT DFDI    : DERIVEE DES FCT FORME
C OUT R       : TABLEAU DES RESIDUS
C OUT VECTU   : FORCES NODALES
C ======================================================================
      INTEGER      KPI,I,J,N,K,NHOM
      PARAMETER   (NHOM=3)
      REAL*8       DEUX,RAC2,HOM(NHOM),PESA(3)
      PARAMETER   (DEUX = 2.D0)
      CHARACTER*2  CODRET(NHOM)
      CHARACTER*8  NCRA5(NHOM)
      DATA NCRA5 / 'PESA_X','PESA_Y','PESA_Z' /
C ======================================================================
      RAC2 = SQRT(DEUX)
C ======================================================================
C --- RECUPERATION DE LA PESANTEUR DANS DEFI_MATERIAU ------------------
C ======================================================================
       CALL RCVALA(IMATE,' ','THM_DIFFU',0,' ',0.D0,
     +                                       NHOM,NCRA5,HOM,CODRET,'FM')
       PESA(1)=HOM(1)
       PESA(2)=HOM(2)
       PESA(3)=HOM(3)
C ======================================================================
C --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU --------------
C ======================================================================
      YAMEC  = MECANI(1)
      ADDEME = MECANI(2)
      ADCOME = MECANI(3)
      YAP1   = PRESS1(1)
      NBPHA1 = PRESS1(2)
      ADDEP1 = PRESS1(3)
      ADCP11 = PRESS1(4)
      ADCP12 = PRESS1(5)
      YAP2   = PRESS2(1)
      NBPHA2 = PRESS2(2)
      ADDEP2 = PRESS2(3)
      ADCP21 = PRESS2(4)
      ADCP22 = PRESS2(5)
      YATE   = TEMPE(1)
      ADDETE = TEMPE(2)
      ADCOTE = TEMPE(3)
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
C --- CALCUL DE LA MATRICE B AU POINT DE GAUSS -------------------------
C ======================================================================
         CALL CABTHM(NDDLS,NDDLM,NNO,NNOS,NNOM,DIMUEL,
     +               DIMDEF,NDIM,NPI,KPI,IPOIDS,IPOID2,IVF,IVF2,
     +               IDFDE,IDFDE2,DFDI,DFDI2,
     +               GEOM,POIDS,POIDS2,B,NMEC,YAMEC,ADDEME,YAP1,
     +               ADDEP1,YAP2,ADDEP2,YATE,ADDETE,NP1,NP2,AXI)
C ======================================================================
C --- COMME CONGEM CONTIENT LES VRAIES CONTRAINTES ET ------------------
C --- COMME PAR LA SUITE ON TRAVAILLE AVEC SQRT(2)*SXY -----------------
C --- ON COMMENCE PAR MODIFIER LES CONGEM EN CONSEQUENCE ---------------
C ======================================================================
         IF(YAMEC.EQ.1) THEN
            DO 100 I = 4 , 6
               CONGEM((KPI-1)*DIMCON+ADCOME+I-1)= 
     +                           CONGEM((KPI-1)*DIMCON+ADCOME+I-1)*RAC2
 100        CONTINUE
         ENDIF
C ======================================================================
C --- INITIALISATION DE R ----------------------------------------------
C ======================================================================
         DO 22 I=1,DIMDEF+1
            R(I)=0.D0
 22      CONTINUE
C ======================================================================
C --- CALCUL DU RESIDU R -----------------------------------------------
C ======================================================================
         IF(YAMEC.EQ.1) THEN
C ======================================================================
C --- CONTRIBUTIONS A R2 INDEPENDANTE DE YAP1, YAP2 ET YATE ------------
C --- CONTRAINTES SIGPRIMPLUS PAGE 33 ----------------------------------
C ======================================================================
            DO 6 I=1,6
               R(ADDEME+NDIM+I-1)= R(ADDEME+NDIM+I-1)
     +                                +CONGEM((KPI-1)*DIMCON+ADCOME-1+I)
 6          CONTINUE
C ======================================================================
C --- SCALAIRE SIGPPLUS MULTIPLIE PAR LE TENSEUR UNITE -----------------
C ======================================================================
            DO 7 I=1,3
               R(ADDEME+NDIM-1+I)=R(ADDEME+NDIM-1+I)+
     +                                   CONGEM((KPI-1)*DIMCON+ADCOME+6)
 7          CONTINUE
C ======================================================================
C --- CONTRIBUTION A R DEPENDANTE DE YAP1 ------------------------------
C ======================================================================
            IF(YAP1.EQ.1) THEN
               DO 8 I=1,NDIM
                  R(ADDEME+I-1)=R(ADDEME+I-1)
     +                           - PESA(I)*CONGEM((KPI-1)*DIMCON+ADCP11)
 8             CONTINUE
               IF(NBPHA1.GT.1) THEN
                  DO 9 I=1,NDIM
                     R(ADDEME+I-1)=R(ADDEME+I-1)
     +                           - PESA(I)*CONGEM((KPI-1)*DIMCON+ADCP12)
 9                CONTINUE
               ENDIF
            ENDIF
C ======================================================================
C --- CONTRIBUTIONS A R DEPENDANTE DE YAP2 -----------------------------
C ======================================================================
            IF(YAP2.EQ.1) THEN
               DO 11 I=1,NDIM
                  R(ADDEME+I-1)=R(ADDEME+I-1)
     +                           - PESA(I)*CONGEM((KPI-1)*DIMCON+ADCP21)
 11            CONTINUE
               IF(NBPHA2.GT.1) THEN
                  DO 12 I=1,NDIM
                     R(ADDEME+I-1)=R(ADDEME+I-1)
     +                           - PESA(I)*CONGEM((KPI-1)*DIMCON+ADCP22)
 12               CONTINUE
               ENDIF
            ENDIF
         ENDIF
C ======================================================================
         IF(FNOEVO) THEN
C ======================================================================
C --- TERMES DEPENDANT DE DT DANS FORC_NODA POUR STAT_NON_LINE ---------
C ======================================================================
            IF(YAP1.EQ.1) THEN

               DO 112 I=1,NDIM
                  R(ADDEP1+I)=R(ADDEP1+I)+DT*CONGEM(ADCP11+I)
 112           CONTINUE
 
               IF(NBPHA1.GT.1) THEN
                  DO 13 I=1,NDIM
                     R(ADDEP1+I)=R(ADDEP1+I)+DT*CONGEM(ADCP12+I)
 13               CONTINUE
               ENDIF

               IF(YATE.EQ.1) THEN

                  DO 14 I=1,NDIM
                     R(ADDETE)=R(ADDETE)+DT*CONGEM(ADCP11+I)*PESA(I)
 14               CONTINUE
 
                  IF(NBPHA1.GT.1) THEN
                     DO 15 I=1,NDIM
                        R(ADDETE)=R(ADDETE) +DT*CONGEM(ADCP12+I)*PESA(I)
 15                  CONTINUE
                  ENDIF

                  DO 16 I=1,NDIM
                     R(ADDETE+I)=R(ADDETE+I)+
     +                         DT*CONGEM(ADCP11+NDIM+1)*CONGEM(ADCP11+I)
 16               CONTINUE
 
                  IF(NBPHA1.GT.1) THEN
                     DO 17 I=1,NDIM
                        R(ADDETE+I)=R(ADDETE+I)+
     +                         DT*CONGEM(ADCP12+NDIM+1)*CONGEM(ADCP12+I)
 17                  CONTINUE 
                  ENDIF

               ENDIF
            ENDIF
C
            IF(YAP2.EQ.1) THEN
               DO 18 I=1,NDIM
                  R(ADDEP2+I)=R(ADDEP2+I)+DT*CONGEM(ADCP21+I)
 18            CONTINUE
               IF(NBPHA2.GT.1) THEN
                  DO 19 I=1,NDIM
                     R(ADDEP2+I)=R(ADDEP2+I)+DT*CONGEM(ADCP22+I)
 19               CONTINUE
               ENDIF
C
               IF(YATE.EQ.1) THEN
                  DO 20 I=1,NDIM
                     R(ADDETE)=R(ADDETE)+DT*CONGEM(ADCP21+I)*PESA(I)
 20               CONTINUE
                  IF(NBPHA2.GT.1) THEN
                     DO 21 I=1,NDIM
                        R(ADDETE)=R(ADDETE)+DT*CONGEM(ADCP22+I)*PESA(I)
 21                  CONTINUE
                  ENDIF
                  DO 122 I=1,NDIM
                     R(ADDETE+I)=R(ADDETE+I)+
     +                        DT*CONGEM(ADCP21+NDIM+1)*CONGEM(ADCP21+I)
 122              CONTINUE
                  IF(NBPHA2.GT.1) THEN
                     DO 23 I=1,NDIM
                        R(ADDETE+I)=R(ADDETE+I)+
     +                       DT*CONGEM(ADCP22+NDIM+1)*CONGEM(ADCP22+I)
 23                  CONTINUE
                  ENDIF
               ENDIF
            ENDIF
C
            IF(YATE.EQ.1) THEN
               DO 24 I=1,NDIM
                  R(ADDETE+I)=R(ADDETE+I)+DT*CONGEM(ADCOTE+I)
 24            CONTINUE
            ENDIF
         ENDIF
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
