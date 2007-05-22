      SUBROUTINE FNOREG(DIMUEL,DIMDEF,NNO,NNOS,NNOM,NDIM,NPI,DIMCON,
     +                  GEOM,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,NDDLS,
     +                  NDDLM,AXI,REGULA,DEPLM,CONTM,IMATE,VECTU)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/05/2007   AUTEUR FERNANDES R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
       IMPLICIT      NONE
       LOGICAL       AXI
       INTEGER       DIMUEL,DIMDEF,NNO,NNOS,NNOM,NDIM,NPI,DIMCON,IPOIDS
       INTEGER       IPOID2,IVF,IVF2,IDFDE,IDFDE2,NDDLS,NDDLM,IMATE
       INTEGER       REGULA(6)
       REAL*8        GEOM(NDIM,*),DEPLM(DIMUEL),VECTU(DIMUEL)
       REAL*8        CONTM(DIMCON*NPI)
C ======================================================================
C --- BUT : CALCUL DES FORCES NODALES A PARTIR DE RIGI_MECA ------------
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER    I,KPI,N
      REAL*8     B(DIMDEF,DIMUEL),POIDS,POIDS2,DEFGEM(DIMDEF),R(DIMDEF)
C ======================================================================
      DO 10 I=1,DIMUEL
         VECTU(I)=0.0D0
 10   CONTINUE

      DO 100 KPI=1,NPI
C ======================================================================
C --- INITIALISATION DE R ----------------------------------------------
C ======================================================================
         DO 22 I=1,DIMDEF
            R(I)  = 0.0D0
 22      CONTINUE
C ======================================================================
         CALL CABR2G(KPI,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,GEOM,
     +               DIMDEF,DIMUEL,NDIM,NDDLS,NDDLM,NNO,NNOS,NNOM,AXI,
     +               REGULA,B,POIDS,POIDS2)
C ======================================================================
C --- CALCUL DES DEFORMATIONS GENERALISEES E=B.U -----------------------
C ======================================================================
         DO 110 I=1,DIMDEF
            DEFGEM(I)=0.0D0
            DO 120 N=1,DIMUEL
               DEFGEM(I)=DEFGEM(I)+B(I,N)*DEPLM(N)
 120        CONTINUE
 110     CONTINUE
C ======================================================================
C --- CALCUL DES CONTRAINTES GENERALISEES FINALES ----------------------
C ======================================================================
         CALL REGCGE(DIMDEF,DIMCON,REGULA,NDIM,DEFGEM,
     +               CONTM((KPI-1)*DIMCON+1),R)
C ======================================================================
         CALL DILSGA(DIMDEF,DIMUEL,POIDS,POIDS2,B,R,VECTU)
C ======================================================================
 100  CONTINUE
C ======================================================================
      END
