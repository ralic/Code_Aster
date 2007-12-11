      SUBROUTINE EPSDIL(NPI,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,GEOM,
     +                  DIMDEF,DIMUEL,NDIM,NDDLS,NDDLM,NNO,NNOS,NNOM,
     +                  INTERP,AXI,REGULA,DEPLP,DEFGEP)
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
      IMPLICIT     NONE
      LOGICAL      AXI
      INTEGER      NPI,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,DIMDEF,DIMUEL
      INTEGER      NDIM,NDDLS,NDDLM,NNO,NNOS,NNOM,REGULA(6)
      REAL*8       GEOM(NDIM,*),DEPLP(DIMUEL),DEFGEP(NPI*DIMDEF)
      CHARACTER*2  INTERP
C ======================================================================
C --- BUT : CALCUL DE EPSI_ELGA_DEPL -----------------------------------
C ======================================================================
      INTEGER      KPI,I,N
      REAL*8       POIDS,POIDS2,B(DIMDEF,DIMUEL)
C ======================================================================
C --- BOUCLE SUR LES POINTS D'INTEGRATION ------------------------------
C ======================================================================
      DO 100 KPI=1,NPI
C ======================================================================
C --- DEFINITION DE L'OPERATEUR B (DEFINI PAR E=B.U) -------------------
C ======================================================================
         IF (INTERP.EQ.'P0') THEN
            CALL CABRP0(KPI,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,GEOM,
     +               DIMDEF,DIMUEL,NDIM,NDDLS,NDDLM,NNO,NNOS,NNOM,AXI,
     +               REGULA,B,POIDS,POIDS2)
         ELSE IF (INTERP.EQ.'SL') THEN
            CALL CABRSL(KPI,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,GEOM,
     +               DIMDEF,DIMUEL,NDIM,NDDLS,NDDLM,NNO,NNOS,NNOM,AXI,
     +               REGULA,B,POIDS,POIDS2)
         ENDIF
C ======================================================================
C --- CALCUL DES DEFORMATIONS GENERALISEES E=B.U -----------------------
C ======================================================================
         DO 10 I=1,DIMDEF
            DEFGEP((KPI-1)*DIMDEF+I)=0.0D0
            DO 20 N=1,DIMUEL
               DEFGEP((KPI-1)*DIMDEF+I) = DEFGEP((KPI-1)*DIMDEF+I)+
     +                                    B(I,N)*DEPLP(N)
 20         CONTINUE
 10      CONTINUE
 100  CONTINUE
C ======================================================================
      END
