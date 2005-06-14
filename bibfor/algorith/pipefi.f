      SUBROUTINE PIPEFI( NPG      , LGPG     ,MATE      , GEOM , VIM , 
     &                   DDEPL    , DEPLM    , DDEPL0   ,    
     &                   DDEPL1   , DTAU     , COPILO   , TYPMOD       )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/03/2005   AUTEUR LAVERNE J.LAVERNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================

      IMPLICIT NONE
      INTEGER MATE,NPG,LGPG
      REAL*8 GEOM(2,4), VIM(LGPG,NPG), DDEPL(2,4), DEPLM(2,4)
      REAL*8 DDEPL0(2,4), DDEPL1(2,4), DTAU, COPILO(5,NPG) 
      CHARACTER*8 TYPMOD(2)
       
C-----------------------------------------------------------------------
C
C  PILOTAGE PRED_ELAS POUR LES ELEMENTS DE JOINT (QUAD4)
C    
C IN  : GEOM, MATE, VIM, DDEPL, DEPLM, DDEPL0, DDELP1, DTAU, NPG
C OUT : COPILO
C-----------------------------------------------------------------------
      
      LOGICAL AXI
      INTEGER I,J,KPG
      REAL*8  UP(8),UD(8),SUP(2),SUD(2),B(2,8),POIDS
      REAL*8  R8VIDE
C-----------------------------------------------------------------------

C INITIALISATION

      AXI = TYPMOD(1) .EQ. 'AXIS'
                                          
C DEPLACEMENT U(ETA) = UP + ETA * UD   
      
      CALL DCOPY(8,DEPLM,1,UP,1)
      CALL DAXPY(8,1.D0,DDEPL,1,UP,1)
      CALL DAXPY(8,1.D0,DDEPL0,1,UP,1)
      CALL DCOPY(8,DDEPL1,1,UD,1)
     
       
C BOUCLE SUR LES POINTS DE GAUSS :
 
      DO 10 KPG=1,NPG
              
C      SAUT AU POINT DE GAUSS : SU(ETA) = SUP + ETA * SUD
        CALL NMFISA(AXI,GEOM,KPG,POIDS,B)
        DO 30 I=1,2
          SUP(I) = 0.D0
          SUD(I) = 0.D0
          DO 40 J=1,8
            SUP(I) = SUP(I) + B(I,J)*UP(J)
            SUD(I) = SUD(I) + B(I,J)*UD(J)
 40       CONTINUE
 30     CONTINUE

C      INITIALISATION DES COEFFICIENTS DE PILOTAGE
        CALL R8INIR(4, 0.D0, COPILO(1,KPG),1)
        COPILO(5,KPG) = R8VIDE()

C      APPEL DU PILOTAGE PRED_ELAS SPECIFIQUE A LA LOI DE COMPORTEMENT
        CALL PIPEBA(MATE,SUP,SUD,VIM(1,KPG),DTAU,COPILO(1,KPG))

  10  CONTINUE
   
      END
