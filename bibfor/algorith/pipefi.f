      SUBROUTINE PIPEFI( NPG,MATE , GEOM     , VIM      , 
     &                   DDEPL    , DEPLM    , DDEPL0   ,    
     &                   DDEPL1   , DTAU     , COPILO  ,TYPMOD  )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/07/2003   AUTEUR LAVERNE J.LAVERNE 
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
      INTEGER MATE,NPG
      REAL*8 GEOM(2,4), VIM(3,NPG), DDEPL(2,4), DEPLM(2,4)
      REAL*8 DDEPL0(2,4), DDEPL1(2,4), DTAU, COPILO(5,NPG) 
      CHARACTER*8  TYPMOD(*)
       
C-----------------------------------------------------------------------
C
C BUT : CALCULER LA SOLUTION DE L'EQUATION SUPLEMENTAIRE INTRODUITE POUR
C       LE PILOTAGE.
C
C       L'IDEE EST D'ADAPTER LE CHARGEMENT POUR FAIRE EVOLUER LE SAUT DE
C       L'ELEMENT DE JOINT DE FACON CONTROLEE. CELA PERMET D'EVITER QUE
C       LE SAUT EVOLUE BRUTALEMENT QUAND ON ATTEINT UNE BRANCHE
C       D'EQUILIBRE INSTABLE ET A POUR INTERET DE SUIVRE LA BRANCHE 
C       INSTABLE DE LA COURBE GLOBALE U(F).
C
C       LE CHARGEMENT N'EST PLUS MONOTONE, IL DEPEND DU SAUT DE
C       L'ELEMENT.
C
C    
C IN  : GEOM, MATE, VIM, DDEPL, DEPLM, DDEPL0, DDELP1, DTAU,NPG,TYPMOD
C OUT : COPILO
C I/O : 
C
C-----------------------------------------------------------------------

      INTEGER I,J,P,KPG
      LOGICAL AXI
      REAL*8  UP(8)      , UD(8)  , UREF , UMAX ,
     &        SUP(2)      , SUD(2),
     &        B(2,8),    ETA(2) , 
     &        DPDETA(2) , 
     &        ALPHA      , BETA       , GAMMA      ,
     &        A0         , A1         , A2         , A3         ,
     &        A4         , TEMP  , R8VIDE,AIRE,VALRES(3)
     
      CHARACTER*2 CR,CODRET(3)
      CHARACTER*8 NOMRES(3)

      AXI  = TYPMOD(1) .EQ. 'AXIS'
            
C RECUPERATION DES PARAMETRES DU MODELE :
            
      NOMRES(1) = 'GC'
      NOMRES(2) = 'SIGM_C'
      NOMRES(3) = 'SAUT_C'
      
      CALL RCVALA ( MATE,'RUPT_FRAG',0,' ',0.D0,3,
     &                 NOMRES,VALRES,CODRET, 'F ' )
     
                                         
C INITIALISATION DES VARIABLES  :   
      
      CALL R8COPY(8, DEPLM,1,  UP,1)
      CALL R8AXPY(8, 1.D0, DDEPL,1,  UP,1)
      CALL R8AXPY(8, 1.D0, DDEPL0,1, UP,1)
      
      CALL R8COPY(8, DDEPL1,1,  UD,1)

C CALCUL DE L'AIRE DES PAROIS DE LA FISSURE :
C  * EN 2D ON CONSIDERE QUE L'EPAISSEUR EST DE 1 DONC 
C    L'AIRE EST EGALE A : 1*(LONGUEUR DE L'ELEMENT) 
C  * EN AXIS ON MULTIPLIE CETTE LONGEUR PAR LA DISTANCE DU CENTRE DE 
C    L'ELEMENT A L'AXE DE SYMETRIE.

      AIRE = SQRT( (GEOM(1,2)-GEOM(1,1))**2 + (GEOM(2,2)-GEOM(2,1))**2 )
      IF (AXI) AIRE = AIRE * (GEOM(1,1)+GEOM(1,2))/2.D0
       
C BOUCLE SUR LES POINTS DE GAUSS :
 
      DO 11 KPG=1,NPG
              
C CALCUL DE LA MATRICE B DONNANT LES SAUT PAR ELEMENTS A PARTIR DES 
C DEPLACEMENTS AUX NOEUDS :  
C LE CHANGEMENT DE REPERE EST INTEGRE DANS LA MATRICE B (VOIR NMFISA) 
               
        CALL NMFISA(GEOM,B,KPG)
            
C CALCUL DU SAUT DES VARIABLE UP ET UD :
              
        DO 30 I=1,2
          SUP(I) = 0.D0
          SUD(I) = 0.D0
          DO 40 J=1,8
            SUP(I) = SUP(I) + B(I,J)*UP(J)
            SUD(I) = SUD(I) + B(I,J)*UD(J)
 40       CONTINUE
 30     CONTINUE

C VARIABLE INTERMEDIAIRE :
        UREF  = VALRES(1)/VALRES(2) + VIM(1,KPG) + VALRES(3)
        UMAX = VALRES(3) + VIM(1,KPG)
        TEMP = UMAX + DTAU*UREF

C LES COEF DU POLYNOME DE DEGRE DEUX : P(ETA) SONT : 
      
        ALPHA = SUD(1)*SUD(1) + SUD(2)*SUD(2)   
        BETA  = SUP(1)*SUD(1) + SUP(2)*SUD(2)  
        GAMMA = SUP(1)*SUP(1) + SUP(2)*SUP(2) - TEMP*TEMP
     
      
C - ON CALCUL LES SOLUTIONS DE P(ETA)=0 : ETA(1) ET ETA(2) AINSI QUE 
C   LES PENTES DES TANGENTES A P EN CES POINTS : DPDETA(1) ET DPDETA(2)

C LES SOLUTION DE P(ETA)=0 SONT AUSSI CELLES DE FEL(ETA)-DTAU=0.

C - ON CALCUL L'EQUATION DES DROITES TANGENTES A FEL(ETA)-DTAU 
C   AUX POINTS OU CELLE-CI S'ANNULE I.E: EN ETA(1) ET ETA(2)

C   SI FEL(ETA)-DTAU S'ANNULE :
C      A0 ET A2 SONT LES ORDONNEES A L'ORGINE DE CES DROITES. 
C      A1 ET A3 SONT LES COEFF DIRECTEURS DE CES DROITES.

C   SI FEL(ETA)-DTAU NE S'ANNULE PAS:
C      A0 = MIN_ETA   ( FEL(ETA)-DTAU ) 
C      A4 = ARGMIN_ETA( FEL(ETA)-DTAU )

      
        IF (BETA*BETA-ALPHA*GAMMA .GE. 0.D0) THEN
      
            ETA(1) = ( - BETA + SQRT( BETA*BETA-ALPHA*GAMMA ) ) / ALPHA 
            ETA(2) = ( - BETA - SQRT( BETA*BETA-ALPHA*GAMMA ) ) / ALPHA
          
            DPDETA(1) = 2*ALPHA*ETA(1) + 2*BETA
            DPDETA(2) = 2*ALPHA*ETA(2) + 2*BETA

         
            A0 = DTAU*UREF - 0.5D0*( ETA(1)*DPDETA(1) / TEMP )     
            A2 = DTAU*UREF - 0.5D0*( ETA(2)*DPDETA(2) / TEMP )         
          
            A1 = 0.5D0 * DPDETA(1) / TEMP
            A3 = 0.5D0 * DPDETA(2) / TEMP
          
            A4 = R8VIDE()

            COPILO(1,KPG) = A0/UREF  
            COPILO(2,KPG) = A1/UREF
            COPILO(3,KPG) = A2/UREF
            COPILO(4,KPG) = A3/UREF
            COPILO(5,KPG) = A4
          
          ELSE
                      
            A4 = - BETA/ALPHA 
            A0 = SQRT( ALPHA*A4*A4 + 2*BETA*A4 + GAMMA + TEMP*TEMP )
     &           - UMAX
          
            A1 = R8VIDE()
            A2 = R8VIDE()
            A3 = R8VIDE()

            COPILO(1,KPG) = A0/UREF 
            COPILO(2,KPG) = A1
            COPILO(3,KPG) = A2
            COPILO(4,KPG) = A3
            COPILO(5,KPG) = A4/UREF

                   
        ENDIF
        
  11  CONTINUE
   
      END
