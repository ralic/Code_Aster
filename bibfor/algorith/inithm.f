      SUBROUTINE INITHM(IMATE,YAMEC,PHI0,EM,ALPHA0,K0,CS,BIOT,T,
     +                                    EPSV,DEPSV,EPSVM,MECA)
      IMPLICIT      NONE
      INTEGER       IMATE,YAMEC
      REAL*8        PHI0,EM,ALPHA0,K0,CS,BIOT,EPSVM,EPSV,DEPSV
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/05/2007   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- RECUPERATION DE VALEURS MECANIQUES -------------------------------
C --- SI PAS MECA ALORS ON POSE BIOT = PHI0 POUR ANNULER LES TERMES ----
C --- DANS LES FORMULATIONS AVEC MECA. IDEM POUR ALPHA = 0 -------------
C ======================================================================
      INTEGER      NELAS
      PARAMETER  ( NELAS=4 )
      REAL*8       ELAS(NELAS),YOUNG,NU,T
      CHARACTER*8  NCRA1(NELAS)
      CHARACTER*2  CODRET(NELAS)
      CHARACTER*16  MECA
      REAL*8       EPS
      PARAMETER  ( EPS = 1.D-21 ) 
C ======================================================================
C --- DONNEES POUR RECUPERER LES CARACTERISTIQUES MECANIQUES -----------
C ======================================================================
      DATA NCRA1/'E','NU','ALPHA','RHO'/
C =====================================================================
C --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
C =====================================================================
      IF (YAMEC.EQ.1) THEN
          CALL RCVALA(IMATE,' ','ELAS',1,'TEMP', T,3,
     +                                 NCRA1(1),ELAS(1),CODRET,'FM')
          YOUNG  = ELAS(1)
          NU     = ELAS(2)
          ALPHA0 = ELAS(3)
          K0     = YOUNG / 3.D0 / (1.D0-2.D0*NU)
          CS     = (1.0D0-BIOT) / K0
      ELSE
C =====================================================================
C --- EN ABSENCE DE MECA ALPHA0 = 0 et 1/KS = 0 OU EM -----------------
C =====================================================================
         ALPHA0 = 0.0D0
         CS     = EM
         IF(EM.LT.EPS)THEN 
           BIOT   = PHI0
         ENDIF
         K0     = 0.0D0
      ENDIF
C =====================================================================
C --- CALCUL EPSV AU TEMPS MOINS --------------------------------------
C =====================================================================
      EPSVM = EPSV - DEPSV
C =====================================================================
      END
