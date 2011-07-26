      SUBROUTINE NMPICH(MODELE,NUMEDD,MATE  ,CARELE,COMREF,
     &                  COMPOR,LISCHA,CARCRI,FONACT,SDSTAT,
     &                  DEFICO,RESOCO,SDPILO,ITERAT,SDNUME,
     &                  DELTAT,VALINC,SOLALG,VEELEM,VEASSE,
     &                  SDTIME,SDDISC,ETA   ,RHO   ,OFFSET,
     &                  LDCCVG,PILCVG,MATASS)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/07/2011   AUTEUR ABBAS M.ABBAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_21
C
      IMPLICIT NONE
      INTEGER       FONACT(*)
      INTEGER       ITERAT,PILCVG,LDCCVG
      REAL*8        DELTAT,ETA,RHO,OFFSET
      CHARACTER*19  LISCHA,SDNUME,SDPILO,SDDISC,MATASS
      CHARACTER*24  CARCRI,MODELE,NUMEDD,MATE  ,CARELE,COMREF,COMPOR
      CHARACTER*24  DEFICO,RESOCO
      CHARACTER*24  SDSTAT,SDTIME
      CHARACTER*19  VEELEM(*),VEASSE(*) 
      CHARACTER*19  SOLALG(*),VALINC(*)  
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
C
C CALCUL DU ETA DE PILOTAGE ET CALCUL DE LA CORRECTION
C      
C ----------------------------------------------------------------------
C
C
C IN  MODELE : MODELE
C IN  NUMEDD : NUME_DDL
C IN  MATE   : CHAMP MATERIAU
C IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
C IN  SDNUME : SD NUMEROTATION
C IN  COMREF : VARI_COM DE REFERENCE
C IN  COMPOR : COMPORTEMENT
C IN  LISCHA : LISTE DES CHARGES
C IN  SDSTAT : SD STATISTIQUES
C IN  SDPILO : SD PILOTAGE
C IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
C IN  FONACT : FONCTIONNALITES ACTIVEES
C IN  DEFICO : SD DEFINITION CONTACT
C IN  RESOCO : SD RESOLUTION CONTACT VOLATILE OP0070
C IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
C IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
C IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
C IN  ITERAT : NUMERO D'ITERATION DE NEWTON
C IN  DELTAT : INCREMENT DE TEMPS
C IN  NBEFFE : NOMBRE DE VALEURS DE PILOTAGE ENTRANTES
C IN  SDTIME : SD TIMER
C IN  SDDISC : SD DISCRETISATION
C OUT ETA    : PARAMETRE DE PILOTAGE
C OUT RHO    : PARAMETRE DE RECHERCHE_LINEAIRE
C OUT OFFSET : DECALAGE DE ETA_PILOTAGE EN FONCTION DE RHO
C OUT PILCVG : CODE DE CONVERGENCE POUR LE PILOTAGE
C                     - 1 : BORNE ATTEINTE -> FIN DU CALCUL
C                       0 : RAS
C                       1 : PAS DE SOLUTION
C OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
C                       0 : CAS DE FONCTIONNEMENT NORMAL
C                       1 : ECHEC DE L'INTEGRATION DE LA LDC
C                       3 : SIZZ PAS NUL POUR C_PLAN DEBORST
C IN  MATASS : SD MATRICE ASSEMBLEE
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      NBEFFE,NBATTE
      REAL*8       PROETA(2),RESIDU 
      INTEGER      IFM,NIV     
      LOGICAL      IRECLI
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('PILOTAGE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<PILOTAGE> ... PILOTAGE SANS RECH_LINE'  
      ENDIF
C
C --- INITIALISATIONS
C
      RHO    = 1.D0
      OFFSET = 0.D0
      NBATTE = 2
      IRECLI = .FALSE.        
C
C --- RESOLUTION DE L'EQUATION DE PILOTAGE
C         
      CALL NMPILO(SDPILO,DELTAT,RHO   ,SOLALG,VEASSE,
     &            MODELE,MATE  ,COMPOR,RESOCO,VALINC,
     &            NBATTE,NUMEDD,NBEFFE,PROETA,PILCVG,
     &            CARELE)
C     
      IF (PILCVG.EQ.1) GOTO 9999
C
C --- CHOIX DE ETA_PILOTAGE
C  
      CALL NMCETA(MODELE,NUMEDD,MATE  ,CARELE,COMREF,
     &            COMPOR,LISCHA,CARCRI,FONACT,SDSTAT,
     &            DEFICO,SDPILO,ITERAT,SDNUME,VALINC,
     &            SOLALG,VEELEM,VEASSE,SDTIME,SDDISC,
     &            NBEFFE,IRECLI,PROETA,OFFSET,RHO   ,
     &            ETA   ,LDCCVG,PILCVG,RESIDU,MATASS)          
C        
 9999 CONTINUE
      CALL JEDEMA()
      END
