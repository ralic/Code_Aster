      SUBROUTINE MDFNLI (NBMODE,DEPGEN,VITGEN,ACCGEN,FEXGEN,
     +                   MASGEN,PHICAR,PULSA2,AMOGEN,
     +                   NBCHOC,LOGCHO,DPLMOD,PARCHO,NOECHO,SAUCHO,
     +                   NBREDE,DPLRED,PARRED,FONRED,SAURED,SAREDI,
     +                   NBREVI,DPLREV,FONREV,  
     +                   TEMPS,NOFDEP,NOFVIT,NOFACC,NBEXCI,PSIDEL,
     +                   MONMOT,
     +                   NBRFIS,FK,DFK,ANGINI,
     +                   NUMPAS,NBPAL,DT,DTSTO,TCF,
     +                   VROTAT,TYPAL,FINPAL,CNPAL,PRDEFF,CONV,FSAUV)
      IMPLICIT NONE
      INTEGER       NBMODE,NBREDE,NBREVI,NBEXCI,LOGCHO(*),SAREDI(*)
      INTEGER       NBPAL,NBCHOC
      INTEGER       NUMPAS
      REAL*8        DT,DTSTO,TCF,VROTAT,CONV,ANGINI
      REAL*8        DEPGEN(*),VITGEN(*),FEXGEN(*),MASGEN(*)
      REAL*8        PHICAR(*),PULSA2(*),AMOGEN(*),PARCHO(*),SAUCHO(*)
      REAL*8        PARRED(*),SAURED(*),DPLREV(*),DPLRED(*)
      REAL*8        ACCGEN(*),DPLMOD(NBCHOC,NBMODE,*)
      REAL*8        TEMPS,PSIDEL(NBCHOC,NBEXCI,*)
      CHARACTER*8   NOECHO(*),FONRED(*),FONREV(*),MONMOT
      CHARACTER*8   NOFDEP(NBEXCI),NOFVIT(NBEXCI),NOFACC(NBEXCI)
      CHARACTER*8   FK(2),DFK(2)
      
      LOGICAL       PRDEFF    
      INTEGER       PALMAX 
C-----------------------------------------------------------------------
      INTEGER NBRFIS 
C-----------------------------------------------------------------------
      PARAMETER (PALMAX=20)
      CHARACTER*3   FINPAL(PALMAX)
      CHARACTER*6   TYPAL(PALMAX)      
      CHARACTER*8   CNPAL(PALMAX)
      REAL*8        FSAUV(PALMAX,3)  
      
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21 CRS_512
C
C     AJOUTE AU SECOND MEMBRE LA CONTRIBUTION DES FORCES NON LINEAIRES
C     ------------------------------------------------------------------
C IN  : NBMODE : NOMBRE DE MODES
C IN  : DEPGEN : DEPLACEMENTS GENERALISES AU PAS COURANT
C IN  : VITGEN : VITESSES GENERALISEES AU PAS COURANT
C IN  : ACCGEN : ACCELERATIONS GENERALISEES AU PAS PRECEDENT
C VAR : FEXGEN : FORCES EXTERIEURES GENERALISEES AU PAS COURANT
C VAR : MASGEN : MASSES GENERALISEES AU PAS COURANT (VAR SI FLUIDE)
C VAR : AMOGEN : AMORTISSEMENTS GENERALISES COURANTS (VAR SI FLUIDE)
C VAR : PULSA2 : CARRES DES PULSATIONS PROPRES (VAR SI FLUIDE)
C IN  : PHICAR : DIAGONALE DU PRODUIT (PHI)T. PHI 
C IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
C IN  : LOGCHO : INDICATEUR D'ADHERENCE ET DE FORCE FLUIDE
C IN  : DPLMOD : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
C IN  : PARCHO : TABLEAU DES PARAMETRES DE CHOC
C IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
C OUT : SAUCHO : TABLEAU DES VALEURS A SAUVEGARDER POUR LES CHOCS
C IN  : NBREDE : NOMBRE DE RELATION EFFORT DEPLACEMENT (RED)
C IN  : DPLRED : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE RED
C IN  : PARRED : TABLEAU DES PARAMETRES DE RED
C IN  : FONRED : TABLEAU DES FONCTIONS AUX NOEUDS DE RED
C OUT : SAURED : TABLEAU DES VALEURS A SAUVEGARDER POUR LES RED
C OUT : SAREDI : TABLEAU DES VALEURS A SAUVEGARDER POUR LES RED
C IN  : NBREVI : NOMBRE DE RELATION EFFORT VITESSE (REV)
C IN  : DPLREV : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE REV
C IN  : FONREV : TABLEAU DES FONCTIONS AUX NOEUDS DE REV
C
C IN  : TEMPS  : INSTANT DE CALCUL DES DEPL_IMPO
C IN  : NOFDEP : NOM DE LA FONCTION DEPL_IMPO
C IN  : NOFVIT : NOM DE LA FONCTION VITE_IMPO
C IN  : NOFACC : NOM DE LA FONCTION ACCE_IMPO
C IN  : NBEXCI : NOMBRE D'ACCELERO DIFFERENTS
C IN  : PSIDEL : TABLEAU DE VALEURS DE PSI*DELTA
C IN  : MONMOT : = OUI SI MULTI-APPUIS
C ----------------------------------------------------------------------
C
C     --- FORCES NON-LINEAIRES DE TYPE CHOC ---
      IF ( NBCHOC.NE.0 .AND. NBRFIS.EQ.0 ) 
     +                   CALL MDFCHO ( NBMODE,DEPGEN,VITGEN,
     +                                 ACCGEN,FEXGEN,MASGEN,PHICAR,
     +                                 PULSA2,AMOGEN,NBCHOC,LOGCHO,
     +                                 DPLMOD,PARCHO,NOECHO,SAUCHO,
     +                                 TEMPS,NOFDEP,NOFVIT,NOFACC,
     +                                 NBEXCI,PSIDEL,MONMOT)
C
C     --- NON-LINEARITE DE TYPE RELA_EFFO_DEPL ---
      IF ( NBREDE.NE.0 ) CALL MDFRED ( NBMODE,DEPGEN,FEXGEN,NBREDE,
     +                                 DPLRED,PARRED,FONRED,SAURED,
     +                                 SAREDI )
C
C     --- NON-LINEARITE DE TYPE ROTOR FISSURE ---
      IF ( NBRFIS.GT.0 ) CALL MDRFIS ( NBMODE,DEPGEN,FEXGEN,NBCHOC,
     +                                 NBRFIS,DPLMOD,FK,DFK,PARCHO,
     +                                 ANGINI,VROTAT,TEMPS) 
C
C     --- NON-LINEARITE DE TYPE RELA_EFFO_VITE ---
      IF ( NBREVI.NE.0 ) CALL MDFREV ( NBMODE,VITGEN,FEXGEN,NBREVI,
     +                                 DPLREV,FONREV)
C
C     COUPLAGE AVEC EDYOS
      CONV = 1.D0
      IF ( NBPAL.NE.0 )  CALL MDFEDY ( NBPAL,NBMODE,NUMPAS,
     +   DT,DTSTO,TEMPS,VROTAT,DPLMOD,DEPGEN,VITGEN,
     +   FEXGEN, TYPAL, FINPAL, CNPAL, PRDEFF, CONV, FSAUV)

      END
