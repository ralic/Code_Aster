      SUBROUTINE ERRCOU(NOMPRG,NUMPAS,NOMVAR,INFO,NPROG,NLU)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ECHANGE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE GREFFET N.GREFFET
C **********************************************************************
C *   LOGICIEL CODE_ASTER - COUPLAGE ASTER/EDYOS - COPYRIGHT EDF 2009  *
C **********************************************************************

CPAT_FONCTION

C  ERRCOU : FONCTION

C    CE SSP PERMET DE VERIFIER LA BONNE EXECUTION DES APPELS YACS
C    EN CAS D'ERREUR, IL INTERPRETE LE CODE ERREUR FOURNI PAR YACS ET
C    L'ECRIT SUR LES FICHIERS DE SORTIE.
C    AU 13/02/09 : MESSAGE D'ERREUR CALCIUM

C    EN PHASE DE DEBUGGAGE :
C    ERRCOU VERIFIE EGALEMENT QUE LA DIMENSION DE LA VARIABLE TEL QUE
C    TRAITEE PAR YACS (NLU) CORRESPOND BIEN A CELLE QUI ETAIT PREVUE
C    ET PROGRAMMEE (NPROG) CE TEST A UNIQUEMENT UN SENS POUR LES APPELS
C    EN LECTURE DE YACS. POUR LES APPELS EN ECRITURE NPROG=NLU
C    (FIXE DANS LE PROGRAMME APPELANT).


C     ERRCOU EST APPELE PAR TOUS LES SSP FAISANT APPEL A YACS ET FAIT
C     APPEL A UN PROGRAMME PYTHON "edyos.py" CONTENANT LES MESSAGES
C     D'ERREURS

CPAT_FONCTION

C.======================================================================

C  REFERENCES BIBLIOGRAPHIQUES
C  ---------------------------

C  NOTE HI-26/03/007A
C  "DEVELOPPEMENT D'UN MODE PRODUCTION POUR CALCIUM: MANUEL UTILISATEUR"
C  ANNEXE 1: CODES D'ERREURS  (PAGE 70)
C  FAYOLLE ERIC, DEMKO BERTRAND (CS SI)  JUILLET 2003
C
C ======================================================================


C  DEVELOPPEMENTS ET CORRECTIONS D'ANOMALIES
C  -----------------------------------------

C  DATE: 13/02/09   AUTEUR: P. VAUGRANTE    ANOMALIE: DEVELOPPEMENT
C  DATE:            AUTEUR:                 ANOMALIE:
C  DATE:            AUTEUR:                 ANOMALIE:
C  DATE:            AUTEUR:                 ANOMALIE:


C ======================================================================
C  VARIABLES UTILISEES
C  -------------------
C
C  ____________________________________________________________________
C !    NOM  !   TYPE      !                  ROLE                      !
C !_________!_____________!____________________________________________!
C !         !             !                                            !
C !         !             !                                            !
C ! NUMPAS  !  ENTIER     !  NUMERO D'ITERATION PENDANT LEQUEL IL Y A  !
C !         !             !  EU UN PROBLEME                            !
C !         !             !                                            !
C ! NOMPRG  !  CHARACTER  !  NOM DU SSP DANS LEQUEL IL Y A EU PROBLEME !
C !         !             !                                            !
C ! NOMVAR  !  CHARACTER  !  NOM DE LA VARIABLE POSANT PROBLEME        !
C !         !             !                                            !
C !         !             !                                            !
C ! INFO    !  ENTIER     !  FLAG D'EXECUTION RETOUNE PAR YACS         !
C !         !             !                                            !
C ! NPROG   !  ENTIER     !  DIMENSION DE LA VARIABLE ECHANGEE         !
C !         !             !  (TELLE QUE PROGRAMMEE)                    !
C !         !             !                                            !
C ! NLU     !  ENTIER     !  DIMENSION DE LA VARIABLE ECHANGEE         !
C !         !             !  (RENVOYE PAR YACS)                        !
C !         !             !                                            !
C !_________!_____________!____________________________________________!




C
C=======================================================================
C  SOUS PROGRAMME(S) APPELE(S) : AUCUN
C
C-----------------------------------------------------------------------
C  SOUS PROGRAMME(S) APPELANT(S) :  INIPAT.F, CHAPAT.F, ECRPAT.F
C
C***********************************************************************
C%W% %D% %T%
C TOLE CRP_4

      IMPLICIT NONE

C     ARGUMENTS
C     ==============
      INCLUDE 'jeveux.h'
      CHARACTER*8   NOMPRG
      INTEGER*4     NUMPAS, INFO, NPROG, NLU, LENVAR
      PARAMETER (LENVAR = 144)
      CHARACTER*(LENVAR) NOMVAR





C     VARIABLES INTERNES
C     ==================

      CHARACTER*(LENVAR) VALK(2)
      INTEGER      VALI(1)
C
      CALL JEMARQ()
C     PAS DE PROBLEME YACS
C     ====================
      IF (INFO.EQ.0 .AND. NPROG.EQ.NLU ) GOTO 9999
      IF (INFO.EQ.0 .AND. NPROG.NE.NLU) THEN
        VALK(1)=NOMPRG
        VALK(2)=NOMVAR
        CALL U2MESK('A','EDYOS_41',2,VALK)
        GOTO 9999
      ENDIF
C
C     PROBLEMES LORS DE L'APPEL YACS
C     ==============================
      VALK(1)=NOMPRG
      VALK(2)=NOMVAR
      VALI(1)=NUMPAS
      CALL U2MESG('F','EDYOS_42',2,VALK,1,VALI,0,0.D0)
      IF (INFO.EQ.1) THEN
        CALL U2MESS('F','EDYOS_1')
      ENDIF
C
      IF (INFO.EQ.2) THEN
        CALL U2MESS('F','EDYOS_2')
      ENDIF
C
      IF (INFO.EQ.3) THEN
        CALL U2MESS('F','EDYOS_3')
      ENDIF
C
      IF (INFO.EQ.4) THEN
        CALL U2MESS('F','EDYOS_4')
      ENDIF
C
      IF (INFO.EQ.5) THEN
        CALL U2MESS('F','EDYOS_5')
      ENDIF
C
      IF (INFO.EQ.6) THEN
        CALL U2MESS('F','EDYOS_6')
      ENDIF
C
      IF (INFO.EQ.7) THEN
        CALL U2MESS('F','EDYOS_7')
      ENDIF
C
      IF (INFO.EQ.8) THEN
        CALL U2MESS('F','EDYOS_8')
      ENDIF
C
      IF (INFO.EQ.9) THEN
        CALL U2MESS('F','EDYOS_9')
      ENDIF
C
      IF (INFO.EQ.10) THEN
        CALL U2MESS('F','EDYOS_10')
      ENDIF
C
      IF (INFO.EQ.11) THEN
        CALL U2MESS('F','EDYOS_11')
      ENDIF
C
      IF (INFO.EQ.12) THEN
        CALL U2MESS('F','EDYOS_12')
      ENDIF
C
      IF (INFO.EQ.13) THEN
        CALL U2MESS('F','EDYOS_13')
      ENDIF
C
      IF (INFO.EQ.14) THEN
        CALL U2MESS('F','EDYOS_14')
      ENDIF
C
      IF (INFO.EQ.15) THEN
        CALL U2MESS('F','EDYOS_15')
      ENDIF
C
      IF (INFO.EQ.16) THEN
        CALL U2MESS('F','EDYOS_16')
      ENDIF
C
      IF (INFO.EQ.17) THEN
        CALL U2MESS('F','EDYOS_17')
      ENDIF
C
      IF (INFO.EQ.18) THEN
        CALL U2MESS('F','EDYOS_18')
      ENDIF
C
      IF (INFO.EQ.19) THEN
        CALL U2MESS('F','EDYOS_19')
      ENDIF
C
      IF (INFO.EQ.20) THEN
        CALL U2MESS('F','EDYOS_20')
      ENDIF
C
      IF (INFO.EQ.21) THEN
        CALL U2MESS('F','EDYOS_21')
      ENDIF
C
      IF (INFO.EQ.22) THEN
        CALL U2MESS('F','EDYOS_22')
      ENDIF
C
      IF (INFO.EQ.23) THEN
        CALL U2MESS('F','EDYOS_23')
      ENDIF
C
      IF (INFO.EQ.24) THEN
        CALL U2MESS('F','EDYOS_24')
      ENDIF
C
      IF (INFO.EQ.25) THEN
        CALL U2MESS('F','EDYOS_25')
      ENDIF
C
      IF (INFO.EQ.26) THEN
        CALL U2MESS('F','EDYOS_26')
      ENDIF
C
      IF (INFO.EQ.27) THEN
        CALL U2MESS('F','EDYOS_27')
      ENDIF
C
      IF (INFO.EQ.28) THEN
        CALL U2MESS('F','EDYOS_28')
      ENDIF
C
      IF (INFO.EQ.29) THEN
        CALL U2MESS('F','EDYOS_29')
      ENDIF
C
      IF (INFO.EQ.30) THEN
        CALL U2MESS('F','EDYOS_30')
      ENDIF
C
      IF (INFO.EQ.31) THEN
        CALL U2MESS('F','EDYOS_31')
      ENDIF
C
      IF (INFO.EQ.32) THEN
        CALL U2MESS('F','EDYOS_32')
      ENDIF
C
      IF (INFO.EQ.33) THEN
        CALL U2MESS('F','EDYOS_33')
      ENDIF
C
      IF (INFO.EQ.34) THEN
        CALL U2MESS('F','EDYOS_34')
      ENDIF
C
      IF (INFO.EQ.35) THEN
        CALL U2MESS('F','EDYOS_35')
      ENDIF
C
      IF (INFO.EQ.36) THEN
        CALL U2MESS('F','EDYOS_36')
      ENDIF
C
      IF (INFO.EQ.37) THEN
        CALL U2MESS('F','EDYOS_37')
      ENDIF
C
      IF (INFO.EQ.38) THEN
        CALL U2MESS('F','EDYOS_38')
      ENDIF
C
      IF (INFO.EQ.39) THEN
        CALL U2MESS('F','EDYOS_39')
      ENDIF
C
      IF (INFO.EQ.40) THEN
        CALL U2MESS('F','EDYOS_40')
      ENDIF
C
C     SORTIE DE L'EXECUTION
C     =====================
C
 9999 CONTINUE
      CALL JEDEMA()
C
      END
