      SUBROUTINE NMFPAS(FONACT,SDDYNA,SDPILO,SDDISC,NBITER,
     &                  NUMINS,ETA   ,VALINC,SOLALG,VEASSE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*19 SOLALG(*),VALINC(*),VEASSE(*)
      CHARACTER*19 SDDYNA,SDPILO,SDDISC
      REAL*8       ETA
      INTEGER      NBITER,NUMINS
      INTEGER      FONACT(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C MISE A JOUR DES INFORMATIONS POUR UN NOUVEAU PAS DE TEMPS
C
C ----------------------------------------------------------------------
C
C
C IN  SDDYNA : SD DYNAMIQUE
C IN  SDPILO : SD PILOTAGE
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  ETA    : PARAMETRE DE PILOTAGE
C IN  NUMINS : NUMERO D'INSTANT
C IN  NBITER : NOMBRE D'ITERATIONS DE NEWTON
C IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
C IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
C IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
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
      LOGICAL      NDYNLO,LDYNA,LMPAS
      LOGICAL      ISFONC,LPILO
      CHARACTER*19 DEPMOI,VARMOI,SIGMOI,COMMOI,VITMOI,ACCMOI
      CHARACTER*19 DEPPLU,VARPLU,SIGPLU,COMPLU,VITPLU,ACCPLU
      CHARACTER*19 DEPDEL,DEPOLD,STRMOI,STRPLU
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- FONCTIONNALITES ACTIVEES
C
      LDYNA  = NDYNLO(SDDYNA,'DYNAMIQUE')
      LPILO  = ISFONC(FONACT,'PILOTAGE')
      LMPAS  = NDYNLO(SDDYNA,'MULTI_PAS')
C
C --- DECOMPACTION DES VARIABLES CHAPEAUX
C
      CALL NMCHEX(VALINC,'VALINC','DEPMOI',DEPMOI)
      CALL NMCHEX(VALINC,'VALINC','VITMOI',VITMOI)
      CALL NMCHEX(VALINC,'VALINC','ACCMOI',ACCMOI)
      CALL NMCHEX(VALINC,'VALINC','SIGMOI',SIGMOI)
      CALL NMCHEX(VALINC,'VALINC','VARMOI',VARMOI)
      CALL NMCHEX(VALINC,'VALINC','COMMOI',COMMOI)
      CALL NMCHEX(VALINC,'VALINC','STRMOI',STRMOI)
      CALL NMCHEX(VALINC,'VALINC','DEPPLU',DEPPLU)
      CALL NMCHEX(VALINC,'VALINC','VITPLU',VITPLU)
      CALL NMCHEX(VALINC,'VALINC','ACCPLU',ACCPLU)
      CALL NMCHEX(VALINC,'VALINC','SIGPLU',SIGPLU)
      CALL NMCHEX(VALINC,'VALINC','VARPLU',VARPLU)
      CALL NMCHEX(VALINC,'VALINC','COMPLU',COMPLU)
      CALL NMCHEX(VALINC,'VALINC','STRPLU',STRPLU)
      CALL NMCHEX(SOLALG,'SOLALG','DEPDEL',DEPDEL)
      CALL NMCHEX(SOLALG,'SOLALG','DEPOLD',DEPOLD)
C
C --- ADAPTATION DU NOUVEAU PAS DE TEMPS
C
      CALL NMADAT(SDDISC,NUMINS,NBITER,VALINC)
C
C --- POUR UNE PREDICTION PAR EXTRAP. DES INCREMENTS DU PAS D'AVANT
C
      CALL COPISD('CHAMP_GD','V',DEPDEL,DEPOLD)
C
C --- ETAT AU DEBUT DU NOUVEAU PAS DE TEMPS
C
      CALL COPISD('CHAMP_GD','V',DEPPLU,DEPMOI)
      CALL COPISD('CHAMP_GD','V',SIGPLU,SIGMOI)
      CALL COPISD('CHAMP_GD','V',VARPLU,VARMOI)
      CALL COPISD('VARI_COM','V',COMPLU,COMMOI)
      CALL COPISD('CHAMP_GD','V',STRPLU,STRMOI)
C
C --- ETAT AU DEBUT DU NOUVEAU PAS DE TEMPS EN DYNAMIQUE
C
      IF (LDYNA) THEN
        CALL COPISD('CHAMP_GD','V',VITPLU,VITMOI)
        CALL COPISD('CHAMP_GD','V',ACCPLU,ACCMOI)
      ENDIF
C
C --- REACTUALISATION DES BORNES DE PILOTAGE SI DEMANDE
C
      IF (LPILO) THEN
        CALL NMPIAC(SDPILO,ETA   )
      ENDIF
C
C --- SAUVEGARDE DU SECOND MEMBRE SI MULTI_PAS EN DYNAMIQUE
C
      IF (LMPAS) THEN
        CALL NMCHSV(FONACT,VEASSE,SDDYNA)
      ENDIF
C
      CALL JEDEMA()
C
      END
