      SUBROUTINE FGLEMA(NBF,NBPOIN,SIG,DEFPLA,TEMP,NOMMAT,DOM)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)                NOMMAT
      REAL*8                       SIG(*),DEFPLA(*),TEMP(*),DOM(*)
      INTEGER           NBF,NBPOIN
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/03/2004   AUTEUR VABHHTS J.PELLET 
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
C     -----------------------------------------------------------------
C     CALCUL DU DOMMAGE DE LEMAITRE-SERMAGE
C     NOTE: Routine identique à "CALCUL DU DOMMAGE DE LEMAITRE"
C           avec prise en compte de l'exposant EXP_S dans la loi
C           d'évolution et VSEUIL dependant de la température.
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C IN  NBF    : I   : NOMBRE DE FONCTIONS DECRIVANT LE CHARGEMENT
C IN  NBPOIN : I   : NOMBRE DE POINTS DE L'HISTOIRE DE CHARGEMENT
C IN  SIG    : R   : VALEURS TENSEUR CONTRAINTES
C IN  DEFPLA : R   : VALEURS DEFORMATION PLASTIQUE CUMULEE
C IN  TEMP   : R   : VALEURS TEMPERATURE
C IN  NOMMAT : K   : NOM DU MATERIAU
C OUT DOM    : R   : VALEURS DU DOMMAGE A CHAQUE INSTANT
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C
      CHARACTER*2   CODRET(3)
      CHARACTER*8   NOMRES(3),NOMPAR
      CHARACTER*16  PHENO,PHENOM
      REAL*8        VALMOI(3),VALPLU(3),TEMMOI,TEMPLU,PMOI,PPLU
      REAL*8        SIHMOI,SIHPLU,SEQMOI,SEQPLU,VMOI,VPLU,VALE
      REAL*8        VSEUIL,EXPS,EXPO
      REAL*8        NULL,UN,DEUX,TROIS
      DATA ZERO /1.D-13/
C
      CALL JEMARQ()
C
      NULL = 0.D0
      UN = 1.D0
      DEUX = 2.D0
      TROIS = 3.D0

      PHENO = 'DOMMA_LEMAITRE'
      CALL RCCOME (NOMMAT,PHENO,PHENOM,CODRET(1))
      IF(CODRET(1).EQ.'NO') CALL UTMESS('F','POST_FATIGUE',
     +  'POUR CALCULER LE DOMMAGE DE LEMAITRE-SERMAGE IL FAUT DEFINIR '
     +  //' LE COMPORTEMENT DOMMA_LEMAITRE DANS DEFI_MATERIAU')
      PHENO = 'ELAS'
      CALL RCCOME (NOMMAT,PHENO,PHENOM,CODRET(1))
      IF(CODRET(1).EQ.'NO') CALL UTMESS('F','POST_FATIGUE',
     +  'POUR CALCULER LE DOMMAGE DE LEMAITRE_SERMAGE IL FAUT DEFINIR '
     +  //' LE COMPORTEMENT ELAS_FO DANS DEFI_MATERIAU')
C
C --- CALCUL DU DOMMAGE ELEMENTAIRE
C
      CALL WKVECT('&&FGLEMA.DEVIAT','V V R',NBF*NBPOIN,IDEV)
      CALL FMDEVI(NBF,NBPOIN,SIG,ZR(IDEV))
C
      DOM(1) = NULL
C
      DO 10 I= 1,NBPOIN-1
C
C --- RECUPERATION DE P,SIG,TEMP AUX INSTANTS TI ET TI+1
C
C --- RECUPERATION DE EXP_S
        NOMRES(1) = 'EXP_S'
        CALL RCVALE(NOMMAT,'DOMMA_LEMAITRE',
     +              0,' ',RBID,1,NOMRES(1),
     +              EXPS,CODRET(1),'F ')
C --- RECUPERATION DU VSEUIL AUX INSTANTS TI+1
        NBPAR = 1
        NOMPAR = 'TEMP'
        TEMMOI    = TEMP(I)
        TEMPLU    = TEMP(I+1)
        NOMRES(1) = 'EPSP_SEU'
        CALL RCVALE(NOMMAT,'DOMMA_LEMAITRE',
     +              NBPAR,NOMPAR,TEMPLU,1,NOMRES(1),
     +              VSEUIL,CODRET(1),'F ')
C
        PMOI      = DEFPLA(I)
        PPLU      = DEFPLA(I+1)
C
        IF(PPLU.GT.VSEUIL-ZERO) THEN
C
C --- RECUPERATION DE E,NU,S,PD AUX INSTANTS TI ET TI+1
C
          NOMRES(1) = 'E'
          NOMRES(2) = 'NU'
          NOMRES(3) = 'S'
C
          CALL RCVALE(NOMMAT,'ELAS',
     +                NBPAR,NOMPAR,TEMMOI,2,NOMRES(1),
     +                VALMOI(1),CODRET(1),'F ')
          CALL RCVALE(NOMMAT,'ELAS',
     +                NBPAR,NOMPAR,TEMPLU,2,NOMRES(1),
     +                VALPLU(1),CODRET(1),'F ')
          CALL RCVALE(NOMMAT,'DOMMA_LEMAITRE',
     +                NBPAR,NOMPAR,TEMMOI,1,
     +                NOMRES(3),VALMOI(3),CODRET(3),'F ')
          CALL RCVALE(NOMMAT,'DOMMA_LEMAITRE',
     +                NBPAR,NOMPAR,TEMPLU,1,
     +                NOMRES(3),VALPLU(3),CODRET(3),'F ')
C
C --- CALCUL DE SIGMAH ET SIGMA EQUIVALENTE AUX INSTANTS TI ET TI+1
C
          IDE = (I-1)*NBF
          SIHMOI=(SIG(IDE+1)+SIG(IDE+2)+SIG(IDE+3))/TROIS
          SIHMOI=SIHMOI**2
          IF(NBF.EQ.6) THEN
            SEQMOI=(ZR(IDEV+IDE)*ZR(IDEV+IDE)+ZR(IDEV+IDE+1)*
     +        ZR(IDEV+IDE+1)+ZR(IDEV+IDE+2)*ZR(IDEV+IDE+2))/DEUX
     +        +ZR(IDEV+IDE+3)*ZR(IDEV+IDE+3)+ZR(IDEV+IDE+4)*
     +        ZR(IDEV+IDE+4)+ZR(IDEV+IDE+5)*ZR(IDEV+IDE+5)
          ELSEIF(NBF.EQ.4) THEN
            SEQMOI=(ZR(IDEV+IDE)*ZR(IDEV+IDE)+ZR(IDEV+IDE+1)*
     +        ZR(IDEV+IDE+1)+ZR(IDEV+IDE+2)*ZR(IDEV+IDE+2))/DEUX
     +        +ZR(IDEV+IDE+3)*ZR(IDEV+IDE+3)
          ENDIF
          SEQMOI = TROIS*SEQMOI
          IDE =  I*NBF
          SIHPLU=(SIG(IDE+1)+SIG(IDE+2)+SIG(IDE+3))/TROIS
          SIHPLU=SIHPLU**2
          IF(NBF.EQ.6) THEN
            SEQPLU=(ZR(IDEV+IDE)*ZR(IDEV+IDE)+ZR(IDEV+IDE+1)*
     +        ZR(IDEV+IDE+1)+ZR(IDEV+IDE+2)*ZR(IDEV+IDE+2))/DEUX
     +        +ZR(IDEV+IDE+3)*ZR(IDEV+IDE+3)+ZR(IDEV+IDE+4)*
     +        ZR(IDEV+IDE+4)+ZR(IDEV+IDE+5)*ZR(IDEV+IDE+5)
          ELSEIF(NBF.EQ.4) THEN
            SEQPLU=(ZR(IDEV+IDE)*ZR(IDEV+IDE)+ZR(IDEV+IDE+1)*
     +        ZR(IDEV+IDE+1)+ZR(IDEV+IDE+2)*ZR(IDEV+IDE+2))/DEUX
     +        +ZR(IDEV+IDE+3)*ZR(IDEV+IDE+3)
          ENDIF
          SEQPLU = TROIS*SEQPLU
C
          VMOI = ((UN/TROIS)*(UN+VALMOI(2))*SEQMOI)
          VMOI = VMOI+((TROIS/DEUX)*(UN-DEUX*VALMOI(2))*SIHMOI)
          VMOI = (UN/(VALMOI(1)*VALMOI(3)))*VMOI
          VMOI = VMOI**EXPS

C
          VPLU = ((UN/TROIS)*(UN+VALPLU(2))*SEQPLU)
          VPLU = VPLU+((TROIS/DEUX)*(UN-DEUX*VALPLU(2))*SIHPLU)
          VPLU = (UN/(VALPLU(1)*VALPLU(3)))*VPLU
          VPLU = VPLU**EXPS

C
          VALE = (UN/DEUX)*(VMOI+VPLU)
          VALE = VALE * (PPLU-PMOI)
C
          EXPO = DEUX*EXPS+UN
          IF (DOM(I).GT.UN) THEN
             VALE = -EXPO*VALE
          ELSE
             VALE =  (UN-DOM(I))**EXPO - EXPO*VALE
          ENDIF
C
          IF(VALE.GT.NULL) THEN
            DOM(I+1) = UN - (VALE)**(UN/EXPO)
          ELSE
            DOM(I+1) = UN + (-VALE)**(UN/EXPO)
          ENDIF
        ELSE
          DOM(I+1) = NULL
        ENDIF
C La valeur de l'endommagement est bornée à 1
        IF (DOM(I+1).GT.UN) THEN
           DOM(I+1) = UN
        ENDIF
 10   CONTINUE
C
      CALL JEDETC('V','&&FGLEMA',1)
      CALL JEDEMA()
      END
