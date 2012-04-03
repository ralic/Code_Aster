      SUBROUTINE NMADCP(SDDISC,DEFICO,RESOCO,IEVDAC,RETPEN)
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
      INTEGER      IEVDAC
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*19 SDDISC
      INTEGER      RETPEN
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C GESTION DE L'ACTION ADAPTATION COEF. PENALISATION
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  IEVDAC : INDICE DE L'EVENEMENT ACTIF
C OUT RETPEN : CODE RETOUR ADAPTATION PENALISATION
C               0 ON N'A PAS ADAPTE
C               1 ON A ADAPTE
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
      INTEGER      IBID
      CHARACTER*8  K8BID
      REAL*8       PENMAX,COEFPN,NEWCOE
      REAL*8       CMMAXI
      REAL*8       JEUMIN,JEUMAX,JEUFIN
      INTEGER      CFDISD,CFDISI,NBLIAI,NZOCO
      INTEGER      ILIAI,IZONE
      CHARACTER*24 JEUITE,NUMLIA
      INTEGER      JJEUIT,JNUMLI
      CHARACTER*24 CTEVPE
      INTEGER      JCTEVP
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      RETPEN = 1
      CALL UTDIDT('L'   ,SDDISC,'ECHE',IEVDAC,'PENE_MAXI',
     &            PENMAX,IBID  ,K8BID )
      CALL UTDIDT('L'   ,SDDISC,'ECHE',IEVDAC,'COEF_MAXI',
     &            CMMAXI,IBID  ,K8BID )
C
C --- PARAMETRES
C
      NBLIAI = CFDISD(RESOCO,'NBLIAI')
      NZOCO  = CFDISI(DEFICO,'NZOCO' )
C
C --- ACCES OBJETS DU CONTACT
C
      JEUITE = RESOCO(1:14)//'.JEUITE'
      NUMLIA = RESOCO(1:14)//'.NUMLIA'
      CALL JEVEUO(JEUITE,'L',JJEUIT)
      CALL JEVEUO(NUMLIA,'L',JNUMLI)
      CTEVPE = RESOCO(1:14)//'.EVENPE'
      CALL JEVEUO(CTEVPE,'E',JCTEVP)
C
C --- DETECTION PENETRATION MAXIMUM/MINIMUM
C
      DO 10 ILIAI = 1,NBLIAI
        JEUFIN = ZR(JJEUIT+3*(ILIAI-1)+1-1)
        IZONE  = ZI(JNUMLI+4*(ILIAI-1)+4-1)
        JEUMIN = ZR(JCTEVP+3*(IZONE-1)+1-1)
        JEUMAX = ZR(JCTEVP+3*(IZONE-1)+2-1)
        IF (JEUFIN.LE.0.D0) THEN
          JEUFIN = ABS(JEUFIN)
          JEUMAX = MAX(JEUMAX,JEUFIN)
        ELSE
          JEUMIN = MAX(JEUMIN,JEUFIN)
        ENDIF
        ZR(JCTEVP+3*(IZONE-1)+1-1) = JEUMIN
        ZR(JCTEVP+3*(IZONE-1)+2-1) = JEUMAX
        ZR(JCTEVP+3*(IZONE-1)+3-1) = JEUFIN
  10  CONTINUE
C
C --- DETECTION PENETRATION MAXIMUM
C
      DO 15 IZONE = 1,NZOCO
        CALL CFMMCO(DEFICO,RESOCO,IZONE,'E_N','L',
     &              COEFPN)
        IF (JEUMAX.GT.PENMAX) THEN
          NEWCOE = COEFPN*2.D0
          IF (NEWCOE.GT.CMMAXI) THEN
            NEWCOE = CMMAXI
            RETPEN = 0
          ENDIF
          CALL CFMMCO(DEFICO,RESOCO,IZONE,'E_N','E',
     &                NEWCOE)
        ENDIF
        IF (RETPEN.EQ.1) THEN
          CALL U2MESG('I','MECANONLINE6_54',0,' ',1,IZONE,1,NEWCOE)
        ENDIF
  15  CONTINUE
C
C --- AFFICHAGE
C
      IF (RETPEN.EQ.0) THEN
        CALL U2MESS('I','MECANONLINE6_53')
      ELSEIF (RETPEN.EQ.1) THEN
        CALL U2MESS('I','MECANONLINE6_52')
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
      END
