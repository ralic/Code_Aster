      SUBROUTINE NMELRU(FAMI,KPG,KSP,POUM,IMATE,COMPOR,EPSEQ,P,DIVU,
     &                  NONLIN,ENER)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C
C FONCTION REALISEE:
C
C     REALISE LE CALCUL DE L'ENERGIE LIBRE, DE LA DERIVEE DE L'ENERGIE
C             LIBRE PAR RAPPORT A LA TEMPERATURE (POUR LE CALCUL DE G)
C     ET DE SA DERIVEE PAR RAPPORT A UNE VARIATION DE DOMAINE (EN DP
C     ELASTIQUE ISOTROPE LINEAIRE).
C
C IN  IMATE   : NATURE DU MATERIAU
C IN  COMPOR  : COMPORTEMENT
C IN  EPSEQ   : DEFORMATION EQUIVALENTE
C IN  P       : DEFORMATION ELASTIQUE CUMULEE
C IN  DIVU    : TRACE DES DEFORMATIONS
C IN  NONLIN  : NON LINEARITE DU MATERIAU
C OUT ENER(1) : ENERGIE LIBRE (POUR LE CALCUL DE G)
C OUT ENER(2) : DERIVEE DE L'ENERGIE LIBRE / TEMPERATURE
C
C ----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INCLUDE 'jeveux.h'
      INTEGER       KPG,KSP,IMATE
      REAL*8        EPSEQ,P,DIVU,ENER(2)
      CHARACTER*(*) FAMI,POUM
      CHARACTER*16  COMPOR(*)
      LOGICAL       NONLIN

C DECLARATION VARIABLES LOCALES
      INTEGER       ICODRE(3)
      INTEGER       JPROL,JVALE,NBVALE,IRET1,IRET2

      REAL*8        TEMP,TREF
      REAL*8        E,NU,DEMU,K,K3,ALPHA,DUM,RESU
      REAL*8        DE,DNU,DEMUDT,DK,DALPHA
      REAL*8        DSDE,SIGY,RPRIM,RP,AIREP,COCO
      REAL*8        DSDEDT,DSIGY,DRPRIM,DP,DRP,DAIREP
      REAL*8        NRJ,DNRJ,VALRES(3),DEVRES(3)

      CHARACTER*8   NOMRES(3),TYPE

      LOGICAL       TRAC,LINE,PUIS

      COMMON        /RCONM2/ALFAFA,UNSURN,SIELEQ
      REAL*8        ALFAFA,UNSURN,SIELEQ
C
C ----------------------------------------------------------------------
C
      TRAC = (COMPOR(1)(1:14).EQ.'ELAS_VMIS_TRAC')
      LINE = (COMPOR(1)(1:14).EQ.'ELAS_VMIS_LINE')
      PUIS = (COMPOR(1)(1:14).EQ.'ELAS_VMIS_PUIS')

C====================================================================
C -  LECTURE DE E, NU, ALPHA ET DERIVEES / TEMPERATRURE
C====================================================================
      CALL RCVARC(' ','TEMP',POUM,FAMI,KPG,KSP,TEMP,IRET1)
      CALL RCVARC(' ','TEMP','REF',FAMI,KPG,KSP,TREF,IRET2)
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
      CALL RCVAD2 (FAMI,KPG,KSP,POUM,IMATE,'ELAS',
     &             3,NOMRES,VALRES,DEVRES,ICODRE)

      IF (ICODRE(3).NE.0) THEN
        VALRES(3)= 0.D0
        DEVRES(3)= 0.D0
      ENDIF

      E     = VALRES(1)
      NU    = VALRES(2)
      ALPHA = VALRES(3)

      DE    = DEVRES(1)
      DNU   = DEVRES(2)
      DALPHA= DEVRES(3)

      DEMU  = E/(1.D0+NU)
      DEMUDT= ((1.D0+NU)*DE-E*DNU)/(1.D0+NU)**2

      K    = E/(1.D0-2.D0*NU)/3.D0
      DK   = (DE+6.D0*K*DNU)/(1.D0-2.D0*NU)/3.D0

      K3   =  3.D0*K

C====================================================================
C - LECTURE DES CARACTERISTIQUES DE NON LINEARITE DU MATERIAU
C====================================================================

C=================================================
C CAS NON LINEAIRE
C=================================================
      IF (NONLIN) THEN
        IF (LINE) THEN

          NOMRES(1)='D_SIGM_EPSI'
          NOMRES(2)='SY'

          CALL RCVAD2(FAMI,KPG,KSP,POUM,IMATE,'ECRO_LINE',
     &                2,NOMRES,VALRES,DEVRES,ICODRE)
          IF ( ICODRE(1) .NE.0    )
     &      CALL U2MESS('F','ALGORITH7_74')
          IF ( ICODRE(2) .NE.0    )
     &      CALL U2MESS('F','ALGORITH7_75')

          DSDE  = VALRES(1)
          SIGY  = VALRES(2)
          DSDEDT= DEVRES(1)
          DSIGY = DEVRES(2)

          RPRIM  = E*DSDE/(E-DSDE)
          DRPRIM = (DE*DSDE+E*DSDEDT+RPRIM*(DSDEDT-DE))/(E-DSDE)

          P  = (DEMU*EPSEQ - SIGY) / (RPRIM+1.5D0*DEMU)
          DP = (DEMUDT*EPSEQ-DSIGY-P*(DRPRIM+1.5D0*DEMUDT))
     &                                    /(RPRIM+1.5D0*DEMU)

          RP  = SIGY +RPRIM*P
          DRP = DSIGY+DRPRIM*P+RPRIM*DP

          AIREP  = 0.5D0*(SIGY+RP)*P
          DAIREP = 0.5D0*((DSIGY+DRP)*P+(SIGY+RP)*DP)

        ELSE IF (TRAC) THEN
          SIELEQ = DEMU * EPSEQ
          CALL RCTYPE(IMATE,1,'TEMP',TEMP,RESU,TYPE)
          IF ((TYPE.EQ.'TEMP').AND.(IRET1.EQ.1))
     &        CALL U2MESS('F','CALCULEL_31')
          CALL RCTRAC(IMATE,1,'SIGM',RESU,
     &                JPROL,JVALE,NBVALE,E)
          CALL RCFONC('S',1,JPROL,JVALE,NBVALE,SIGY,DUM,DUM,
     &                 DUM,DUM,DUM,DUM,DUM,DUM)
          CALL RCFONC('E',1,JPROL,JVALE,NBVALE,DUM,E,NU,
     &                 0.D0,RP,RPRIM,AIREP,SIELEQ,P)
          DP     = 0.D0
          DRP    = 0.D0
          DAIREP = 0.D0
        ELSE IF (PUIS) THEN
          NOMRES(1)='SY'

          CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','ECRO_PUIS',
     &                0,' ',0.D0,1,NOMRES,VALRES,ICODRE,1)
          SIGY=VALRES(1)
          COCO   = E/ALFAFA/SIGY
          RP = SIGY *(COCO*P)**UNSURN+SIGY
          AIREP=SIGY*P+
     & (1.D0/(1.D0+UNSURN))*SIGY*(COCO**UNSURN)*(P**(1+UNSURN))
          DP     = 0.D0
          DRP    = 0.D0
          DAIREP = 0.D0
        ENDIF
      ENDIF

C=====================================================================
C  CALCUL DE L'ENERGIE LIBRE ET DE LA DERIVEE /TEMPERATURE
C=====================================================================

      NRJ  = 0.5D0*K*DIVU*DIVU
      IF (IRET1.EQ.0) THEN
        IF (IRET2.EQ.1) THEN
          CALL U2MESS('F','CALCULEL_31')
        ELSE
          DNRJ = 0.5D0*DK*DIVU*DIVU-K3*DIVU*(ALPHA+DALPHA*(TEMP-TREF))
       ENDIF
      ELSE
        DNRJ = 0.5D0*DK*DIVU*DIVU-K3*DIVU*ALPHA
      ENDIF

      IF (NONLIN) THEN
        ENER(1) = NRJ +RP*RP/DEMU/3.D0 + AIREP
        ENER(2) = DNRJ+RP*(DRP-DEMUDT*RP/DEMU/2.D0)/DEMU/1.5D0+DAIREP
      ELSE
        ENER(1) = NRJ +DEMU*EPSEQ*EPSEQ/3.D0
        ENER(2) = DNRJ+DEMUDT*EPSEQ*EPSEQ/3.D0
      ENDIF

      END
