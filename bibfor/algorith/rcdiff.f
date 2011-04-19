      SUBROUTINE RCDIFF ( IMATE, COMP, TEMP, C, DIFF )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IMATE
      REAL*8                           TEMP, C, DIFF
      CHARACTER*16               COMP
C ----------------------------------------------------------------------
C     CALCUL DU COEFFICIENT DE DIFFUSION POUR LES LOI DE TYPE SECHAGE
C
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  COMP    : COMPORTEMENT
C IN  TEMP    : TEMPERATURE
C IN  C       : CONCENTRATION EN EAU
C OUT DIFF    : VALEUR DU COEFFICIENT DE DIFFUSION
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      PARAMETER        ( NBRES=10 )
      INTEGER            NBPAR
      REAL*8             VALRES(NBRES),VALPAR(2),TZ0,R8T0
      INTEGER ICODRE(NBRES)
      CHARACTER*8        NOMRES(NBRES),NOMPAR(2)
      CHARACTER*16       PHENOM
C
      CALL JEMARQ()
C
      CALL RCCOMA(IMATE,COMP(1:6),PHENOM,ICODRE)
C
      TZ0  = R8T0()
      IF(PHENOM.EQ.'SECH_GRANGER') THEN
         NBPAR = 1
         NOMPAR(1) ='TEMP'
         VALPAR(1) = TEMP
         NOMRES(1) = 'A'
         NOMRES(2) = 'B'
         NOMRES(3) = 'QSR_K'
         NOMRES(4) = 'TEMP_0_C'
         CALL RCVALA(IMATE,' ',PHENOM,NBPAR,NOMPAR,VALPAR,4,
     &                   NOMRES, VALRES, ICODRE, 1)
         DIFF = VALRES(1) * EXP(VALRES(2)*C)
     &            *((TEMP+TZ0)/(VALRES(4)+TZ0))
     &            * EXP(-VALRES(3)
     &            *(1.D0/(TEMP+TZ0)-1.D0/(VALRES(4)+TZ0)))
C
      ELSEIF(PHENOM.EQ.'SECH_MENSI') THEN
         NBPAR = 1
         NOMPAR(1) ='TEMP'
         VALPAR(1) = TEMP
         NOMRES(1) = 'A'
         NOMRES(2) = 'B'
         CALL RCVALA(IMATE,' ',PHENOM,NBPAR,NOMPAR,VALPAR,2,
     &                   NOMRES, VALRES, ICODRE, 1)
         DIFF = VALRES(1) * EXP(VALRES(2)*C)
C
      ELSEIF(PHENOM.EQ.'SECH_BAZANT') THEN
         NBPAR = 1
         NOMPAR(1) ='TEMP'
         VALPAR(1) = C
         NOMRES(1) = 'D1'
         NOMRES(2) = 'ALPHA_BA'
         NOMRES(3) = 'N'
         NOMRES(4) = 'FONC_DES'
         CALL RCVALA(IMATE,' ',PHENOM,NBPAR,NOMPAR,VALPAR,4,
     &                   NOMRES, VALRES, ICODRE, 1)
         RAP = ((1.D0 - VALRES(4)) / 0.25D0) ** VALRES(3)
         DIFF = VALRES(1) * (VALRES(2)+ (1.D0 - VALRES(2))/(1.D0+RAP))
C
      ELSEIF(PHENOM.EQ.'SECH_NAPPE') THEN
         NBPAR = 2
         NOMPAR(1) = 'TEMP'
         VALPAR(1) =  C
         NOMPAR(2) = 'TSEC'
         VALPAR(2) =  TEMP
         NOMRES(1) = 'FONCTION'
         CALL RCVALA(IMATE,' ',PHENOM,NBPAR,NOMPAR,VALPAR,1,
     &                   NOMRES, VALRES, ICODRE, 1)
         DIFF = VALRES(1)
C
      ELSE
         CALL U2MESK('F','ALGORITH10_20',1,COMP)
      ENDIF
C
C
      CALL JEDEMA()
      END
