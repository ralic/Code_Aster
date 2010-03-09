      SUBROUTINE HYPMAT(FAMI  ,KPG   ,KSP   ,POUM  ,IMATE ,
     &                  C10   ,C01   ,C20   ,K     )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/03/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 2005 UCBL LYON1 - T. BARANGER     WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*(*)  FAMI,POUM
      INTEGER        KPG,KSP,IMATE
      REAL*8         C10,C01,C20
      REAL*8         K
C
C ----------------------------------------------------------------------
C
C LOI DE COMPORTEMENT HYPERELASTIQUE DE SIGNORINI
C
C C10 (I1-3) + C01 (I2-3)+ C20 (I1-3)^2 + K/2(J-1)²
C
C RECUPERE DONNEES MATERIAUX
C
C ----------------------------------------------------------------------
C
C  
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  FAMI    : FAMILLE DE POINTS DE GAUSS
C IN  KPG     : NUMERO DU POINT DE GAUSS
C IN  KSP     : NUMERO DU SOUS-POINT DE GAUSS
C IN  POUM    : '-' POUR VARIABLES DE COMMANDE
C OUT C10     : PARAMETRE LOI DE COMPORTEMENT
C OUT C02     : PARAMETRE LOI DE COMPORTEMENT
C OUT C20     : PARAMETRE LOI DE COMPORTEMENT 
C OUT K       : MODULE DE COMPRESSIBILITE
C
C ----------------------------------------------------------------------
C
      INTEGER     NBRES
      PARAMETER   ( NBRES = 3  )
      CHARACTER*8 NOMRES(NBRES)
      CHARACTER*2 CODRES(NBRES)
      REAL*8      VALRES(NBRES)
      CHARACTER*8 BL2,FB2,AB2
      REAL*8      NU
      REAL*8      R8PREM,DENOM
      LOGICAL     CMPK
C
C ----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      BL2    = '  '
      FB2    = 'F '
      AB2    = '  '
C
C --- SOIT ON PREND LE K DONNE PAR DEFI_MATERIAU, SOIT ON LE CALCULE
C --- A PARTIR DU NU
C
      NOMRES(1)   = 'K'
      CALL RCVALB(FAMI  ,KPG   ,KSP   ,POUM  ,IMATE ,
     &            BL2   ,'ELAS_HYPER' ,0     ,' '   ,
     &            0.D0  ,1            ,NOMRES,VALRES,
     &            CODRES,AB2 )

      IF (CODRES(1).EQ.'OK') THEN
        K         = VALRES(1)
        CMPK      = .FALSE.
      ELSE
        NOMRES(1) = 'NU'
        CALL RCVALB(FAMI  ,KPG   ,KSP   ,POUM  ,IMATE ,
     &              BL2   ,'ELAS_HYPER' ,0     ,' '   ,
     &              0.D0  ,1            ,NOMRES,VALRES,
     &              CODRES,FB2 )        
        NU        = VALRES(1)
        DENOM     = 3.D0*(1.D0-2.D0*NU)
        IF (DENOM.LE.R8PREM()) THEN
          CALL U2MESS('F','ELASHYPER_98')
        ENDIF

        CMPK      = .TRUE.

      ENDIF
C
C --- RECUPERATION C10, C01 ET C20
C
      NOMRES(1) = 'C10'
      NOMRES(2) = 'C01'
      NOMRES(3) = 'C20'
      CALL RCVALB(FAMI  ,KPG   ,KSP   ,POUM  ,IMATE ,
     &            BL2   ,'ELAS_HYPER' ,0     ,' '   ,
     &            0.D0  ,NBRES        ,NOMRES,VALRES,
     &            CODRES,FB2 )  
      C10       = VALRES(1)
      C01       = VALRES(2)
      C20       = VALRES(3)
C
C --- CALCUL DU MODULE DE COMPRESSIBILITE
C
      IF (CMPK) THEN
        K = 6.D0*(C10+C01)/DENOM
      ENDIF
C

      END
