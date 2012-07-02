      SUBROUTINE USUBAN ( MATER, ISUPP, PARA, IER )
      IMPLICIT NONE
      REAL*8                            PARA(*)
      CHARACTER*(*)       MATER
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
C     BANQUE DE DONNEES 
C IN  : MATE   : DEFINITION DU COUPLE DE MATERIAUX
C IN  : ISUPP  : RECUPERATION DES COEFFICIENTS TUBE ( ISUPP = 1 )
C                                  OU DE L'OBSTACLE ( ISUPP = 2 )
C OUT : PARA   : DEFINITION DES COEFFICIENTS
C OUT : IER    : CODE RETOUR
C-----------------------------------------------------------------------
      CHARACTER*24 LOI, MATE, TYPE
      CHARACTER*24 VALK(3)
      INTEGER      IARG
C
C-----------------------------------------------------------------------
      INTEGER IER ,ISUPP ,LK ,LT ,LXLGUT ,N1 ,N2 

C-----------------------------------------------------------------------
      IER = 0
      MATE = MATER
      CALL GETVTX(' ','LOI_USURE' ,1,IARG,1,LOI ,N1)
      CALL GETVTX(' ','CONTACT'   ,1,IARG,1,TYPE,N2)
C
C **********************************************************************
C                 M O D E L E     A R C H A R D
C **********************************************************************
C
      IF ( LOI(1:7) .EQ. 'ARCHARD' ) THEN
         PARA(1) = 9999.D0
C
         IF ( TYPE .EQ. 'TUBE_BAV' ) THEN
         IF ( MATE .EQ. 'I600_I600' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 1.2D-13
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600TT_I600' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 4.5D-14
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600TT_I600TT' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 1.4D-15
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600_I600CR' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 7.2D-14
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600TT_I600CR' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 9.1D-16
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I690TT_I600CR' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 1.2D-15
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600_Z10C13' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 9.9D-14
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600_A405' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 6.2D-14
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I690_A405' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 4.1D-16
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600TT_Z6C13' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 9.2D-15
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600_Z6C13' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 7.1D-15
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I690_Z6C13' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 7.7D-15
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600_A347' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 1.0D-13
            ELSE
            ENDIF
         ENDIF
C
         ELSEIF ( TYPE .EQ. 'TUBE_ALESAGE' ) THEN
         IF ( MATE .EQ. 'I690_Z10C13' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 6.0D-17
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600_I600' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 1.6D-13
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I690_I600' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 5.2D-14
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600_I600CR' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 2.2D-15
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I690_I600CR' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 4.4D-15
            ELSE
            ENDIF
         ENDIF
C
         ELSEIF ( TYPE .EQ. 'TUBE_4_ENCO' ) THEN
         IF ( MATE .EQ. 'I600_Z10C13' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 2.4D-16
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I690_Z10C13' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 8.2D-17
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600_A405' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 6.5D-14
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I600TT_A405' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 1.4D-15
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I690_A405' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 7.8D-15
            ELSE
            ENDIF
         ENDIF
C
         ELSEIF ( TYPE .EQ. 'TUBE_3_ENCO' ) THEN
         IF ( MATE .EQ. 'I600_Z10C13' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 2.5D-16
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I690_Z10C13' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 2.4D-16
            ELSE
            ENDIF
         ENDIF
C
         ELSEIF ( TYPE .EQ. 'TUBE_TUBE' ) THEN
         IF ( MATE .EQ. 'I600_I600' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 1.8D-13
            ELSE
            ENDIF
         ELSEIF ( MATE .EQ. 'I690_I690' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 1.0D-12
            ELSE
            ENDIF
         ENDIF
C
         ELSEIF( TYPE .EQ. 'GRAPPE_ALESAGE' ) THEN
         IF ( MATE .EQ. 'A304L_A304L' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 10.D-15
            ELSE
               PARA(1) = 15.D-15
            ENDIF
         ELSEIF ( MATE .EQ. 'A316L_A304L' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 10.D-15
            ELSE
               PARA(1) = 5.D-15
            ENDIF
         ELSEIF ( MATE .EQ. 'NITRURE_A304L' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 0.1D-15
            ELSE
               PARA(1) = 70.D-15
            ENDIF
         ELSEIF ( MATE .EQ. 'CHROME_A304L' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 0.1D-15
            ELSE
               PARA(1) = 50.D-15
            ENDIF
         ENDIF
C
         ELSEIF( TYPE .EQ. 'GRAPPE_1_ENCO' ) THEN
         IF ( MATE .EQ. 'A304L_A304L' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 20.D-15
            ELSE
               PARA(1) = 20.D-15
            ENDIF
         ELSEIF ( MATE .EQ. 'A316L_A304L' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 30.D-15
            ELSE
               PARA(1) = 20.D-15
            ENDIF
         ENDIF
         ENDIF
C
         IF ( PARA(1).EQ.9999D0 ) THEN
            IER = IER + 1
            LK = LXLGUT(MATE)
            LT = LXLGUT(TYPE)
            VALK (1) = TYPE(1:LT)
            VALK (2) = MATE(1:LK)
            VALK (3) = ' '
            CALL U2MESG('E', 'PREPOST5_77',3,VALK,0,0,0,0.D0)
         ENDIF
C
C **********************************************************************
C                 M O D E L E     K W U _ E P R I
C **********************************************************************
C
      ELSEIF ( LOI(1:8) .EQ. 'KWU_EPRI' ) THEN
C        --- 'COEF_USURE_...' ---
         PARA(1) = 9999.D0
C        --- 'COEF_GLISSEMENT_...' ---
         PARA(2) = 9999.D0
C        --- 'COEF_IMPACT_...' ---
         PARA(3) = 9999.D0
C        --- 'CST_K_...' ---
         PARA(4) = 9999.D0
C        --- 'CST_C_...' ---
         PARA(5) = 9999.D0
         IF( TYPE .EQ. 'TUBE_BAV' ) THEN
         IF ( MATE .EQ. 'I600_I600' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 1.4D-15
               PARA(2) = 1.4D-15
               PARA(3) = 1.4D-15
               PARA(4) = 5.D0
               PARA(5) = 10.D0
            ELSE
            ENDIF
         ENDIF
         ELSEIF( TYPE .EQ. 'GRAPPE_ALESAGE' ) THEN
         ENDIF
         IF ( PARA(1).EQ.9999D0 ) THEN
            IER = IER + 1
            LK = LXLGUT(MATE)
            LT = LXLGUT(TYPE)
            VALK (1) = TYPE(1:LT)
            VALK (2) = MATE(1:LK)
            VALK (3) = ' '
            CALL U2MESG('E', 'PREPOST5_77',3,VALK,0,0,0,0.D0)
         ENDIF
C
C **********************************************************************
C                 M O D E L E     E D F _ M Z
C **********************************************************************
C
      ELSEIF ( LOI(1:6) .EQ. 'EDF_MZ' ) THEN
C        --- 'COEF_USURE_...' ---
         PARA(1) = 9999.D0
C        --- 'SEUIL_...' ---
         PARA(2) = 9999.D0
C        --- 'EXPOSANT_USURE_...' ---
         PARA(3) = 9999.D0
C        --- 'TAUX_RALENTISSEMENT_...' ---
         PARA(4) = 9999.D0
         IF( TYPE .EQ. 'TUBE_BAV' ) THEN
         IF ( MATE .EQ. 'I600_I600' ) THEN
            IF (ISUPP.EQ.1) THEN
               PARA(1) = 1.D-13
               PARA(2) = 1.14D-16
               PARA(3) = 1.2D0
               PARA(4) = 2.44D-08
            ELSE
            ENDIF
         ENDIF
         ELSEIF( TYPE .EQ. 'GRAPPE_ALESAGE' ) THEN
         ENDIF
         IF ( PARA(1).EQ.9999D0 ) THEN
            IER = IER + 1
            LK = LXLGUT(MATE)
            LT = LXLGUT(TYPE)
            VALK (1) = TYPE(1:LT)
            VALK (2) = MATE(1:LK)
            VALK (3) = ' '
            CALL U2MESG('E', 'PREPOST5_77',3,VALK,0,0,0,0.D0)
         ENDIF
C
      ENDIF
C
      END
