      SUBROUTINE PKMATE ( NDIM, COEFD, COEFD3, COEFG, COEFG3 )
      IMPLICIT   NONE
      INTEGER             NDIM
      REAL*8              COEFD, COEFD3, COEFG, COEFG3
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 31/05/2000   AUTEUR CIBHHLV L.VIVAN 
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
C
C     OPERATEUR POST_K1_K2_K3 : RECUPERTAION DES CARACTERISTIQUES
C                               MATERIAU ET DIMENSION DU PROBLEME
C     ------------------------------------------------------------------
      INTEGER       N1, NBPAR
      REAL*8        R8B, VALRES(2), E,  NU,  R8DEPI, UNMNU2, UNPNU
      CHARACTER*2   CODRET(2)
      CHARACTER*8   NOMRES, NOMPAR, NOMVAL(2), NOMMAT
      CHARACTER*16  NOMCMD, CONCEP, PHENOM, MODELI
C DEB ------------------------------------------------------------------
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
      CALL GETVID ( ' ', 'MATER', 1,1,1, NOMMAT, N1 )
      CALL RCCOME ( NOMMAT, 'ELAS', PHENOM, CODRET )
      IF ( CODRET(1) .EQ. 'NO' ) THEN
         CALL UTMESS('F',NOMCMD,'IL FAUT DEFINIR LE '//
     +                   'COMPORTEMENT "ELAS" DANS DEFI_MATERIAU')
      ENDIF
C
      NBPAR     = 0
      NOMPAR    = ' '
      NOMVAL(1) = 'E'
      NOMVAL(2) = 'NU'
      CALL RCVALE ( NOMMAT, 'ELAS', NBPAR, NOMPAR, R8B, 2,
     +              NOMVAL, VALRES, CODRET, 'F ' )
      E  = VALRES(1)
      NU = VALRES(2)
      COEFD = E * SQRT( R8DEPI() )
      UNMNU2 = 1.D0 - ( NU * NU )
      UNPNU  = 1.D0 + NU
C
      CALL GETVTX ( ' ', 'MODELISATION', 1,1,1, MODELI, N1 )
C
      IF     ( MODELI .EQ. '3D'     ) THEN
         NDIM = 3
         COEFD = COEFD / ( 8.D0 * UNMNU2 )
         COEFD3 = E*SQRT(R8DEPI()) / ( 8.D0 * UNPNU )
         COEFG = UNMNU2 / E
         COEFG3 = UNPNU / E
      ELSEIF ( MODELI .EQ. 'AXIS'   ) THEN
         NDIM = 2
         COEFD = COEFD / ( 8.D0 * UNMNU2 )
         COEFG = UNMNU2 / E
         COEFG3 = UNPNU / E
      ELSEIF ( MODELI .EQ. 'D_PLAN' ) THEN
         CALL UTMESS('I',NOMCMD,'L''OPERATEUR CALC_G_THETA_T CALCULE '//
     +                          'PLUS PRECISEMENT LES K1 K2')
         NDIM = 2
         COEFD = COEFD / ( 8.D0 * UNMNU2 )
         COEFG = UNMNU2 / E
         COEFG3 = UNPNU / E
      ELSEIF ( MODELI .EQ. 'C_PLAN' ) THEN
         CALL UTMESS('I',NOMCMD,'L''OPERATEUR CALC_G_THETA_T CALCULE '//
     +                          'PLUS PRECISEMENT LES K1 K2')
         NDIM = 2
         COEFD = COEFD / 8.D0
         COEFG = 1.D0 / E
         COEFG3 = UNPNU / E
      ELSE
         CALL UTMESS('F',NOMCMD,'MODELISATION '//MODELI//
     +                          ' NON IMPLANTEE')
      ENDIF
C
      END
