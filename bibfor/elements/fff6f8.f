      SUBROUTINE FFF6F8(NOMTEZ)
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*(*)   NOMTEZ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/03/2002   AUTEUR JMBHH01 J.M.PROIX 
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
C.......................................................................
C
C BUT :  ROUTINE D'INITIALISATION DES ELEMENTS DE PEAU
C        FAC6 ET FACE8
C
C IN  NOMTE : NOM DU TYPE ELEMENT
C.......................................................................
C
      CHARACTER*16 NOMTE
      CHARACTER*8  ALIA2D
      REAL*8       XIN(27),YIN(27),ZIN(27)
      REAL*8       XSO(8),YSO(8),ZSO(8)
      INTEGER      NBPG(10),ITRAV
      ALIA2D = ' '
C
      NOMTE = NOMTEZ
      IF( NOMTE(6:10) .EQ. 'FACE8') THEN
C
        ALIA2D = 'FACE8   '
        CALL JEEXIN( '&INEL.FACE8   .CARACTE',IRET)
        IF (IRET.GT.0) GO TO 9999
        NDIM  = 2
        NNO   = 8
        NNOS  = NNO
        NBFPG = 2
        NBPG(1) = 9
        NBPG(2) = 9
        XIN( 1) = -1.D+00
        XIN( 5) = 0.D+00
        XIN( 2) = +1.D+00
        XIN( 6) = +1.D+00
        XIN( 3) = +1.D+00
        XIN( 7) = 0.D+00
        XIN( 4) = -1.D+00
        XIN( 8) = -1.D+00
        YIN( 1) = -1.D+00
        YIN( 5) = -1.D+00
        YIN( 2) = -1.D+00
        YIN( 6) = +0.D+00
        YIN( 3) = +1.D+00
        YIN( 7) = +1.D+00
        YIN( 4) = +1.D+00
        YIN( 8) = +0.D+00
        CALL INIOBJ(NDIM,NNO,NNOS,NBFPG,ALIA2D,XIN,YIN,ZIN,XSO,YSO,ZSO,
     +            NBPG)
      ENDIF
C
      IF( NOMTE(6:10) .EQ. 'FACE6' ) THEN
C
        ALIA2D = 'FACE6   '
        CALL JEEXIN( '&INEL.FACE6   .CARACTE',IRET)
        IF (IRET.GT.0) GO TO 9999
        NDIM = 2
        NNO  = 6
        NNOS = NNO
        NBFPG= 2
        NBPG(1) = 4
        NBPG(2) = 6
        XIN( 1) = 0.D+00
        XIN( 4) = .5D+00
        XIN( 2) = 1.D+00
        XIN( 5) = .5D+00
        XIN( 3) = 0.D+00
        XIN( 6) = 0.D+00
        YIN( 1) = 0.D+00
        YIN( 4) = 0.D+00
        YIN( 2) = 0.D+00
        YIN( 5) = .5D+00
        YIN( 3) = 1.D+00
        YIN( 6) = .5D+00
        CALL INIOBJ(NDIM,NNO,NNOS,NBFPG,ALIA2D,XIN,YIN,ZIN,XSO,YSO,ZSO,
     +            NBPG)
C
      ENDIF
C
9999  CONTINUE
      END
