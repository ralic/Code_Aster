      SUBROUTINE ORIEN2 ( XP, XQ, XR, ANGL )
      IMPLICIT  REAL*8  ( A-H , O-Z )
      REAL*8              XP(*), XQ(*), XR(*), ANGL(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C     ORIENTATION D'UN TRIEDRE(XQ,XP,XR) DEFINI PAR TROIS POINTS (X,Y)
C ----------------------------------------------------------------------
C IN  : X..    : COORDONNEES DES POINTS
C OUT : A B G  : ANGLES D'ORIENTATION DE L'AXE
C ----------------------------------------------------------------------
      REAL*8   XPR(3), XPQ(3), XXPR(3), MRO(3,3) 
C ----------------------------------------------------------------------
      ZERO = 0.D0
C
      R = ZERO
      S = ZERO
      DO 10 I = 1,3
         XPQ(I) = XQ(I) - XP(I)
         R = R + XPQ(I)*XPQ(I)
         XPR(I) = XR(I) - XP(I)
         S = S + XPR(I)*XPR(I)
 10   CONTINUE
      IF ( R .EQ. ZERO ) CALL UTMESS('F','ORIEN2','POINTS CONFONDUS.')
      IF ( S .EQ. ZERO ) CALL UTMESS('F','ORIEN2','POINTS CONFONDUS.')
      R = SQRT( R )
      S = SQRT( S )
      CALL ORIEN1 ( XP, XQ, ANGL )
      CALL MATROT ( ANGL , MRO )
      CALL PMAVEC ( 'ZERO', 3, MRO, XPR, XXPR )
      IF ( XXPR(2).EQ.ZERO .AND. XXPR(3).EQ.ZERO )THEN
         ANGL(3) = ZERO
      ELSE
         ANGL(3) = ATAN2 ( XXPR(3) , XXPR(2) )
      ENDIF
C
      END
