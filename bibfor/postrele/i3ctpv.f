      SUBROUTINE I3CTPV(EPSI,NOEUD,NBN,COORDO,PAVE,COUPE)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER NOEUD(*),NBN
      REAL*8  EPSI,COORDO(*),PAVE(*)
      LOGICAL COUPE
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     ------------------------------------------------------------------
C     REPONSE A LA QUESTION : UN PAVE DONNE COUPE-T-IL LE PLUS PETIT
C     PAVE CONTENANT UN ENSEMBLE DE NOEUDS
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  NOEUD  : I : TABLE DES NOEUDS
C IN  NBN    : I : NOMBRE DE NOEUDS
C IN  COORDO : R : TABLE DES COORDONNEES
C IN  PAVE   : R : DESCRIPTEUR DU PAVE (XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX)
C OUT COUPE  : L : REPONSE
C     ------------------------------------------------------------------
C
      INTEGER I,J
      REAL*8  XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,A1,A2,B1,B2,C1,C2
C
C======================================================================
C
      XMIN =  1.0D50
      XMAX = -1.0D50
      YMIN =  1.0D50
      YMAX = -1.0D50
      ZMIN =  1.0D50
      ZMAX = -1.0D50
      DO 100, I = 1, NBN, 1
         J    = 3*(NOEUD(I)-1)
         XMIN = MIN(XMIN,COORDO(J+1))
         XMAX = MAX(XMAX,COORDO(J+1))
         YMIN = MIN(YMIN,COORDO(J+2))
         YMAX = MAX(YMAX,COORDO(J+2))
         ZMIN = MIN(ZMIN,COORDO(J+3))
         ZMAX = MAX(ZMAX,COORDO(J+3))
100   CONTINUE
      A1 = MAX(PAVE(1),XMIN)
      A2 = MIN(PAVE(4),XMAX)
      B1 = MAX(PAVE(2),YMIN)
      B2 = MIN(PAVE(5),YMAX)
      C1 = MAX(PAVE(3),ZMIN)
      C2 = MIN(PAVE(6),ZMAX)
      COUPE = (( (A1.LE.A2) .OR. (ABS(A1-A2).LE.EPSI) ).AND.
     +         ( (B1.LE.B2) .OR. (ABS(B1-B2).LE.EPSI) ).AND.
     +         ( (C1.LE.C2) .OR. (ABS(C1-C2).LE.EPSI) ))
      END
