      SUBROUTINE ORIEM0 ( NOMA,COOR, LINO1,NBNO1, LINO2,NBNO2, PREC,IER)
      IMPLICIT   NONE
      INTEGER             LINO1(*),NBNO1, LINO2(*),NBNO2, IER
      REAL*8              PREC, COOR(*)
      CHARACTER*(*)       NOMA
C.======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/03/2006   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C     SI LES MAILLES ONT LES MEMES SUPPORTS (DUPLICATION DES MAILLES)
C     COORDONNEES DES NOEUDS IDENTIQUES : IER = 0
C                                 SINON : IER = 1
C
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : LINO1  : LISTE DES NOEUDS DE LA MAILLE 1
C IN  : NBNO1  : NB DE NOEUDS DE LINO1
C IN  : LINO2  : LISTE DES NOEUDS DE LA MAILLE 2
C IN  : NBNO2  : NB DE NOEUDS DE LINO2
C IN  : PREC   : PRECISION
C OUT : IER    : = 0  MAILLE IDENTIQUE
C                = 1  MAILLE DIFFERENTE
C
C.========================= DEBUT DES DECLARATIONS ====================
C
      INTEGER      INO, N1, N2, IC
      REAL*8       X1, X2
C
C ========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      IER  = 1
C
      IF ( NBNO1.NE.NBNO2 ) GOTO 9999
C
C --- VERIFICATION DES COORDONNEES :
C     ----------------------------
      DO 10 INO = 1 , NBNO1
         N1 = LINO1(INO)
         N2 = LINO2(INO)
         DO 12 IC = 1 , 3
            X1 = COOR(3*(N1-1)+IC)
            X2 = COOR(3*(N2-1)+IC)
            IF ( X2 .EQ. 0.D0 ) THEN
               IF ( ABS(X1) .GT. PREC ) GOTO 9999
            ELSE
               IF ( ABS((X1-X2)/X2) .GT. PREC ) GOTO 9999
            ENDIF
 12      CONTINUE
 10   CONTINUE
C
      IER = 0
C
 9999 CONTINUE
C
      END
