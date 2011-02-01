      SUBROUTINE DCHAPG(SIG1,SIG2,NPG,NBSIG,DECHA)
      IMPLICIT NONE
      INTEGER NPG,NBSIG
      REAL*8 SIG1(*),SIG2(*),DECHA(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 01/02/2011   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C     BUT:
C       CALCUL DE L'INDICATEUR LOCAL DE DECHARGE DECHA
C       I = (NORME(SIG2) - NORME(SIG1))/NORME(SIG2)
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   SIG1     : CONTRAINTES INSTANT +
C IN   SIG2     : CONTRAINTES INSTANT -
C IN   NPG      : NOMBRE DE POINT DE GAUSS
C
C      SORTIE :
C-------------
C OUT  DECHA    : INDICATEUR DE DECHARGE AU POINTS DE GAUSS
C
C ......................................................................
C
      INTEGER IGAU

      REAL*8 ZERO,UN,NORM1,NORM2,NORSIG,ZERNOR,R8PREM
C
C ----------------------------------------------------------------------
C
      ZERO = 0.0D0
      UN = 1.0D0
      ZERNOR = 10.0D0*R8PREM()

      DO 10 IGAU = 1,NPG
C      CALCUL DU SECOND INVARIANT DU TENSEUR DES CONTRAINTES :
C
        NORM1 = NORSIG(SIG1(1+ (IGAU-1)*NBSIG),NBSIG)
        NORM2 = NORSIG(SIG2(1+ (IGAU-1)*NBSIG),NBSIG)

C     DANS LE CAS OU NORME(SIG2) = 0 :
C     SI NORME(SIG1) = 0, ON MET L'INDICATEUR A 0
C     SINON IL Y A EU DECHARGE ET ON MET L'INDICATEUR A -1 :
C
        IF (NORM2.LE.ZERNOR) THEN
          IF (NORM1.LE.ZERNOR) THEN
            DECHA(IGAU) = ZERO
          ELSE
            DECHA(IGAU) = -UN
          END IF
        ELSE
          DECHA(IGAU) = (NORM2-NORM1)/NORM2
        END IF
  10  CONTINUE
C
      END
