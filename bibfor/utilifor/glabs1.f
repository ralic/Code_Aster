      REAL*8 FUNCTION GLABS1(Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) BLAS
C ======================================================================
C REMPLACE LA BLAS DCABS1 SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
C            REMPLACEMENT DE DABS PAR ABS
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C TOLE CRP_18
C CORPS DU PROGRAMME
      IMPLICIT NONE
      COMPLEX*16 Z,ZZ
      REAL*8 T(2)
      EQUIVALENCE (ZZ,T(1))
      ZZ = Z
      GLABS1 = ABS(T(1)) + ABS(T(2))
      END
