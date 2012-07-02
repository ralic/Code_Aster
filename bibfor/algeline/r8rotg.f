      SUBROUTINE R8ROTG (DA, DB, DC, DS)
      IMPLICIT NONE
      REAL *8 DA, DB, DC, DS
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C        ROTATON PLANE  (METHODE DE GIVENS).
C-----------------------------------------------------------------------
C I/O : DA   : PREMIER ELEMENT DU VECTEUR.
C         OUT: R = (+/-)SQRT(DA**2 + DB**2) OVERWRITES DA.
C     : DB   : DEUXIEME ELEMENT DU VECTEUR.
C         OUT: ZDB OU Z EST DEFINI PAR
C                 DS        SI ABS(DA) .GT. ABS(DB)
C                 1.0D0/DC  SI ABS(DB) .GE. ABS(DA) ET DC .NE. 0.0D0
C                 1.0D0     SI DC .EQ. 0.0D0.
C OUT : DC   : COEFFICIENT DE ROTATION.
C     : DC   - COEFFICIENT DE ROTATION.
C-----------------------------------------------------------------------
      REAL *8 R, U, V
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IF (ABS(DA) .GT. ABS(DB)) THEN
C                                       ABS(DA) .GT. ABS(DB)
         U = DA + DA
         V = DB/U
C                     U ET R MEME SIGNE QUE DA
         R = SQRT(.25D0+V**2)*U
C                     DC EST POSITIF
         DC = DA/R
         DS = V*(DC+DC)
         DB = DS
         DA = R
C                                   ABS(DA) .LE. ABS(DB)
      ELSE
         IF (DB .NE. 0.0D0) THEN
            U = DB + DB
            V = DA/U
C
C                   U ET R ONT MEME SIGNE QUE
C            DB (R EST IMMEDIATEMENT STOCKE DANS DA)
            DA = SQRT(.25D0+V**2)*U
C                                  DS EST POSITIF
            DS = DB/DA
            DC = V*(DS+DS)
            IF (DC .NE. 0.0D0) THEN
               DB = 1.0D0/DC
            ELSE
               DB = 1.0D0
            END IF
         ELSE
C                                   DA = DB = 0.D0
            DC = 1.0D0
            DS = 0.0D0
            DA = 0.0D0
            DB = 0.0D0
         END IF
      END IF
      END
