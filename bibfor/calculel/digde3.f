      INTEGER FUNCTION DIGDE3(MODELO,LOUC)
      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 06/05/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PELLET J.PELLET
C     ARGUMENTS:
C     ----------
      INTEGER MODELO
      CHARACTER*1 LOUC
C ----------------------------------------------------------------------
C     ENTREES:
C        MODELO : MODE_LOCAL DE TYPE "MATRICE"
C                 (SON INDICE DANS &CATA.TE.MODELOC )
C        LOUC   : / 'L' : LIGNE
C                 / 'C' : COLONNE
C     SORTIES:
C        DIGDE3 : NOMBRE DE LIGNES (OU COLONNES) DE LA MATRICE
C                 ELEMENTAIRE

C ----------------------------------------------------------------------
      INTEGER IAOPTT,LGCO,IAOPMO,ILOPMO,IAOPNO,ILOPNO,IAOPDS,
     +       IAOPPA,NPARIO,NPARIN,IAMLOC,ILMLOC,IADSGD
      COMMON /CAII02/IAOPTT,LGCO,IAOPMO,ILOPMO,IAOPNO,ILOPNO,IAOPDS,
     +       IAOPPA,NPARIO,NPARIN,IAMLOC,ILMLOC,IADSGD

C     VARIABLES LOCALES:
C     ------------------
      INTEGER JMODLO,MOD1,JMOD1
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      INTEGER ZI

C DEB-------------------------------------------------------------------
      CALL ASSERT (LOUC.EQ.'C' .OR. LOUC.EQ.'L')

      JMODLO = IAMLOC - 1 + ZI(ILMLOC-1+MODELO)
      CALL ASSERT(ZI(JMODLO-1+1).EQ.5)

      IF (LOUC.EQ.'C') THEN
        MOD1=ZI(JMODLO-1+4)
      ELSE
        MOD1=ZI(JMODLO-1+5)
      ENDIF
      JMOD1 = IAMLOC - 1 + ZI(ILMLOC-1+MOD1)
      CALL ASSERT(ZI(JMOD1-1+1).EQ.2)
      DIGDE3 = ZI(JMOD1-1+3)
   10 CONTINUE
      END
