      LOGICAL FUNCTION IMPLIG(NOMNOE, NBNO, LISNOE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 05/11/96   AUTEUR CIBHHGB G.BERTRAND 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      IMPLIG -- RETOURNE .TRUE. SI LE K8 NOMNOE APPARTIENT
C                                A LA LISTE DE K8 LISNOE DE LONGUEUR
C                                NBNO
C                             OU SI NBNO = 0
C
C                RETOURNE .FALSE. DANS LES AUTRES CAS
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NOMNOE          IN   K8     K8 DONT ON VEUT SAVOIR S'IL APPARTIENT
C                                AU VECTEUR DE K8 LISNOE
C    NBNO            IN   I      LONGUEUR DE LA LISTE LISNOE
C                                SI NBNO = 0 IMPLIG = .TRUE.
C    LISNOE(1)       IN   K8     VECTEUR DE K8
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      CHARACTER*8   LISNOE(1), NOMNOE
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      IMPLIG = .FALSE.
C
      IF (NBNO.EQ.0) THEN
          IMPLIG = .TRUE.
      ELSE
          DO 10 I = 1, NBNO
             IF (NOMNOE.EQ.LISNOE(I)) THEN
                 IMPLIG = .TRUE.
             ENDIF
  10      CONTINUE
      ENDIF
C
      END
