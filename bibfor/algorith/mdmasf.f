      SUBROUTINE MDMASF( I,DNORM,MASGEN,NBMODE,PHICAR,FEXGEN,ACCGEN,
     +                   PULS2,AMOGEN,COEFA )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
C***********************************************************************
C 01/01/91    G.JACQUART AMV/P61 47 65 49 41
C***********************************************************************
C     FONCTION  : CALCULE LA FORCE FLUIDE NORMALE A L'OBSTACLE
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C        NOCM       MODE                    ROLE
C  ________________ ____ ______________________________________________
C                         VARIABLES DU SYSTEME DYNAMIQUE MODAL
C  ________________ ____ ______________________________________________
C    I              <--   INDICE DU NOEUD DE CHOC
C    DNORM          <--   DISTANCE NORMALE
C    MASGEN         <-->  MATRICE DE MASSE GENERALISEE
C    NBMODE         <--   NOMBRE DE MODES DANS LA BASE
C    PHICAR         <--   PRODUIT PHI T * PHI
C    FEXGEN         <-->  FORCES GENERALISEES
C    ACCGEN          -->  ACCELERATIONS GENERALISEES
C    PULS2           -->  PULSATIONS PROPRES GENERALISEES
C    COEFA           -->  COEFFICIENT DE MASSE AJOUTEE
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I, NBMODE, IM
      REAL*8 MASGEN(*),PHICAR(*),FEXGEN(*),ACCGEN(*)
      REAL*8 COEFA, DNORM, PULS2(*),AMOGEN(*),OLDM
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DO 10 IM=1,NBMODE
        OLDM = MASGEN(IM)
        MASGEN(IM) = MASGEN(IM) - PHICAR(IM+(I-1)*NBMODE)*COEFA/DNORM
        FEXGEN(IM) = FEXGEN(IM) - PHICAR(IM+(I-1)*NBMODE)*
     +                             COEFA*ACCGEN(IM)/DNORM
        PULS2(IM) = PULS2(IM)*OLDM/MASGEN(IM)
        AMOGEN(IM) = AMOGEN(IM)*OLDM/MASGEN(IM)
 10   CONTINUE
      END
