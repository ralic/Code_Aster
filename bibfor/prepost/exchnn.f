      SUBROUTINE EXCHNN(DESCN,NUMN,TCMP,NBC,TVALE,TNUEQ,B,VALCMP,TABER)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      INTEGER DESCN(*),TCMP(*),NBC,TABER(*),NUMN,TNUEQ(*)
      REAL*8 TVALE(*),VALCMP(*)
      LOGICAL B
C
C**********************************************************************
C
C     OPERATION REALISEE
C     ------------------
C
C       EXTRACTION DES VALEURS D' UN ENSEMBLE DE COMPOSANTES SUR UN
C       NOEUDS DANS UN CHAMP_NO
C
C     ARGUMENTS EN ENTREE
C     -------------------
C
C       DESCN : PARTIE DU PRNO ASSOCIE AU NOEUD TRAITE
C
C                  (1) --> ADRESSE DANS TVALE DE LA PARTIE
C                          ASSOCIEE AU NOEUD
C
C                  (2) --> NBR DE CMP SUR CE NOEUD
C
C                  (3),(4),.. --> LES ENTIERS CODES
C
C       NUMN  : NUMERO DU NOEUD A TRAITER
C               QUAND LE CHAMP EST A REPRESENTATION NON CONSTANTE
C               CET INFORMATION EST REDONDANTE AVEC DESCN, DANS CE
C               CAS NUMN VAUT ZERO
C
C       TCMP  : TABLE DES NUMERO DE COMPOSANTES MISE EN JEU
C
C       NBC   : NBR DE COMPOSANTES MISES EN JEU
C
C       TVALE : TABLE DES VALEURS DES CMP DE TOUT LE CHAMP_NO
C
C       TNUEQ : TABLE D'INDIRECTION (JACOT) '.NUEQ'
C
C       B     : .TRUE. LE CHAMP EST PROF_CHNO (FALSE SINON).
C
C     ARGUMENTS EN SORTIE
C     -------------------
C
C       VALCMP : TABLE DES VALEURS DES CMP MISE EN JEU SUR LE NOEUD
C
C**********************************************************************
C
      INTEGER IPOSDG
C
      INTEGER ADR,I,POSCMP,NBCN
C
C-----------------------------------------------------------------------
      INTEGER IIAD 
      REAL*8 R8VIDE 
C-----------------------------------------------------------------------
      ADR = DESCN(1)
      NBCN = -DESCN(2)
C
      IF (NUMN.GT.0) THEN
C
          ADR = 1 + NBCN* (NUMN-1)
C
      END IF
C
      DO 10,I = 1,NBC,1
C
          POSCMP = IPOSDG(DESCN(3),TCMP(I))
C
          IF (POSCMP.GT.0) THEN
C
              IF (B) THEN
C
                  IIAD = TNUEQ(ADR+POSCMP-1)
C
              ELSE
C
                  IIAD = ADR + POSCMP - 1
C
              END IF
C
              VALCMP(I) = TVALE(IIAD)
C
              TABER(I) = 1
C
          ELSE
C
              VALCMP(I) = R8VIDE()
C
              TABER(I) = 0
C
          END IF
C
   10 CONTINUE
C
      END
