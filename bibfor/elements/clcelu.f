      SUBROUTINE CLCELU (PIVA, PIVB,
     &  EFFM, EFFN, HT, ENROBG,
     &  SIGACI, SIGBET,
     &  DNSINF, DNSSUP, EPSILB,
     &  IERR)
C______________________________________________________________________
C
C     CC_ELU
C
C      DETERMINATION DES ARMATURES EN FLEXION COMPOSEE, CONDITIONS ELU
C
C      I PIVA        VALEUR DU PIVOT A
C      I PIVB        VALEUR DU PIVOT B = DEFORMATION MAXI. DU BETON
C      I EFFM        MOMENT DE FLEXION
C      I EFFN        EFFORT NORMAL
C      I HT          EPAISSEUR DE LA COQUE
C      I SIGACI      CONTRAINTE ADMISSIBLE DANS L'ACIER
C      I SIGBET      CONTRAINTE ADMISSIBLE DANS LE BETON
C      O DNSINF      DENSITE DE L'ACIER INFERIEUR
C      O DNSSUP      DENSITE DE L'ACIER SUPERIEUR
C      O EPSILB      DEFORMATION DU BETON
C      O IERR        CODE RETOUR (0 = OK)
C
C______________________________________________________________________
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/10/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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


        IMPLICIT NONE

        REAL*8  PIVA
        REAL*8  PIVB
        REAL*8  EFFM
        REAL*8  EFFN
        REAL*8  HT
        REAL*8  ENROBG
        REAL*8  SIGACI
        REAL*8  SIGBET
        REAL*8  DNSINF
        REAL*8  DNSSUP
        REAL*8  EPSILB
        INTEGER IERR

C       EPAISSEUR UTILE DEPUIS L'ACIER JUSQU'A LA FIBRE SUP COMPRIMEE
        REAL*8  HS
C       EPAISSEUR DEPUIS L'ACIER JUSQU'AU MILIEU DE LA SECTION
        REAL*8  HU
C       EPAISSEUR RELATIVE DU BETON COMPRIME
        REAL*8  ALPHA
C       MOMENT MESURE A PARTIR DE L'ACIER
        REAL*8  MS
C       MOMENT REDUIT
        REAL*8  MUB

        REAL*8  MOMENT
        REAL*8  TMP

        IERR = 0

        MOMENT = -EFFM

        HS = HT - ENROBG
        HU = 0.5D0 * HT - ENROBG

        DNSINF = 0D0
        DNSSUP = 0D0
        EPSILB = 0D0

        MS = ABS(MOMENT) - EFFN * HU
        IF( MS .LT. 0D0 ) THEN
C         BETON ENTIEREMENT TENDU
          TMP = 0.5D0 * (EFFN + MOMENT / HU)
          DNSINF = TMP
          DNSSUP = EFFN - TMP
        ELSE
C         BETON PARTIELLEMENT COMPRIME
          MUB = MS / (HS * HS * SIGBET)
          IF( MUB .LT. 0.48D0 ) THEN
            ALPHA = 1D0 - SQRT(1D0 - 2D0 * MUB)
            EPSILB = PIVA * ALPHA / (1D0 - ALPHA)
            TMP = MS / (HS * (1D0 - ALPHA / 2D0)) + EFFN
C            PIVOT A (PIVOT C NON TRAITE )
            IF( TMP .GT. 0D0 ) THEN
              IF( EPSILB .LT. PIVB ) THEN
                IF( 0D0 .LT. MOMENT ) DNSINF = TMP
                IF( MOMENT .LT. 0D0 ) DNSSUP = TMP
              ELSE
                IERR = 1010
                GOTO 9999
              END IF
            END IF
          ELSE
            IERR = 1020
            GOTO 9999
          END IF
        END IF
        DNSINF = DNSINF / SIGACI
        DNSSUP = DNSSUP / SIGACI

9999  CONTINUE
      END
