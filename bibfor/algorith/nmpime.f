      SUBROUTINE NMPIME(FAMI,KPG,KSP,IMATE,OPTION,
     &           XLONG0,A,XLONGM,DLONG0,
     &           NCSTPM,CSTPM,
     &           VIM,EFFNOM,
     &           VIP,EFFNOP,KLV,FONO)
C ----------------------------------------------------------------------
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
      IMPLICIT NONE
C-----------------------------------------------------------------------
      INTEGER IRET ,NBT ,NEQ ,NVAR 
      REAL*8 DSDE 
C-----------------------------------------------------------------------
      PARAMETER   (NEQ = 6,NBT = 21,NVAR=8)

      CHARACTER*(*) FAMI,OPTION
      REAL*8        XLONG0,A,XLONGM
      INTEGER       KPG,KSP,NCSTPM,IMATE
      REAL*8        CSTPM(NCSTPM)
      REAL*8        DLONG0
      REAL*8        EFFNOM,VIM(NVAR)
      REAL*8        EFFNOP,VIP(NVAR),FONO(NEQ),KLV(NBT)
C -----------------------------------------------------------------
C
C    TRAITEMENT DE LA RELATION DE COMPORTEMENT -ELASTOPLASTICITE-
C    ECROUISSAGE NON LINEAIRE - MODELE DE PINTO MENEGOTTO
C    POUR UN ELEMENT BARRE DE TYPE MECA_ BARRE
C
C -----------------------------------------------------------------
C IN  : E      : MODULE D'YOUNG
C       XLONG0 : LONGUEUR DE L'ELEMENT DE BARRE AU REPOS
C       A      : SECTION DE LA BARRE
C       NCSTPM : NOMBRE DE CONSTANTES DE MATERIAU
C       CSTPM  : CONSTANTES DE MATERIAU :
C           E      : MODULE D'YOUNG
C           SY     : LIMITE ELASTIQUE
C           EPSU   : DEFORMATION ULTIME
C           SU     : CONTRAINTE ULTIME
C           EPSH   : DEFORMATION A LA FIN DU PALIER PLASTIQUE PARFAIT
C           R0     : COEFFICIENT EXPERIMENTAL
C           B      : COEFFICIENT
C           A1     : COEFFICIENT EXPERIMENTAL
C           A2     : COEFFICIENT EXPERIMENTAL
C           ELAN   : RAPPORT LONGUEUR/DIAMETRE DE LA BARRE
C           A6     : COEFFICIENT EXPERIMENTAL FLAMMBAGE
C           C      : COEFFICIENT EXPERIMENTAL FLAMMBAGE
C           COA    : COEFFICIENT EXPERIMENTAL FLAMMBAGE
C       DLONG0 : INCREMENT D'ALLONGEMENT DE L'ELEMENT
C       XLONGM : LONGUEUR DE L'ELEMENT AU TEMPS MOINS
C       EFFNOM : EFFORT NORMAL PRECEDENT
C       OPTION : OPTION DEMANDEE (R_M_T,FULL OU RAPH_MECA)
C
C OUT : EFFNOP   : CONTRAINTE A L'INSTANT ACTUEL
C       VIP    : VARIABLE INTERNE A L'INSTANT ACTUEL
C       FONO   : FORCES NODALES COURANTES
C       KLV    : MATRICE TANGENTE
C
C----------VARIABLES LOCALES
C
      REAL*8      SIGM
      REAL*8      EPSM
      REAL*8      EPSP
      REAL*8      SIGP,XRIG
      REAL*8      DEPS,EPSTHE

C
C----------INITIALISATIONS
C
      CALL R8INIR (NBT,0.D0,KLV,1)
      CALL R8INIR (NEQ,0.D0,FONO,1)
C
C----------RECUPERATION DES CARACTERISTIQUES
C
      CALL VERIFT(FAMI,KPG,KSP,'T',IMATE,'ELAS',1,EPSTHE,IRET)

      EPSM = (XLONGM-XLONG0)/XLONG0
      EPSP = (XLONGM+DLONG0-XLONG0)/XLONG0
      DEPS = EPSP - EPSM - EPSTHE
      SIGM = EFFNOM/A
C
      CALL NM1DPM(FAMI,KPG,KSP,IMATE,OPTION,NVAR,
     >           NCSTPM,CSTPM,
     >           SIGM,VIM,
     >           DEPS,
     >           VIP,SIGP,DSDE)
C
      EFFNOP=SIGP*A
C
C --- CALCUL DES FORCES NODALES
C
      FONO(1) = -EFFNOP
      FONO(4) =  EFFNOP
C
C ----------------------------------------------------------------
C
C
C
C --- CALCUL DU COEFFICIENT NON NUL DE LA MATRICE TANGENTE
C
      IF ( OPTION(1:10) .EQ. 'RIGI_MECA_'.OR.
     &     OPTION(1:9)  .EQ. 'FULL_MECA'         ) THEN
C
             XRIG= DSDE*A/XLONG0
             KLV(1)  =  XRIG
             KLV(7)  = -XRIG
             KLV(10)  = XRIG
      ENDIF
C
      END
