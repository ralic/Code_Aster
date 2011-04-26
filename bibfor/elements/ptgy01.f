      SUBROUTINE PTGY01(SK,NL,XNU,RHO,A,XL,XIY,XIZ,XJX,ALFINV,
     &                   EY,EZ,IST)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C ======================================================================
      IMPLICIT NONE
      REAL*8 SK(*)
      REAL*8 XNU,RHO,A,XL,XIY,XIZ,XJX,EY,EZ
      INTEGER NL,IST
C    -------------------------------------------------------------------
C    * CE SOUS PROGRAMME CALCULE LA MATRICE D'AMORITSSEMENT GYROSCOPIQUE
C      DE L'ELEMENT DE POUTRE DROITE A SECTION CONSTANTE.
C
C    * DESCRIPTION DE L'ELEMENT:
C      C'EST UN ELEMENT A DEUX NOEUDS ET A SIX DEGRES DE LIBERTES PAR
C      NOEUDS (3 DEPLACEMENTS ET 3 ROTATIONS).
C
C    * REMARQUE :
C      LA MATRICE EST STOCKEE PLEINE (ANTISYMETRIQUE)
C      UNICOLONNE
C    -------------------------------------------------------------------
C
C IN TYPE ! NOM    ! TABLEAU !             SIGNIFICATION
C IN -------------------------------------------------------------------
C IN R*8  ! E      !     -   ! MODULE D'ELASTICITE DU MATERIAU
C IN  I   ! NL     !     -   ! TAILLE MATRICE DECLAREE DANS te0006.f
C IN R*8  ! RHO    !     -   ! MASSE VOLUMIQUE DU MATERIAU
C IN R*8  ! A      !     -   ! AIRE DE LA SECTION DROITE DE L'ELEMENT
C IN R*8  ! XL     !     -   ! LONGUEUR DE L ELEMENT
C IN R*8  ! XIY    !     -   ! MOMENT D INERTIE / Y PRINCIPAL
C IN R*8  ! XIZ    !     -   ! MOMENT D INERTIE / Z PRINCIPAL
C IN R*8  ! XJX    !     -   ! CONSTANTE DE TORSION
C IN R*8  ! G      !     -   ! MODULE DE CISAILLEMENT DU MATERIAU
C IN R*8  ! ALFINV  !     ! INVERSE DU COEFFICIENT DE CISAILLEMENT
C IN R*8  ! EY     !     -   ! COMPOSANTE GT SUR Y PRINCIPAL
C IN R*8  ! EZ     !     -   ! COMPOSANTE GT SUR Z PRINCIPAL
C IN  I   ! IST    !    -    ! TYPE DE STRUCTURE DE LA POUTRE
C IN
C IN (+) REMARQUES :
C
C OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
C OUT ------------------------------------------------------------------
C OUT R*8 !   SK   ! (78)    ! MATRICE ELEMENTAIRE UNICOLONNE
C

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER IADZI,IAZK24
      CHARACTER*8 NOMAIL
      REAL*8 ZERO, R8PREM
      REAL*8 PHI, COM
      REAL*8 IP, ALFINV
      INTEGER I, J, IPOINT
C
      PARAMETER (ZERO=0.D0)

C ---------------------------------------------------------------------
      DO 1,I = 1,NL
          SK(I) = ZERO
    1 CONTINUE
C
      CALL ASSERT(NL.EQ.78)


      IF (ABS(XL).LT.R8PREM()) THEN
        CALL TECAEL(IADZI,IAZK24)
        NOMAIL = ZK24(IAZK24-1+3)(1:8)
        CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
      END IF
      IP = (XIY+XIZ)
      PHI = 12.D0*IP*ALFINV*(1.D0+XNU)/(A*XL*XL)
      COM =  RHO * IP / (30.D0 * XL*(1.D0+PHI)*(1.D0+PHI))

C
C     I : LIGNE ; J : COLONNE
      I = 2
      J = 3
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = 36.D0 * COM
      I = 2
      J = 5
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = -(3.D0 - 15.D0 * PHI) * COM *XL
      I = 3
      J = 6
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = -(3.D0  - 15.D0 * PHI) * COM *XL
      I = 5
      J = 6
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = (4.D0+5.D0*PHI+10.D0*PHI*PHI)*COM*XL*XL
      I = 3
      J = 8
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) =  36.D0 * COM
      I = 5
      J = 8
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = -(3.D0 - 15.D0 * PHI) * COM * XL
      I = 2
      J = 9
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = -36.D0 * COM
      I = 6
      J = 9
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = -(3.D0 - 15.D0 * PHI) * COM * XL
      I = 8
      J = 9
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = 36.D0 * COM
      I = 2
      J = 11
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = -(3.D0 - 15.D0 * PHI) * COM * XL
      I = 6
      J = 11
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = (1.D0+5.D0*PHI-5.D0*PHI*PHI)*COM*XL*XL
      I = 8
      J = 11
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = (3.D0 - 15.D0 * PHI) * COM * XL
      I = 3
      J = 12
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = -(3.D0 - 15.D0 * PHI) * COM * XL
      I = 5
      J = 12
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = -(1.D0+5.D0*PHI-5.D0*PHI*PHI)*COM*XL*XL
      I = 9
      J = 12
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = (3.D0 - 15.D0 * PHI) * COM * XL
      I = 11
      J = 12
      IPOINT = INT(J*(J-1)/2)+I
      SK(IPOINT) = (4.D0+5.D0*PHI+10.D0*PHI*PHI)*COM*XL*XL
C

      END
