      SUBROUTINE TENSK1(ICABL,NBNO,S,ALPHA,F0,DELTA,EA,FRCO,FRLI,SA,F)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/05/98   AUTEUR H1BAXBG M.LAINET 
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
C-----------------------------------------------------------------------
C  DESCRIPTION : CALCUL DE LA TENSION LE LONG D'UN CABLE EN PRENANT EN
C  -----------   COMPTE LES PERTES PAR FROTTEMENT ET LES PERTES PAR
C                RECUL DE L'ANCRAGE
C                CAS D'UN SEUL ANCRAGE ACTIF
C                APPELANT : TENSCA
C
C  IN     : ICABL  : INTEGER , SCALAIRE
C                    NUMERO DU CABLE
C  IN     : NBNO   : INTEGER , SCALAIRE
C                    NOMBRE DE NOEUDS DU CABLE
C  IN     : S      : REAL*8 , VECTEUR DE DIMENSION NBNO
C                    CONTIENT LES VALEURS DE L'ABSCISSE CURVILIGNE
C                    LE LONG DU CABLE
C  IN     : ALPHA  : REAL*8 , VECTEUR DE DIMENSION NBNO
C                    CONTIENT LES VALEURS DE LA DEVIATION ANGULAIRE
C                    CUMULEE LE LONG DU CABLE
C  IN     : F0     : REAL*8 , SCALAIRE
C                    VALEUR DE LA TENSION APPLIQUEE A L'ANCRAGE ACTIF
C                    DU CABLE
C  IN     : DELTA  : REAL*8 , SCALAIRE
C                    VALEUR DU RECUL DE L'ANCRAGE
C  IN     : EA     : REAL*8 , SCALAIRE
C                    VALEUR DU MODULE D'YOUNG DE L'ACIER
C  IN     : FRCO   : REAL*8 , SCALAIRE
C                    VALEUR DU COEFFICIENT DE FROTTEMENT EN COURBE
C                    (CONTACT ENTRE LE CABLE ACIER ET LE MASSIF BETON)
C  IN     : FRLI   : REAL*8 , SCALAIRE
C                    VALEUR DU COEFFICIENT DE FROTTEMENT EN LIGNE
C                    (CONTACT ENTRE LE CABLE ACIER ET LE MASSIF BETON)
C  IN     : SA     : REAL*8 , SCALAIRE
C                    VALEUR DE L'AIRE DE LA SECTION DROITE DU CABLE
C  OUT    : F      : REAL*8 , VECTEUR DE DIMENSION NBNO
C                    CONTIENT LES VALEURS DE LA TENSION LE LONG DU CABLE
C                    APRES PRISE EN COMPTE DES PERTES PAR FROTTEMENT ET
C                    DES PERTES PAR RECUL DE L'ANCRAGE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C
C ARGUMENTS
C ---------
      INTEGER       ICABL, NBNO
      REAL*8        S(*), ALPHA(*), F0, DELTA, EA, FRCO, FRLI, SA, F(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER       INO
      REAL*8        D
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   PRISE EN COMPTE DES PERTES DE TENSION PAR FROTTEMENT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      DO 10 INO = 1, NBNO
         F(INO) = F0 * DBLE ( EXP (-FRCO*ALPHA(INO)-FRLI*S(INO)) )
  10  CONTINUE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   PRISE EN COMPTE DES PERTES DE TENSION PAR RECUL DE L'ANCRAGE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      CALL ANCRCA(ICABL,NBNO,S,ALPHA,F0,DELTA,EA,FRCO,FRLI,SA,D,F)
C
C --- FIN DE TENSK1.
      END
