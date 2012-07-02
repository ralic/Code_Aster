      SUBROUTINE LXLIIS(CHAINE,IVAL,IER)
      IMPLICIT NONE
      CHARACTER*(*)     CHAINE
      INTEGER                  IVAL,IER
C    -------------------------------------------------------------------
C  DOCUMENTATION "LX"
C        ---------------------------------------------------------------
C        ICLASS      CODE DE CE QUE L'ON A TROUVE
C           -- TYPE -----    ---- INFORMATION --------------------------
C        -1 FIN D'ENREGISTREMENT  (RIEN A LIRE)
C         0 ERREUR           CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C         1 ENTIER           IVAL DE TYPE INTEGER
C         2 REEL             RVAL DE TYPE REAL*8
C         3 IDENTIFICATEUR   CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C         4 TEXTE            CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C         5 SEPARATEUR       CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C        ---------------------------------------------------------------
C
C
C                         LISTE DES ROUTINES "LX"
C
C        LXLIIS    DECODAGE D'UN ENTIER ECRIT EN CHAINE DE CARACTERES
C        LXLIRE    LECTURE DE L' ENTITE LEXICALE SUIVANTE
C        LXLIGN    PASSAGE A LA LIGNE PUIS LECTURE D'UNE ENTITE LEXICALE
C        LXUNIT    DECLARATION DES UNITES DE LECTURE ET D'ECRITURE
C        LXINFO    RENVOI LES INFORMATIONS RELATIVES A LA LIGNE COURANTE
C        LXCAPS    PASSAGE D'UN TEXTE DE MINUSCULES EN MAJUSCULES
C        LXCADR    CADRAGE A DROITE D'UN TEXTE
C        LXINIT    INITIALISATIONS DE L'ANALYSEUR LEXICALE
C        LXDELI    DEFINITIONS DES DELIMITEURS
C        LXSCAN    RECHERCHE D'UNE ENTITE LEXICALE
C
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     DECODAGE D'UN ENTIER ECRIT EN CHAINE DE CARACTERES
C     ------------------------------------------------------------------
C IN  CHAINE : CH*(*) : CHAINE DE CARACTERES CONTENANT L'ENTIER
C OUT IVAL   : IS     : ENTIER DECODE
C OUT IER    : IS     : CODE RETOUR
C              = 0  PAS D'ERREUR ON A BIEN LU UN ENTIER (IVAL)
C              = 1  ON A LU AUTRE CHOSE QU'UN ENTIER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         LXSCAN
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN LXLIIS
C     ------------------------------------------------------------------
C
      CHARACTER*80 CVAL
      REAL*8       RVAL
C
C-----------------------------------------------------------------------
      INTEGER ICLASS ,ICOL 
C-----------------------------------------------------------------------
      IER  = 0
      ICOL = 1
      CALL LXSCAN(CHAINE,ICOL,ICLASS,IVAL,RVAL,CVAL)
C     ------------------------------------------------------------------
C                          ICLASS      CODE DE CE QUE L'ON A TROUVE
C           -- TYPE -----    ---- INFORMATION --------------------------
C          -1 FIN DE LIGNE   (RIEN A LIRE)
C           0 ERREUR         CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C           1 ENTIER         IVAL DE TYPE INTEGER
C           2 REEL           RVAL DE TYPE REAL*8
C           3 IDENTIFICATEUR CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C           4 TEXTE          CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C           5 SEPARATEUR     CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C     ------------------------------------------------------------------
      IF (ICLASS.NE.1) IER = 1
      END
