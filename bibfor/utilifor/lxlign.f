      SUBROUTINE LXLIGN(ICLASS,IVAL,RVAL,CVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8                        RVAL
      CHARACTER*(*)                      CVAL
C
C     ------------------------------------------------------------------
C     PROVOQUE UN PASSAGE A LA LIGNE SUIVANTE  PUIS FAIT LA
C     LECTURE D'UNE ENTITE LEXICALE (AVEC GESTION DES SAUTS DE LIGNES)
C     ------------------------------------------------------------------
C OUT ICLASS CLASSE DE L'ITEM TROUVE
C           -- TYPE -----    ---- INFORMATION --------------------------
C      -1   FIN DU FICHIER DE LECTURE
C       0   ERREUR           CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       1   ENTIER           IVAL DE TYPE INTEGER
C       2   REEL             RVAL DE TYPE REAL*8
C       3   IDENTIFICATEUR   CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       4   TEXTE            CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       5   SEPARATEUR       CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         LXLIRE
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN LXLIGN
C     ------------------------------------------------------------------
C
      PARAMETER (MXCOLS=80)
C
C     VARIABLES GLOBALES DE LECTURE
      INTEGER            ILECT, IECR, LRECL, IEOF, ICOL, ILIG
      COMMON /LXCN02/    ILECT, IECR, LRECL, IEOF, ICOL, ILIG
C
      ICOL = MXCOLS+1
      CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
C
      END
