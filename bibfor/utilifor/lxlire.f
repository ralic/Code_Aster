      SUBROUTINE LXLIRE(ICLASS,IVAL,RVAL,CVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           ICLASS,IVAL
      REAL*8                        RVAL
      CHARACTER*(*)                      CVAL
C
C     ------------------------------------------------------------------
C     LECTURE D'UNE ENTITE LEXICALE AVEC GESTION DES FINS DE LIGNES
C     ------------------------------------------------------------------
C OUT ICLASS  CLASSE DE CE QUE L'ON A TROUVE
C     ------------------------------------------------------------------
C     ICLASS      CLASSE DE L'ITEM TROUVE
C           -- TYPE -----    ---- INFORMATION --------------------------
C      -1   EOF FIN DU FICHIER DE LECTURE
C       0   ERREUR           CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       1   ENTIER           IVAL DE TYPE INTEGER
C       2   REEL             RVAL DE TYPE REAL*8
C       3   IDENTIFICATEUR   CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       4   TEXTE            CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       5   SEPARATEUR       CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 24/11/2003   AUTEUR DURAND C.DURAND 
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
C         LXSCAN
C     ROUTINE(S) FORTRAN     :
C         READ   WRITE
C     ------------------------------------------------------------------
C FIN LXLIRE
C     ------------------------------------------------------------------
C     DEFINITION DES PARAMETRES
      PARAMETER (MXCOLS=80)
C
C     VARIABLES GLOBALES DE LECTURE-ECRITURE ---------------------------
      CHARACTER*(MXCOLS) CARTE
      COMMON /LXCC02/    CARTE
      INTEGER            ILECT, IECR, LRECL, IEOF, ICOL, ILIG
      COMMON /LXCN02/    ILECT, IECR, LRECL, IEOF, ICOL, ILIG
      CHARACTER*20       FMTWRT
      COMMON /LXCC03/    FMTWRT
C     ------------------------------------------------------------------
C     ------- COMMUN FORMULE
      INTEGER NOMBRE
      PARAMETER(NOMBRE=50)
      CHARACTER*80 FTEXT(NOMBRE)
      COMMON /CXFO00/LPOS
      COMMON /CXFO01/FTEXT
C     ------------------------------------------------------------------
C
  1   CONTINUE
C
C        --- LECTURE DE LA LIGNE SUIVANTE SI NECESSAIRE ---
         IF ( ICOL.GT.LRECL ) THEN
            IF ( IEOF.EQ.0 ) THEN
               IF(LPOS.LE.0)THEN
                 READ(ILECT,'(A)',END=999,ERR=999) CARTE(1:LRECL)
               ELSE
                 IF(ILIG.GE.NOMBRE)GO TO 999
                 CARTE(1:LRECL)=FTEXT(ILIG+1)(1:LRECL)
                 LPOS=LPOS+80
               ENDIF
               ILIG = ILIG + 1
               IF (IECR.GT.0) WRITE(IECR ,FMTWRT) ILIG, CARTE(1:LRECL)
               ICOL = 1
            ELSE
               GOTO 999
            ENDIF
         ENDIF
C
C        --- LECTURE D'UNE ENTITE LEXICALE ---
         CALL LXSCAN(CARTE(1:LRECL),ICOL,ICLASS,IVAL,RVAL,CVAL)
C        ---------------------------------------------------------------
C                          ICLASS      CODE DE CE QUE L'ON A TROUVE
C           -- TYPE -----    ---- INFORMATION --------------------------
C          -1 FIN DE LIGNE   (RIEN A LIRE)
C           0 ERREUR         CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C           1 ENTIER         IVAL DE TYPE INTEGER
C           2 REEL           RVAL DE TYPE REAL*8
C           3 IDENTIFICATEUR CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C           4 TEXTE          CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C           5 SEPARATEUR     CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C        ---------------------------------------------------------------
      IF (ICLASS.EQ.-1) GOTO 1
      GOTO 9999
C
C     ---- EOF ---------------------------------------------------------
 999  CONTINUE
      IEOF   =  1
      ICLASS = -1
C
 9999 CONTINUE
      END
