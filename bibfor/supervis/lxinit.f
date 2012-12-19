      SUBROUTINE LXINIT()
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_6
C     INITIALISATION DE L'ANALYSEUR LEXICAL
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         (CF ARGUMENT)
C     ROUTINE(S) FORTRAN     :
C         CHAR    ICHAR
C     ------------------------------------------------------------------
C FIN LXINIT
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,MXCHAR ,MXCLAS ,MXCOLS ,MXDELI ,NBDELI
C-----------------------------------------------------------------------
      PARAMETER ( MXCLAS = 10 , MXCHAR = 255 , MXDELI = 15 )
      INTEGER
     +  CLNUM , CLLET , CLSIG , CLPNT , CLEXP , CLQUO , CLBLS , CLBL  ,
     +  CLILL , CLEOR
C
      COMMON /LXCN01/   CLNUM , CLLET , CLSIG , CLPNT , CLEXP , CLQUO ,
     +                  CLBLS , CLBL  , CLILL , CLEOR , NBDELI
C
      CHARACTER*1        CLASS(0:MXCHAR), CLDELI(MXDELI)
      COMMON /LXCC01/    CLASS          , CLDELI
C
C     ------------------------------------------------------------------
      PARAMETER  ( MXCOLS = 80 )
      CHARACTER*(MXCOLS)    CHAINE
      CHARACTER*1  KCLASS
C     ------------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
C                     DEFINITION DES CLASSES SIMPLES
C     CLNUM  =  1 : NUMERIQUES        CLLET  =  2 : LETTRES
C     CLSIG  =  3 : SIGNE + -         CLPNT  =  4 : POINT .
C     CLEXP  =  5 : EXPOSANT E D      CLQUO  =  6 : QUOTE '
C     CLBLS  =  7 : BLANC SOULIGNE _  CLBL   =  8 : BLANC
C     CLILL  =  9 : ILLEGAUX          CLEOR  = 10 : FIN D'ENREGISTREMENT
C     ------------------------------------------------------------------
C
      CLNUM  =  1
      CLLET  =  2
      CLSIG  =  3
      CLPNT  =  4
      CLEXP  =  5
      CLQUO  =  6
      CLBLS  =  7
      CLBL   =  8
      CLILL  =  9
      CLEOR  = 10
C     ------------------------------------------------------------------
C
C
C     INITIALISATION DES CLASSES A ILLEGAL /* OPTION PAR DEFAUT */
      KCLASS = CHAR(CLILL)
      DO 10 I = 0 , MXCHAR
        CLASS(I) = KCLASS
  10  CONTINUE
C
C     INITIALISATION DE LA CLASSE NUMERIQUE
      CHAINE = '0123456789'
      KCLASS = CHAR(CLNUM)
      DO 20 I = 1 , 10
        CLASS(ICHAR(CHAINE(I:I))) = KCLASS
  20  CONTINUE
C
C     INITIALISATION DE LA CLASSE ALPHABETIQUE
      CHAINE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'//
     +         'abcdefghijklmnopqrstuvwxyz'
      KCLASS = CHAR(CLLET)
      DO 30 I = 1 , 52
        CLASS(ICHAR(CHAINE(I:I))) = KCLASS
  30  CONTINUE
C
C     INITIALISATION DE LA CLASSE SIGNE
      CLASS(ICHAR('+')) = CHAR(CLSIG)
      CLASS(ICHAR('-')) = CHAR(CLSIG)
C
C     INITIALISATION DE LA CLASSE EXPOSANT
      CLASS(ICHAR('E')) = CHAR(CLEXP)
      CLASS(ICHAR('e')) = CHAR(CLEXP)
      CLASS(ICHAR('D')) = CHAR(CLEXP)
      CLASS(ICHAR('d')) = CHAR(CLEXP)
C
C     INITIALISATION DE LA CLASSE QUOTE BLANC BLANC_SOULIGNE ET POINT
      CLASS(ICHAR('''')) = CHAR(CLQUO)
      CLASS(ICHAR(' '))  = CHAR(CLBL )
      CLASS(ICHAR('_'))  = CHAR(CLBLS)
      CLASS(ICHAR('.'))  = CHAR(CLPNT)

C     TABULATION
      CLASS(9) = CHAR(CLBL )
C
C     INITIALISATION DE LA CLASSE 'DELIMITEUR'
      NBDELI = MXDELI
      CALL LXDELI( CLDELI , NBDELI )
      KCLASS = CHAR(CLILL)
      DO 40 I = 1 , NBDELI
         IF ( CLASS(ICHAR(CLDELI(I))) .EQ. KCLASS ) THEN
            CLASS(ICHAR(CLDELI(I))) = CHAR(MXCLAS+I)
         ENDIF
  40  CONTINUE
C
C     ------------------------------------------------------------------
C
      END
