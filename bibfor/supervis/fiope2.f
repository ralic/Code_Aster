      SUBROUTINE FIOPE2(NCODOP,LPILE,PILE,ICLASS,MI,MR,ML,MC,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NCODOP,LPILE,PILE(*),ICLASS(*),MI(*),IER
      REAL*8                                        MR(*)
      LOGICAL                                          ML(*)
      COMPLEX*16                                          MC(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 26/03/97   AUTEUR G8BHHXD X.DESROCHES 
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
C     EFFECTUER UNE OPERATION BINAIRE       (FONCTIONS INTERPRETEES)
C     ------------------------------------------------------------------
C PARAMETRES D ENTREE :
C     NCODOP  : CODE OPERATION
C     PILE    : PILE DES OPERANDES
C     MI,MR,ML,MC: TABLE DES CONSTANTES ENTIERES, REELLES, ...
C OUT IER     : CODE DE RETOUR
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
      COMMON /FICL01/ ICSTE , IOPE1, IOPE2, LOPE1, LOPE2
C     ------------------------------------------------------------------
      INTEGER RESTYP(6,6),TYPE1,TYPE2,TYPRES
      DATA    RESTYP/   1,   2,   0,  0,  5,  0,
     +                  2,   2,   0,  0,  5,  0,
     +                  0,   0,   0,  0,  0,  0,
     +                  0,   0,   0,  0,  0,  0,
     +                  5,   5,   0,  0,  5,  0,
     +                  0,   0,   0,  0,  0,  6/
C     ------------------------------------------------------------------
C
      IER = 0
C
C     --- REPERAGE DES OPERANDES ---
      IF (LPILE.LE.0) GOTO 900
      IARG2 = LPILE
      TYPE2 = ICLASS(LPILE)
C
      LPILE = LPILE - 1
      IF (LPILE.LE.0) GOTO 900
      IARG1 = LPILE
      TYPE1 = ICLASS(LPILE)
C
      TYPRES = RESTYP(TYPE1,TYPE2)
      IRES   = LPILE
C
C     BRANCHEMENT SELON LE CODE D'OPERATION (CF FIINIT)
C
C     ---------------------------------------------------------------
C     CODE : NOM    ! CODE : NOM    ! CODE : NOM    ! CODE : NOM    !
C     ---------------------------------------------------------------
C       01 : (      !   02 : +      !   03 : -      !   04 : *      !
C       05 : /      !   06 : **     !   07 : =      !   08 : MOD
C       09 : MIN    !   10 : MAX    !
C       11 : .EQ.   !   12 : .NE.   !   13 : .GT.   !   14 : .LT.   !
C       15 : .GE.   !   16 : .LE.   !   17 : ATAN2  !   18 :        !
C       19 :        !   20 :        !   21 :        !   22 :        !
C     ------------------------------------------------------------------
      NCOD = NCODOP - LOPE2 + 1
      GOTO( 900,  20,  30,  40,  50,  60, 900,  80,  90, 100,
     +      110, 120, 130, 140, 150, 160, 170                ),NCOD
C
      IER = IER + 1
      CALL UTMESS('F','SUPERVISEUR.FONCTIONS.INTERPRETEES.(ERREUR.00)',
     +                'CODE D''OPERATION BINAIRE INCONNU')
      GOTO 999
C
C  2-: +
   20 CONTINUE
      GOTO (21,22,900,900,25,26),TYPRES
   21 CONTINUE
      MI(IRES) = MI(IARG1) + MI(IARG2)
      GOTO 999
   22 CONTINUE
      MR(IRES) = MR(IARG1) + MR(IARG2)
      GOTO 999
   25 CONTINUE
      MC(IRES) = MC(IARG1) + MC(IARG2)
      GOTO 999
   26 CONTINUE
      ML(IRES) = ML(IARG1) .OR. ML(IARG2)
      GOTO 999
C
C  -3: -
   30 CONTINUE
      GOTO (31,32,900,900,35,900),TYPRES
   31 CONTINUE
      MI(IRES) = MI(IARG1) - MI(IARG2)
      GOTO 999
   32 CONTINUE
      MR(IRES) = MR(IARG1) - MR(IARG2)
      GOTO 999
   35 CONTINUE
      MC(IRES) = MC(IARG1) - MC(IARG2)
      GOTO 999
C
C  4-:*
   40 CONTINUE
      GOTO (41,42,900,900,45,46),TYPRES
   41 CONTINUE
      MI(IRES) = MI(IARG1) * MI(IARG2)
      GOTO 999
   42 CONTINUE
      MR(IRES) = MR(IARG1) * MR(IARG2)
      GOTO 999
   45 CONTINUE
      MC(IRES) = MC(IARG1) * MC(IARG2)
      GOTO 999
   46 CONTINUE
      ML(IRES) = ML(IARG1) .AND. ML(IARG2)
      GOTO 999
C
C  5_: /
   50 CONTINUE
      GOTO (51,52,900,900,55,900),TYPRES
   51 CONTINUE
      MI(IRES) = MI(IARG1) / MI(IARG2)
      GOTO 999
   52 CONTINUE
      MR(IRES) = MR(IARG1) / MR(IARG2)
      GOTO 999
   55 CONTINUE
      MC(IRES) = MC(IARG1) / MC(IARG2)
      GOTO 999
C
C  6-: **
   60 CONTINUE
      GOTO (61,62,900,900,65,900),TYPRES
   61 CONTINUE
      MI(IRES) = MI(IARG1) ** MI(IARG2)
      GOTO 999
   62 CONTINUE
      IF ( TYPE2 .EQ. 1 ) THEN
         MR(IRES) = MR(IARG1) ** MI(IARG2)
      ELSE
         MR(IRES) = MR(IARG1) ** MR(IARG2)
      ENDIF
      GOTO 999
   65 CONTINUE
      IF ( TYPE2 .EQ. 1 ) THEN
         MC(IRES) = MC(IARG1) ** MI(IARG2)
      ELSEIF ( TYPE2 .EQ. 2 ) THEN
         MC(IRES) = MC(IARG1) ** MR(IARG2)
      ELSE
         MC(IRES) = MC(IARG1) ** MC(IARG2)
      ENDIF
      GOTO 999
C
C  8-: MOD
   80 CONTINUE
      GOTO ( 81, 82,900,900,900,900),TYPRES
   81 CONTINUE
      MI(IRES) = MOD(MI(IARG1), MI(IARG2))
      GOTO 999
   82 CONTINUE
      MR(IRES) = MOD(MR(IARG1), MR(IARG2))
      GOTO 999
C
C  9-: MIN
   90 CONTINUE
      GOTO ( 91, 92,900,900,900,900),TYPRES
   91 CONTINUE
      MI(IRES) = MIN(MI(IARG1), MI(IARG2))
      GOTO 999
   92 CONTINUE
      MR(IRES) = MIN(MR(IARG1), MR(IARG2))
      GOTO 999
C
C 10: MAX
  100 CONTINUE
      GOTO (101,102,900,900,900,900),TYPRES
  101 CONTINUE
      MI(IRES) = MAX(MI(IARG1), MI(IARG2))
      GOTO 999
  102 CONTINUE
      MR(IRES) = MAX(MR(IARG1), MR(IARG2))
      GOTO 999
C
C 11_:.EQ.
  110 CONTINUE
      TYPRES = 6
      GOTO (111,112,900,900,115,116),MAX(TYPE1,TYPE2)
  111 CONTINUE
      ML(IRES) = MI(IARG1) .EQ. MI(IARG2)
      GOTO 999
  112 CONTINUE
      ML(IRES) = MR(IARG1) .EQ. MR(IARG2)
      GOTO 999
  115 CONTINUE
      ML(IRES) = ML(IARG1) .EQV. ML(IARG2)
      GOTO 999
  116 CONTINUE
      ML(IRES) = MC(IARG1) .EQ. MC(IARG2)
      GOTO 999
C
C 12: .NE.
  120 CONTINUE
      TYPRES = 6
      GOTO (121,122,900,900,125,126),MAX(TYPE1,TYPE2)
  121 CONTINUE
      ML(IRES) = MI(IARG1) .NE. MI(IARG2)
      GOTO 999
  122 CONTINUE
      ML(IRES) = MR(IARG1) .NE. MR(IARG2)
      GOTO 999
  125 CONTINUE
      ML(IRES) = MC(IARG1) .NE. MC(IARG2)
      GOTO 999
  126 CONTINUE
      ML(IRES) = ML(IARG1) .NEQV. ML(IARG2)
      GOTO 999
C
C 13: .GT.
  130 CONTINUE
      TYPRES = 6
      GOTO (131,132,900,900,900,900) MAX(TYPE1,TYPE2)
  131 CONTINUE
      ML(IRES) = MI(IARG1) .GT. MI(IARG2)
      GOTO 999
  132 CONTINUE
      ML(IRES) = MR(IARG1) .GT. MR(IARG2)
      GOTO 999
C
C 14: .LT.
  140 CONTINUE
      TYPRES = 6
      GOTO (141,142,900,900,900,900),MAX(TYPE1,TYPE2)
  141 CONTINUE
      ML(IRES) = MI(IARG1) .LT. MI(IARG2)
      GOTO 999
  142 CONTINUE
      ML(IRES) = MR(IARG1) .LT. MR(IARG2)
      GOTO 999
C
C 15: .GE.
  150 CONTINUE
      TYPRES = 6
      GOTO (151,152,900,900,900,900),MAX(TYPE1,TYPE2)
  151 CONTINUE
      ML(IRES) = MI(IARG1) .GE. MI(IARG2)
      GOTO 999
  152 CONTINUE
      ML(IRES) = MR(IARG1) .GE. MR(IARG2)
      GOTO 999
C
C 16: .LE.
  160 CONTINUE
      TYPRES = 6
      GOTO (161,162,900,900,900,900),TYPE1
  161 CONTINUE
      ML(IRES) = MI(IARG1) .LE. MI(IARG2)
      GOTO 999
  162 CONTINUE
      ML(IRES) = MR(IARG1) .LE. MR(IARG2)
      GOTO 999
C
C 17: ATAN2
  170 CONTINUE
      GOTO (171,172,900,900,900,900),TYPE1
  171 CONTINUE
      MR(IRES) = ATAN2( DBLE(MI(IARG1)),DBLE(MI(IARG2)))
      TYPRES = 2
      GOTO 999
  172 CONTINUE
      MR(IRES) = ATAN2( MR(IARG1),MR(IARG2))
      GOTO 999
C
C RETOUR (AVEC OU SANS ERREUR)
  900 CONTINUE
      CALL UTMESS('F','FIOPE2','OPERATION IMPOSSIBLE: ERREUR DE TYPAGE')
      IER = IER + 1
  999 CONTINUE
      ICLASS(IRES) = TYPRES
C
      END
