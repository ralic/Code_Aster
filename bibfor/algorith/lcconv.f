        SUBROUTINE LCCONV ( LOI,  YD,DY, DDY,  NR, ITMAX, TOLER, ITER,
     &          INTG, NMAT, MATER, R, RINI,EPSTR,TYPESS, ESSAI,
     &          ICOMP, IRTETI)
        IMPLICIT   NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/05/2011   AUTEUR PROIX J-M.PROIX 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C       ----------------------------------------------------------------
C       ROUTINE D AIGUILLAGE
C       ----------------------------------------------------------------
C       CONTROLE DE LA CONVERGENCE DE LA METHODE DE NEWTON (LCPLNL):
C
C                       - CONTROLE DU NOMBRE D ITERATIONS
C                       - CONTROLE DE LA PRECISION DE CONVERGENCE
C                       - CONTROLE DE LA VALIDITE SOLUTION A CONVERGENCE
C                       - CONTROLE DES RE-INTEGRATIONS EVENTUELLES
C                       - CONTROLE DU REDECOUPAGE DU PAS DE TEMPS
C
C       ----------------------------------------------------------------
C       IN  LOI    :  MODELE DE COMPORTEMENT
C           TYPESS :  TYPE DE SOLUTION D ESSAI POUR DY(DEPEND DU MODELE)
C                      > VOIR XXXCVG ET XXXINI
C           ESSAI  :  VALEUR SOLUTION D ESSAI
C           ITMAX  :  NB MAXI D ITERATIONS LOCALES
C           TOLER  :  TOLERANCE A CONVERGENCE
C           ITER   :  NUMERO ITERATION COURANTE
C           INTG   :  NUMERO INTEGRATION COURANTE
C           NR     :  DIMENSION DY DDY
C           DY     :  VECTEUR SOLUTION = ( DSIG DVIN (DEPS3) )
C           DDY    :  VECTEUR CORRECTION SUR LA SOLUTION
C           ICOMP  :  COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
C       OUT IRTETI = 0:  CONVERGENCE
C           IRTETI = 1:  ITERATION SUIVANTE
C           IRTETI = 2:  RE-INTEGRATION
C           IRTETI = 3:  REDECOUPAGE DU PAS DE TEMPS
C       ----------------------------------------------------------------
        INTEGER         TYPESS, ITMAX,  ITER,   INTG, NR,  ICOMP
        INTEGER         IRTET, IRTETI, NMAT
        REAL*8          TOLER,ESSAI, DDY(*), DY(*),R(*),RINI(*),YD(*)
        REAL*8          MATER(NMAT,2), EPSTR(6)
        CHARACTER*16    LOI
C       ----------------------------------------------------------------
C
         IRTETI = 0

      IF ( LOI(1:9) .EQ. 'VISCOCHAB' ) THEN
         CALL CVMCVG (       DY,     DDY,   NR, ITMAX,  TOLER, ITER,
     &                 INTG, TYPESS, ESSAI, ICOMP, IRTET)
         IF ( IRTET.GT.0 ) GOTO (1,2,3), IRTET
C
      ELSEIF ( LOI(1:8) .EQ. 'MONOCRIS' ) THEN
         CALL LCMMCV ( YD, DY, DDY, NR, ITMAX,  TOLER, ITER,
     &          R,RINI,EPSTR,IRTET)
         IF ( IRTET.GT.0 ) GOTO (1,2,3,4), IRTET
      ELSEIF ( LOI(1:7) .EQ. 'IRRAD3M' ) THEN
         CALL IRRCVG ( DY, DDY, NR, NMAT, MATER, ITMAX, TOLER, ITER,
     &                 R, RINI, IRTET)
         IF ( IRTET.GT.0 ) GOTO (1,2,3,4), IRTET
      ENDIF
C CONVERGENCE, TOUT VA BIEN
      IRTETI = 0
      GOTO 9999
C
 1    CONTINUE
C      =1 ITERATIONS SUPPLEMENTAIRE (ITER<ITMAX)
      IRTETI = 1
      GOTO 9999
C
 2    CONTINUE
      IRTETI = 2
      GOTO 9999
C
 3    CONTINUE
C       =3 ITMAX ATTEINT : redecoupage local si demande.
      IRTETI = 3
      GOTO 9999
 4    CONTINUE
C       =4 ITMAX ATTEINT : redecoupage du pas de temps global
      IRTETI = 4
      GOTO 9999
 9999 CONTINUE
      END
