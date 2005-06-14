        SUBROUTINE LCVERR ( DY, DDY, NR, TYP, ERR )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 06/08/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C       ----------------------------------------------------------------
C       MODULE DE CALCUL DE L'ERREUR DE CONVERGENCE
C       IN  DY     :    VECTEUR SOLUTION
C           DDY    :    VECTEUR CORRECTION SUR LA SOLUTION
C           NR     :    DIMENSION DE DY DDY
C           TYP    :    TYPE D'ERREUR A CALCULER
C                               0 = IDDYI/DYII     < EPS (POUR TOUT I)
C                               1 = IIDDYII/IIDYII < EPS
C                               2 = IIDDYI/DYIII   < EPS
C       OUT ERR    :    VECTEUR ERREUR
C       ----------------------------------------------------------------
        REAL*8          ZERO,R8PREM
        PARAMETER       ( ZERO = 0.D0   )
        INTEGER         N , ND , NR,  TYP
        REAL*8          DY(*)  , DDY(*)
        REAL*8          ERR(*) , E(50)
C       ----------------------------------------------------------------
        COMMON /TDIM/   N , ND
C       ----------------------------------------------------------------
C
C       ERREUR(I) =  !DDYI/DYI! < EPS
C
        IF     ( TYP .EQ. 0 ) THEN
            ERR(1)=0.D0
            DO 1 I = 1,NR
C                IF(DY(I).EQ.ZERO) THEN
                IF(DY(I).LT.R8PREM()) THEN
                   ERR(I)    = ABS(DDY(I))
                ELSE
                   ERR(I)    = ABS(DDY(I) / DY(I))
                ENDIF
                ERR(1)=MAX(ERR(1),ERR(I))
 1          CONTINUE
C
C       ERREUR = !!DDY!!/!!DY!! < EPS
C
        ELSEIF ( TYP .EQ. 1 ) THEN
        CALL LCNRVN ( NR , DDY , E(1) )
        CALL LCNRVN ( NR , DY  , E(2) )
            IF(E(2).EQ.ZERO) THEN
            ERR(1)   = E(1)
            ELSE
            ERR(1)   = E(1) / E(2)
            ENDIF
C
C       ERREUR = !!DDYI/DYI!! < EPS
C
        ELSEIF ( TYP .EQ. 2 ) THEN
            DO 2 I = 1,NR
                IF(DY(I).EQ.ZERO) THEN
                E(I) =  DDY(I)
                ELSE
                E(I) =  DDY(I) / DY(I)
                ENDIF
 2          CONTINUE
        CALL LCNRVN ( NR , E , ERR(1) )
        ENDIF
C
        END
