        SUBROUTINE BURCVG( NR, ITMAX, TOLER, ITER, DY,
     &                     R, RINI, IRTET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2012   AUTEUR GENIAUT S.GENIAUT 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE FOUCAULT A.FOUCAULT      
C       ----------------------------------------------------------------
C       CONTROLE DE LA CONVERGENCE DE LA METHODE DE NEWTON :
C
C                       - CONTROLE DU NOMBRE D ITERATIONS
C                       - CONTROLE DE LA PRECISION DE CONVERGENCE
C                       - CONTROLE DE LA VALIDITE SOLUTION A CONVERGENCE
C                       - CONTROLE DES RE-INTEGRATIONS EVENTUELLES
C                       - CONTROLE DU REDECOUPAGE DU PAS DE TEMPS
C
C       ----------------------------------------------------------------
C       IN  ITMAX  :  NB MAXI D ITERATIONS LOCALES
C           TOLER  :  TOLERANCE A CONVERGENCE
C           ITER   :  NUMERO ITERATION COURANTE
C           INTG   :  NUMERO INTEGRATION COURANTE
C           NR     :  DIMENSION DY DDY
C           DY     :  INCREMENT DU VECTEUR SOLUTION (YF-YD)
C           R      :  RESIDU DU SYSTEME NL A L'ITERATION COURANTE
C           RINI   :  RESIDU DU SYSTEME NL APRES TIR D'EULER
C       OUT IRTET = 0:  CONVERGENCE
C           IRTET = 1:  ITERATION SUIVANTE
C           IRTET = 2:  RE-INTEGRATION
C           IRTET = 3:  REDECOUPAGE DU PAS DE TEMPS
C       ----------------------------------------------------------------
      IMPLICIT NONE
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT ,NDI
C     ----------------------------------------------------------------
      INTEGER         I,NR,ITMAX,ITER,IRTET,IMAX,NDT,NDI
      REAL*8          TOLER,R(NR),RINI(NR),DY(NR)
      REAL*8          ER,ERINI,YOUNG,NDY,MAXI 
C       ----------------------------------------------------------------
C === =================================================================
C --- CALCUL DE LA NORME DE RINI ET DE R(Y)
C === =================================================================
      ER    = 0.0D0
      ERINI = 0.0D0
      DO 1 I=1,NR
         ER    = ER + R(I)*R(I)
         ERINI = ERINI + RINI(I)*RINI(I)
1     CONTINUE
      ER    = SQRT(ER)
      ERINI = SQRT(ERINI) 
C      IF(ERINI.GT.TOLER)ER=ER/ERINI
C === =================================================================
C --- TEST DE CONVERGENCE PAR RAPPORT A TOLER
C === =================================================================
      IF ( ER .LT. TOLER ) THEN
         IRTET = 0
         GOTO 9999
      ENDIF
C === =================================================================
C --- TEST DE CONVERGENCE PAR RAPPORT A DY
C === =================================================================
      IF ( ER .LT. ERINI ) THEN
        NDY = 0.D0
        DO 2 I=NDT+1,2*NDT
           NDY = NDY + DY(I)*DY(I)
 2      CONTINUE
        NDY = SQRT(NDY)
        IF(NDY.LT.TOLER)THEN
          MAXI = 0.D0
          IMAX = 0
          DO 3 I = 1, NR
            IF(ABS(R(I)).GT.MAXI)THEN
              MAXI = ABS(R(I))
              IMAX = I
            ENDIF
 3        CONTINUE
          IF(IMAX.GT.NDT)THEN
            IRTET = 0
            GOTO 9999
          ENDIF
        ENDIF
      ENDIF
C === =================================================================
C --- SI NON CONVERGENCE: TEST DU N°ITERATION
C === =================================================================
      IF(ITER.LT.ITMAX)THEN
        IRTET = 1
      ELSE
        IRTET = 3
      ENDIF

 9999 CONTINUE

      END
