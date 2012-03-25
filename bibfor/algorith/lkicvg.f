        SUBROUTINE LKICVG(NR,ITMAX,TOLER,ITER,R,IRTET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 
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
C     ------------------------------------------------------------------
C       CONTROLE DE LA CONVERGENCE DU NEWTON LOCAL DE LETK
C                     - CONTROLE DU NOMBRE D ITERATIONS
C                     - CONTROLE DE LA PRECISION DE CONVERGENCEC
C     ------------------------------------------------------------------
C       IN  ITMAX  :  NB MAXI D ITERATIONS LOCALES
C           TOLER  :  TOLERANCE A CONVERGENCE
C           ITER   :  NUMERO ITERATION COURANTE
C           NR     :  DIMENSION R
C           R      :  RESIDU DU SYSTEME NL A L'ITERATION COURANTE
C
C       OUT IRET = 0:  CONVERGENCE
C           IRET = 1:  ITERATION SUIVANTE
C           IRET = 2:  RE-INTEGRATION
C           IRET = 3:  REDECOUPAGE DU PAS DE TEMPS
C     ------------------------------------------------------------------
        IMPLICIT NONE
C     ------------------------------------------------------------------
        COMMON /TDIM/   NDT ,NDI
C     ------------------------------------------------------------------
        INTEGER         NR,ITMAX,ITER,IRTET,NDT,NDI
        REAL*8          TOLER,R(NR)
C
        INTEGER         I
        REAL*8          ER,ZERO
        PARAMETER       (ZERO  =  0.D0 )
C     ------------------------------------------------------------------
C === ==================================================================
C --- CALCUL DE LA NORME DE RINI ET DE R(Y)
C === ==================================================================
        ER    = ZERO
        DO 10 I=1,NR
          ER    = ER + R(I)*R(I)
  10    CONTINUE
        ER    = SQRT(ER)

C === =================================================================
C --- TEST DE CONVERGENCE PAR RAPPORT A TOLER
C === =================================================================
      IF ( ER .LT. TOLER ) THEN
         IRTET = 0
         GOTO 9999
      ENDIF

C === ==================================================================
C --- SI NON CONVERGENCE: TEST DU N°ITERATION
C === ==================================================================
      IF(ITER.LT.ITMAX)THEN
        IRTET = 1
      ELSE
        IRTET = 3
      ENDIF

 9999 CONTINUE

      END
