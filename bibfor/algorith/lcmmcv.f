        SUBROUTINE LCMMCV ( DY,   DDY,    NR,    ITMAX, TOLER, ITER,
     &               NMAT,NBCOMM,   R,RINI,IRTETI)
C RESPONSABLE JMBHH01 J.M.PROIX
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/03/2007   AUTEUR ELGHARIB J.EL-GHARIB 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ----------------------------------------------------------------
C       MONOCRISTAL        : CONTROLE DE LA CONVERGENCE
C                                  DE LA CONFORMITE DE LA SOLUTION DP
C                                  ET DE LA RE-INTEGRATION
C                                  ET DU REDECOUPAGE DU PAS DE TEMPS
C                                  SUR LA NORME DU RESIDU
C       ----------------------------------------------------------------
C       IN 
C            DY     :  VECTEUR SOLUTION DY = ( DSIG DVINT)
C            DDY    :  VECTEUR CORRECTION SUR LA SOLUTION
C            NR     :  DIMENSION DY DDY
C            ITMAX  :  NB MAXI D ITERATIONS LOCALES
C            TOLER  :  TOLERANCE A CONVERGENCE
C            ITER   :  NUMERO ITERATION COURANTE
C            NMAT   :  DIMENSION MATER
C            NBCOMM :  INCIDES DES COEF MATERIAU
C            R      :  R(Y) RESIDU A L'ITERATION COURANTE
C            RINI   :  R(Y0) RESIDU A L'ITERATION 1
C       OUT  IRTETI  :  =0 CONVERGENCE
C                       =1 ITERATIONS SUPPLEMENTAIRE (ITER<ITMAX)
C                       =3 ITMAX ATTEINT REDECOUPAGE LOCAL
C                       =4 ITMAX ATTEINT  REDECOUPAGE GLOBAL
C       ----------------------------------------------------------------
        INTEGER         ITMAX,  ITER,  NR,IRTETI,NDT,NDI,I,NMAT,MONO1
        REAL*8          TOLER,  DDY(NR), DY(NR), R(NR),RINI(NR)
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
        REAL*8          ERRDY(NR), ERRR(NR)
C        REAL*8          TER(100)
C        SAVE            TER
        INTEGER         NBCOMM(NMAT,3)
C       ----------------------------------------------------------------
C
C -   EVALUATION  DE L'ERREUR ABSOLUE EN RESIDU (DEFORMATIONS)
C
      
      ERRR(1)=0.D0                               
      DO 100 I = 1,NR                            
         ERRR(1) = MAX(ERRR(1), ABS(R(I)))       
 100  CONTINUE                                   

C       ERRDY(1)=0.D0                              
C       DO 101 I = NDT+1,NR                        
C          ERRDY(1) = MAX(ERRDY(1), ABS(DY(I)))    
C  101  CONTINUE                                   

 
C      TER(ITER) = ERRR(1)

C -         ITER < ITMAX
C           ------------

      IF ( ITER .LE. ITMAX ) THEN
C
C -             CONVERGENCE
C
          IF ( ERRR(1) .LE. TOLER ) THEN
             IRTETI = 0
             GOTO 9999
          ENDIF
C
C -     NON CONVERGENCE ITERATION SUIVANTE
C
C          IF((ITER.GE.15).AND.(ITMAX.GE.15)) THEN
C             IF ((TER(ITER) .LT. TER(ITER-1)).AND.
C     1           (TER(ITER-1) .LT. TER(ITER-2))   ) THEN
C                IRTETI = 1
C                GOTO 9999
C             ELSE 
C                IRTETI = 3
C                GOTO 9999             
C             ENDIF
C          ELSE
               IRTETI = 1
               GOTO 9999             
C          ENDIF
C
      ELSE
         IRTETI=3
      ENDIF
C
 9999 CONTINUE
      END
