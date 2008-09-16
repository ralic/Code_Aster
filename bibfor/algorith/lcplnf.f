        SUBROUTINE LCPLNF (LOI, VIND,NBCOMM,NMAT,CPMONO,MATERF,
     &   ITER,NVI,ITMAX, TOLER,PGL, TOUTMS,HSR,DT,DY,YF,VINF)
     
C RESPONSABLE PROIX J-M.PROIX
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/09/2008   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       POST-TRAITEMETNS SPECIFIQUES AUX LOIS
C       CAS PARTICULIER DU  MONOCRISTAL  : 
C       ON GARDE 1 VARIABLE INTERNE PAR SYSTEME DE GLISSEMENT SUR 3
C       DEFORMATION PLASTIQUE EQUIVALENTE CUMULEE MACROSCOPIQUE
C       ----------------------------------------------------------------
C       IN 
C            MATERF :  COEF MATERIAU
C            NBCOMM :  INCIDES DES COEF MATERIAU
C            NMAT   :  DIMENSION MATER
C            NVI    :  NOMBRE DE VARIABLES INTERNES
C       OUT  YD     :  VECTEUR INITIAL
C       ----------------------------------------------------------------
        INTEGER         NDT,NVI,NMAT,NDI,NS,I,NBCOMM(NMAT,3),ITER,ITMAX
        REAL*8          MATERF(NMAT,2),VIND(*),TOLER,PGL(3,3),DT
        REAL*8          TOUTMS(5,24,6),HSR(5,24,24),DY(*),YF(*),VINF(*)
        COMMON /TDIM/   NDT  , NDI
        CHARACTER*16    LOI,CPMONO(5*NMAT+1)
        REAL*8          EPSEQ
C       ----------------------------------------------------------------

        IF ( LOI(1:7) .EQ. 'NADAI_B' ) THEN
           DO 10  I = 3 , NVI-1
              VIND ( I ) = 0.D0
   10      CONTINUE
        ENDIF
C
C --   DEFORMATION PLASTIQUE EQUIVALENTE CUMULEE MACROSCOPIQUE
C
        IF (LOI(1:8).EQ.'MONOCRIS') THEN
           CALL LCDPEC(VIND,NBCOMM,NMAT,NDT,CPMONO,MATERF,
     &      ITER,NVI,ITMAX, TOLER,PGL, TOUTMS,HSR,DT,DY,YF,VINF,EPSEQ)
        ENDIF
        
      END
