      FUNCTION  SPECT4(XX,Y,XLC,VITN,RHOE,DEFM,NBP,IM,JM)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C-----------------------------------------------------------------------
C  CALCUL DE
C   F(X,Y) = RHOE(X)*RHOE(Y)*DEFM(X,I)*DEFM(Y,J)*U(X)*U(X)*
C            U(Y)*U(Y)*EXP(-ABS(Y-X)/XLC) 
C  (APPEL: ROUTINE PRINCIPALE OP0146)
C-----------------------------------------------------------------------
C IN  : XX,Y  : VARIABLES DE LA FONCTION F(XX,Y) 
C IN  : XLC   : LONGUEUR DE CORRELATION
C IN  : VITN  : VITESSE NORMALISEE,(VECTEUR DE DIM=2*NBP) 
C IN  : RHOE  : MASSE VOL. DU FLUIDE EXTERIEUR, (VECTEUR DE DIM=2*NBP) 
C IN  : DEFM  : DEFORMEES MODALES (CONCEPT MELASFLU)
C IN  : NBP   : NOMBRE DE POINTS DE LA DISCR. SPATIALE
C IN  : IM,IM : NUMEROS D ORDRE DES MODES DU CONCEPT MELASFLU
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
        REAL*8       DEFM(NBP,*),VITN(NBP,*),RHOE(NBP,*),XLC,XX,Y
C
C-----------------------------------------------------------------------
      IF (XX .LE. RHOE(2,1)) THEN
        I = 2
      ELSEIF (XX .GT. RHOE(NBP-1,1)) THEN
        I = NBP
      ELSE
        I =3
 10   CONTINUE
        IF (XX .GT. RHOE(I,1)) THEN
          I = I+1
          GOTO 10
        ENDIF
      ENDIF
C
      DELTA = (XX-RHOE(I-1,1)) / (RHOE(I,1)-RHOE(I-1,1))
C
      PHIX = DEFM(I-1,IM) + DELTA*(DEFM(I,IM)-DEFM(I-1,IM))
      UX   = VITN(I-1,2)  + DELTA*(VITN(I,2)-VITN(I-1,2))
      ROX  = RHOE(I-1,2)  + DELTA*(RHOE(I,2)-RHOE(I-1,2))
C
      IF (Y .LE. RHOE(2,1)) THEN
        I = 2
      ELSEIF (Y .GT. RHOE(NBP-1,1)) THEN
        I = NBP
      ELSE
        I =3
 20   CONTINUE
        IF (Y .GT. RHOE(I,1)) THEN
          I = I+1
          GOTO 20
        ENDIF
      ENDIF
C
      DELTA = (Y-RHOE(I-1,1)) / (RHOE(I,1)-RHOE(I-1,1))
C
      PHIY = DEFM(I-1,JM) + DELTA*(DEFM(I,JM)-DEFM(I-1,JM))
      UY   = VITN(I-1,2)  + DELTA*(VITN(I,2)-VITN(I-1,2))
      ROY  = RHOE(I-1,2)  + DELTA*(RHOE(I,2)-RHOE(I-1,2))
C
      SPECT4 = EXP(-ABS(XX-Y)/XLC) * PHIX * PHIY*
     +             ROX * ROY * UX*UX * UY*UY
C
      END
