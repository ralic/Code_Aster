      SUBROUTINE ISGECO(ICOD1,ICOD2,NDIM,IOPT,ICOD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C***********************************************************************
C    P. RICHARD     DATE 19/02/91
C-----------------------------------------------------------------------
C  BUT:  GERER L'ADDITION OU LA SOUSTRACTION DES DEUX ENTIER CODES SUR
C   LES 7 PREMIERES PUISSANCE ENTIERES DE DEUX
      IMPLICIT NONE
C
C   SI IOPT=1     ADDITION ICOD1+ICOD2
C   SI IOPT=-1     ICOD1-ICOD2
C-----------------------------------------------------------------------
C
C ICOD1    /I/: PREMIER ENTIER CODE
C ICOD2    /I/: DEUXIEME  ENTIER CODE
C NDIM     /I/: NOMBRE DE PUISSANCE A DECODER
C IOPT     /I/: OPTION DE CALCUL
C ICOD     /O/: RESULTAT
C
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IK ,IOPT ,NBCPMX ,NDIM 
C-----------------------------------------------------------------------
      PARAMETER (NBCPMX=300)
      INTEGER   ICOD1(1), ICOD2(1), ICOD(1)
      INTEGER   IDEC1(NBCPMX), IDEC2(NBCPMX),IDEC(NBCPMX)
C
C-----------------------------------------------------------------------
C
      CALL ISDECO(ICOD1,IDEC1,NDIM)
      CALL ISDECO(ICOD2,IDEC2,NDIM)
C
      IF(IOPT.EQ.1) THEN
        DO 10 I=1,NDIM
          IK=IDEC1(I)+IDEC2(I)
          IF(IK.GT.0) THEN
            IDEC(I)=1
          ELSE
           IDEC(I)=0
          ENDIF
 10     CONTINUE
      ENDIF
C
      IF(IOPT.EQ.-1) THEN
        DO 20 I=1,NDIM
          IK=IDEC1(I)-IDEC2(I)
          IF(IK.GT.0) THEN
            IDEC(I)=1
          ELSE
           IDEC(I)=0
          ENDIF
 20     CONTINUE
      ENDIF
C
      CALL ISCODE(IDEC,ICOD,NDIM)
C
      END
