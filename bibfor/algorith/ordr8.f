      SUBROUTINE ORDR8(TAB,NB,IORD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/01/98   AUTEUR CIBHHLB L.BOURHRARA 
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
C***********************************************************************
C    P. RICHARD     DATE //
C-----------------------------------------------------------------------
C  BUT:  TROUVER L'ORDRE CROISSANT D'UNE TABLE DE VALEUR R8
      IMPLICIT REAL*8 (A-H,O-Z)
C     PAS DE MODIFICATION DE L'ORDRE D'ENTREE MAIS DETERMINATION DE
C     POINTEUR D'ORDRE
C
C-----------------------------------------------------------------------
C
C TAB      /I/: TABLEAU A ORDONNER
C NB       /I/: TAILLAE DU TABLEAU A ORDONNER
C IORD     /O/: TABLE DES POINTEURS D'ORDRE
C
C-----------------------------------------------------------------------
C
      REAL*8 TAB(NB)
      INTEGER IORD(NB)
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      DO 10 I=1,NB
        IORD(I)=I
 10   CONTINUE
C
      DO 20 I=1,NB-1
        VMIN=TAB(IORD(I))
        IORMIN=I
        DO 30 J=I+1,NB
          IF(TAB(IORD(J)).LT.VMIN) THEN
            VMIN=TAB(IORD(J))
            IORMIN=J
          ENDIF
 30     CONTINUE
        ITEMP=IORD(I)
        IORD(I)=IORD(IORMIN)
        IORD(IORMIN)=ITEMP
 20   CONTINUE
C
 9999 CONTINUE
      END
