      SUBROUTINE RVRLLN(XY,TN,N,REPERE,V1,V2)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
      CHARACTER*8 REPERE
      INTEGER      TN(*),N
      REAL*8       XY(*),V1(*),V2(*)
C
C***********************************************************************
C
C  OPERATION REALISEE
C  ------------------
C
C     CALCUL  DU REPERE LOCAL OU POLAIRE LA LONG D' UNE LISTE DE NOEUDS
C
C  ARGUMENTS EN ENTREE
C  -------------------
C
C     XY : COORDONNEES DES NOEUDS DU MAILLAGE
C     TN : LISTE DES NOEUDS
C     N  : NBR DE NOEUDS
C
C  ARGUMENTS EN SORTIE
C  -------------------
C
C     V1,V2 : VECTEURS DE LA BASE CALCULEES
C
C***********************************************************************
C
C  VARIABLES LOCALES
C  -----------------
C
      INTEGER      I
      REAL*8       XC,XS,YC,YS,ZZC,ZZS,L
      REAL*8       T1S,T2S,N1S,N2S,T1P,T2P,XAUX,YAUX,ZAUX,N1P,N2P
C
C====================== CORPS DE LA ROUTINE ===========================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      XC  = XY(3*(TN(1)-1)+1)
      YC  = XY(3*(TN(1)-1)+2)
      ZZC = XY(3*(TN(1)-1)+3)
C
      DO 10, I = 1, N, 1
C
         IF ( I .NE. N ) THEN
C
            XS  = XY(3*(TN(I+1)-1)+1)
            YS  = XY(3*(TN(I+1)-1)+2)
            ZZS = XY(3*(TN(I+1)-1)+3)
C
         ENDIF
C
         IF ( REPERE(1:5) .EQ. 'LOCAL' ) THEN
C
            XAUX = XS  - XC
            YAUX = YS  - YC
            ZAUX = ZZS - ZZC
C
            IF ( I .NE. N ) THEN
C
               L = SQRT(XAUX*XAUX + YAUX*YAUX + ZAUX*ZAUX)
               L = 1.0D0/L
C
            ENDIF
C
            T1S =  XAUX*L
            T2S =  YAUX*L
            N1S = -T2S
            N2S =  T1S
C
            IF ( I .EQ. 1 ) THEN
C
               V1(2*I-1) =  T1S
               V1(2*I  ) =  T2S
               V2(2*I-1) =  N1S
               V2(2*I  ) =  N2S
C
            ELSE IF ( I .NE. N ) THEN
C
               V1(2*I-1) =  0.5D0*(T1S + T1P)
               V1(2*I  ) =  0.5D0*(T2S + T2P)
               V2(2*I-1) =  0.5D0*(N1S + N1P)
               V2(2*I  ) =  0.5D0*(N2S + N2P)
C
            ELSE
C
               V1(2*I-1) =  T1P
               V1(2*I  ) =  T2P
               V2(2*I-1) =  N1P
               V2(2*I  ) =  N2P
C
            ENDIF
C
            IF ( I .NE. N ) THEN
C
               T1P =  T1S
               T2P =  T2S
               N1P =  N1S
               N2P =  N2S
C
            ENDIF
C
         ELSE
C
            CALL RVRTHE(XC,YC,V1(2*I-1),V1(2*I),V2(2*I-1),V2(2*I))
C
         ENDIF
C
         XC  = XS
         YC  = YS
         ZZC = ZZS
C
10    CONTINUE
C
      END
