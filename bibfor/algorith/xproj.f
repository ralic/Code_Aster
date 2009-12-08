      SUBROUTINE XPROJ(P,A,B,C,M,D,VN,EPS,IN)
      IMPLICIT NONE
      REAL*8   P(3),A(3),B(3),C(3),M(3),VN(3),EPS(3),D
      LOGICAL  IN
  
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/08/2006   AUTEUR MASSIN P.MASSIN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MASSIN P.MASSIN
C     ------------------------------------------------------------------
C
C       XPROJ   : X-FEM : PROJECTION D'UN POINT SUR UN TRIANGLE EN 3D
C       -----     -       ----
C    DANS LE CADRE DE XFEM, ON PROJETE LE POINT P SUR LE TRIANGLE ABC,ON
C     RENVOIE LE POINT PROJETE ET SES COORDONNES PARAMETRIQUE DANS ABC.
C     LA METHODE UTILISEE EST CELLE DECRTIE DANS LA DOC R5.03.50-B 
C
C    ENTREE
C        P(3)        :  COORDONNEES DU POINT P A PROJETER
C     A(3),B(3),C(3) :  COORDONNEES DU TRIANGLE ABC
C    SORTIE
C        M(3)   : COORDONNEES DE M PROJETE DE P SUR LE TRIANGLE ABC
C        D      : DISTANCE PM
C        VN(3)  : NORMALE RENTRANTE AU TRIANGLE ABC
C        EPS(3) : COORDONNEES PARAMETRIQUES DE M DANS ABC
C                 (APRES PROJECTION & CORRECTION)
C        IN     : VARIABLE LOGIQUE INDIQUANT SI LE POINT M SE TROUVE A
C                 L'INTERIEUR DU TRIANGLE ABC AVANT CORRECTION
C
C     ------------------------------------------------------------------

      INTEGER  I
      REAL*8   AB(3),BC(3),AC(3),AP(3),NORME,VNT(3),PS,PS1,PS2,DDOT,
     &         PADIST

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
      CALL JEMARQ()
      
      DO 100 I=1,3
         AB(I)=B(I)-A(I)
         BC(I)=C(I)-B(I)
         AC(I)=C(I)-A(I)
         AP(I)=P(I)-A(I)
 100  CONTINUE
 
C  CALCUL DE LA NORMALE A L'ISOZERO
C  PROJECTION DE P SUR LE TRIANGLE VOIR R5.03.50-B 
      CALL PROVEC(AB,AC,VN)              
      CALL NORMEV(VN,NORME)      
      CALL PROVEC(AP,VN,VNT)
      PS=DDOT(3,VNT,1,AC,1)
      EPS(1)=-1*PS/NORME              
      PS=DDOT(3,VNT,1,AB,1)
      EPS(2)=PS/NORME
      EPS(3)=1-EPS(1)-EPS(2)
                  
C  ON REPERE AVANT RECTIFICATION SI LA PROJECTION EST DANS LE TRIANGLE
      IN = .FALSE.
      IF (EPS(1).GE.0.D0.AND.EPS(2).GE.0.D0.AND.EPS(3).GE.0.D0)
     &   IN = .TRUE.
      
C  SI M EST DS LE SECTEUR 1
      IF (EPS(1).LT.0.D0) THEN                
         PS=DDOT(3,AC,1,AC,1)
         PS1=DDOT(3,AB,1,AC,1)
         EPS(2)=EPS(2)+EPS(1)*PS1/PS
         EPS(1)=0.D0
      ENDIF
C  SI M EST DS LE SECTEUR 2
      IF (EPS(2).LT.0.D0) THEN               
         PS=DDOT(3,AB,1,AB,1)
         PS1=DDOT(3,AB,1,AC,1)
         EPS(1)=EPS(1)+EPS(2)*PS1/PS
         EPS(2)=0.D0
      ENDIF
C  SI M EST DS LE SECTEUR 3
      IF (EPS(3).LT.0.D0) THEN                
         PS=DDOT(3,BC,1,BC,1)
         PS1=DDOT(3,AB,1,BC,1)
         PS2=DDOT(3,AC,1,BC,1)
         EPS(1)=(-1.D0*EPS(1)*PS1+(1.D0-EPS(2))*PS2)/PS
         EPS(2)=1.D0-EPS(1)
      ENDIF
      
C  ON FINIT DE RAMENER LES POINTS ENCORE DEHORS             
      IF (EPS(1).LT.0.D0) EPS(1)=0.D0
      IF (EPS(2).LT.0.D0) EPS(2)=0.D0
      IF (EPS(1).GT.1.D0) EPS(1)=1.D0
      IF (EPS(2).GT.1.D0) EPS(2)=1.D0
      
      EPS(3) = 1-EPS(1)-EPS(2)
      
      DO 200 I=1,3
         M(I)=A(I)+EPS(1)*AB(I)+EPS(2)*AC(I)
 200  CONTINUE                       
      
C  CALCUL DE LA DISTANCE PM
      D=PADIST(3,P,M)

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
