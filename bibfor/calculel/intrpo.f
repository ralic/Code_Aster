      SUBROUTINE INTRPO(R,S,T,NNO,VH)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
C ......................................................................
C     CALCUL DES VALEURS DES FONCTONS D'INTERPOLATION
C     ET DE LEURS DERIVEES AU POINT (R,S,T)
C
C IN    NNO  : NOMBRE DE NOEUDS
C IN    R,S,T: COORDONNES DU POINT CONSIDERE
C OUT    VH  : VALEUR DE LA FONCTION D INTERPOLATION AU POINT
C ......................................................................
C
      REAL*8 VH(50),HR(3),HS(3),HT(3)
      INTEGER N27(27)
C
C-----------------------------------------------------------------------
      INTEGER I ,I1 ,IH ,J ,K ,NNO 
      REAL*8 DE ,HU ,R ,RM ,RP ,S ,SM 
      REAL*8 SP ,T ,TM ,TP ,UH ,UN 
C-----------------------------------------------------------------------
      DATA UN,DE,HU/1.D0,2.D0,8.D0/
      DATA N27/1,14,5,12,21,26,4,20,8,9,15,23,13,22,27,11,19,25,2,16,
     &         6,10,17,24,3,18,7/
C
C----------HEXAEDRE A 8 NOEUDS
C
      IF( NNO.EQ.8 ) THEN

        RM=UN-R
        RP=UN+R
        SM=UN-S
        SP=UN+S
        TM=UN-T
        TP=UN+T
        UH=UN/HU
C---------------FONCTIONS D INTERPOLATION
        VH(1)=UH*RM*SM*TM
        VH(2)=UH*RP*SM*TM
        VH(3)=UH*RP*SP*TM
        VH(4)=UH*RM*SP*TM
        VH(5)=UH*RM*SM*TP
        VH(6)=UH*RP*SM*TP
        VH(7)=UH*RP*SP*TP
        VH(8)=UH*RM*SP*TP
      END IF
C
C----------ELEMENTS A 20 OU A 27 NOEUDS
C
      IF( NNO.EQ.20 .OR. NNO.EQ. 27 ) THEN
        HR(1)=R*(R-UN)/DE
        HR(2)=UN-R*R
        HR(3)=R*(R+UN)/DE
        HS(1)=S*(S-UN)/DE
        HS(2)=UN-S*S
        HS(3)=S*(S+UN)/DE
        HT(1)=T*(T-UN)/DE
        HT(2)=UN-T*T
        HT(3)=T*(T+UN)/DE
C
        I1=0
        DO 131 I=1,3
        DO 131 J=1,3
        DO 131 K=1,3
        I1=I1+1
        IH=N27(I1)
        VH(IH)=HR(I)*HS(J)*HT(K)
  131   CONTINUE
      END IF
C
      END
