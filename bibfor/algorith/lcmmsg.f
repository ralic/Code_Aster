        SUBROUTINE LCMMSG(NOMFAM,NBSYS,NUSYS,PGL,MS)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/11/2004   AUTEUR JMBHH01 J.M.PROIX 
C RESPONSABLE JMBHH01 J.M.PROIX
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
C ======================================================================
C       IN  FAMSYS  :  NOM FAMILLE SYS GLIS
C           NUSYS   :  NUMERO SYS GLIS (FACULTATIF)
C           PGL     :  MATRICE DE PASSAGE REPERE GLOBAL REPERE LOCAL
C     OUT:
C           NBSYS    : NOMBRE DE SYS GLIS
C           MS       : TENSEUR MS POUR LE SYS GLIS NUMERO NUSYS
C
      CHARACTER*16 NOMFAM
      REAL*8 MS(6), N(12,3), L(12,3), PGL(3,3),NG(3),NL(3),LG(3),LL(3)
      REAL*8 SQRT2,SQRT3
      INTEGER NBSYS,NUSYS, K, J
C     ----------------------------------------------------------------

      NBSYS=0
      SQRT2=SQRT(2.D0)
      SQRT3=SQRT(3.D0)
      CALL LCINVN( 6 , 0.D0 , MS )
      IF (NOMFAM.EQ.'BASAL') THEN
C HCP LATTICE, BASAL PLANE {001}={0001}      
         NBSYS=3
         IF (NUSYS.EQ.0)  GOTO 9999
         N(1,1)=0.D0
         N(1,2)=0.D0
         N(1,3)=1.D0
         N(2,1)=0.D0
         N(2,2)=0.D0
         N(2,3)=1.D0
         N(3,1)=0.D0
         N(3,2)=0.D0
         N(3,3)=1.D0
         L(1,1)=1.D0
         L(1,2)=0.D0
         L(1,3)=0.D0
         L(2,1)=0.D0
         L(2,2)=1.D0
         L(2,3)=0.D0
         L(3,1)=1.D0/SQRT2
         L(3,2)=1.D0/SQRT2
         L(3,3)=0.D0
      ELSE IF (NOMFAM.EQ.'PRISMATIQUE') THEN
         NBSYS=3
         IF (NUSYS.EQ.0)  GOTO 9999
         N(1,1)=0.D0
         N(1,2)=0.D0
         N(1,3)=0.D0
         N(2,1)=0.D0
         N(2,2)=0.D0
         N(2,3)=0.D0
         N(3,1)=0.D0
         N(3,2)=0.D0
         N(3,3)=0.D0
         L(1,1)=1.D0
         L(1,2)=1.D0
         L(1,3)=0.D0
         L(2,1)=1.D0
         L(2,2)=1.D0
         L(2,3)=0.D0
         L(3,1)=1.D0
         L(3,2)=1.D0
         L(3,3)=0.D0
C   N ET L DOIVENT ETRE UNITAIRES
      DO 120 J=1,3
      DO 120 K=1,3
         L(J,K)=L(J,K)/SQRT2
 120  CONTINUE
      ELSE IF (NOMFAM.EQ.'OCTAEDRIQUE') THEN
C FCC LATTICE      
         NBSYS=12
         IF (NUSYS.EQ.0)  GOTO 9999
         N(1,1)=1.D0
         N(1,2)=1.D0
         N(1,3)=1.D0
         N(2,1)=1.D0
         N(2,2)=1.D0
         N(2,3)=1.D0
         N(3,1)=1.D0
         N(3,2)=1.D0
         N(3,3)=1.D0
         N(4,1)=1.D0
         N(4,2)=-1.D0
         N(4,3)=1.D0
         N(5,1)=1.D0
         N(5,2)=-1.D0
         N(5,3)=1.D0
         N(6,1)=1.D0
         N(6,2)=-1.D0
         N(6,3)=1.D0
         N(7,1)=-1.D0
         N(7,2)=1.D0
         N(7,3)=1.D0
         N(8,1)=-1.D0
         N(8,2)=1.D0
         N(8,3)=1.D0
         N(9,1)=-1.D0
         N(9,2)=1.D0
         N(9,3)=1.D0
         N(10,1)=-1.D0
         N(10,2)=-1.D0
         N(10,3)=1.D0
         N(11,1)=-1.D0
         N(11,2)=-1.D0
         N(11,3)=1.D0
         N(12,1)=-1.D0
         N(12,2)=-1.D0
         N(12,3)=1.D0
         L(1,1)=-1.D0
         L(1,2)=0.D0
         L(1,3)=1.D0
         L(2,1)=0.D0
         L(2,2)=-1.D0
         L(2,3)=1.D0
         L(3,1)=-1.D0
         L(3,2)=1.D0
         L(3,3)=0.D0
         L(4,1)=-1.D0
         L(4,2)=0.D0
         L(4,3)=1.D0
         L(5,1)=0.D0
         L(5,2)=1.D0
         L(5,3)=1.D0
         L(6,1)=1.D0
         L(6,2)=1.D0
         L(6,3)=0.D0
         L(7,1)=0.D0
         L(7,2)=-1.D0
         L(7,3)=1.D0
         L(8,1)=1.D0
         L(8,2)=1.D0
         L(8,3)=0.D0
         L(9,1)=1.D0
         L(9,2)=0.D0
         L(9,3)=1.D0
         L(10,1)=-1.D0
         L(10,2)=1.D0
         L(10,3)=0.D0
         L(11,1)=1.D0
         L(11,2)=0.D0
         L(11,3)=1.D0
         L(12,1)=0.D0
         L(12,2)=1.D0
         L(12,3)=1.D0    
C   N ET L DOIVENT ETRE UNITAIRES
      DO 12 J=1,12
      DO 12 K=1,3
         L(J,K)=L(J,K)/SQRT2
         N(J,K)=N(J,K)/SQRT3
 12   CONTINUE     
      ELSE IF (NOMFAM.EQ.'PYRAMIDAL1') THEN
         CALL UTMESS('F','LCMMSG','PYRAMIDAL1 PAS ENCORE DISPONIBLE')
      ELSE IF (NOMFAM.EQ.'PYRAMIDAL2') THEN
         CALL UTMESS('F','LCMMSG','PYRAMIDAL2 PAS ENCORE DISPONIBLE')
      ELSE IF (NOMFAM.EQ.'CUBIQUE1') THEN
C BCC LATTICE, {110} SLIP      
         NBSYS=12
         IF (NUSYS.EQ.0)  GOTO 9999
         N(1,1)=0.D0
         N(1,2)=1.D0
         N(1,3)=1.D0
         N(2,1)=1.D0
         N(2,2)=0.D0
         N(2,3)=1.D0
         N(3,1)=1.D0
         N(3,2)=-1.D0
         N(3,3)=0.D0
         N(4,1)=0.D0
         N(4,2)=1.D0
         N(4,3)=-1.D0
         N(5,1)=1.D0
         N(5,2)=0.D0
         N(5,3)=1.D0
         N(6,1)=1.D0
         N(6,2)=1.D0
         N(6,3)=0.D0
         N(7,1)=0.D0
         N(7,2)=1.D0
         N(7,3)=1.D0
         N(8,1)=1.D0
         N(8,2)=0.D0
         N(8,3)=-1.D0
         N(9,1)=1.D0
         N(9,2)=1.D0
         N(9,3)=0.D0
         N(10,1)=0.D0
         N(10,2)=1.D0
         N(10,3)=-1.D0
         N(11,1)=1.D0
         N(11,2)=0.D0
         N(11,3)=-1.D0
         N(12,1)=1.D0
         N(12,2)=-1.D0
         N(12,3)=0.D0
         L(1,1)=1.D0
         L(1,2)=1.D0
         L(1,3)=-1.D0
         L(2,1)=1.D0
         L(2,2)=1.D0
         L(2,3)=-1.D0
         L(3,1)=1.D0
         L(3,2)=1.D0
         L(3,3)=-1.D0
         L(4,1)=1.D0
         L(4,2)=-1.D0
         L(4,3)=-1.D0
         L(5,1)=1.D0
         L(5,2)=-1.D0
         L(5,3)=-1.D0
         L(6,1)=1.D0
         L(6,2)=-1.D0
         L(6,3)=-1.D0
         L(7,1)=1.D0
         L(7,2)=-1.D0
         L(7,3)=1.D0
         L(8,1)=1.D0
         L(8,2)=-1.D0
         L(8,3)=1.D0
         L(9,1)=1.D0
         L(9,2)=-1.D0
         L(9,3)=1.D0
         L(10,1)=1.D0
         L(10,2)=1.D0
         L(10,3)=1.D0
         L(11,1)=1.D0
         L(11,2)=1.D0
         L(11,3)=1.D0
         L(12,1)=1.D0
         L(12,2)=1.D0
         L(12,3)=1.D0
C   N ET L DOIVENT ETRE UNITAIRES
      DO 123 J=1,12
      DO 123 K=1,3
         L(J,K)=L(J,K)/SQRT3
         N(J,K)=N(J,K)/SQRT2
 123  CONTINUE
         ELSE IF (NOMFAM.EQ.'CUBIQUE2') THEN
C BCC LATTICE, {211} SLIP      
         NBSYS=12
         IF (NUSYS.EQ.0)  GOTO 9999
         N(1,1)=2.D0
         N(1,2)=-1.D0
         N(1,3)=1.D0
         N(2,1)=1.D0
         N(2,2)=-2.D0
         N(2,3)=-1.D0
         N(3,1)=1.D0
         N(3,2)=1.D0
         N(3,3)=2.D0
         N(4,1)=2.D0
         N(4,2)=1.D0
         N(4,3)=1.D0
         N(5,1)=1.D0
         N(5,2)=2.D0
         N(5,3)=-1.D0
         N(6,1)=1.D0
         N(6,2)=-1.D0
         N(6,3)=2.D0
         N(7,1)=2.D0
         N(7,2)=1.D0
         N(7,3)=-1.D0
         N(8,1)=1.D0
         N(8,2)=2.D0
         N(8,3)=1.D0
         N(9,1)=1.D0
         N(9,2)=-1.D0
         N(9,3)=-2.D0
         N(10,1)=2.D0
         N(10,2)=-1.D0
         N(10,3)=-1.D0
         N(11,1)=1.D0
         N(11,2)=-2.D0
         N(11,3)=1.D0
         N(12,1)=1.D0
         N(12,2)=1.D0
         N(12,3)=-2.D0
         L(1,1)=1.D0
         L(1,2)=1.D0
         L(1,3)=-1.D0
         L(2,1)=1.D0
         L(2,2)=1.D0
         L(2,3)=-1.D0
         L(3,1)=1.D0
         L(3,2)=1.D0
         L(3,3)=-1.D0
         L(4,1)=1.D0
         L(4,2)=-1.D0
         L(4,3)=-1.D0
         L(5,1)=1.D0
         L(5,2)=-1.D0
         L(5,3)=-1.D0
         L(6,1)=1.D0
         L(6,2)=-1.D0
         L(6,3)=-1.D0
         L(7,1)=1.D0
         L(7,2)=-1.D0
         L(7,3)=1.D0
         L(8,1)=1.D0
         L(8,2)=-1.D0
         L(8,3)=1.D0
         L(9,1)=1.D0
         L(9,2)=-1.D0
         L(9,3)=1.D0
         L(10,1)=1.D0
         L(10,2)=1.D0
         L(10,3)=1.D0
         L(11,1)=1.D0
         L(11,2)=1.D0
         L(11,3)=1.D0
         L(12,1)=1.D0
         L(12,2)=1.D0
         L(12,3)=1.D0
C   N ET L DOIVENT ETRE UNITAIRES
      DO 124 J=1,12
      DO 124 K=1,3
         L(J,K)=L(J,K)/SQRT3
         N(J,K)=N(J,K)/2.D0
 124  CONTINUE
      ELSE IF (NOMFAM.EQ.'MACLAGE') THEN
C FCC LATTICE
         NBSYS=1
         IF (NUSYS.EQ.0)  GOTO 9999
         N(1,1)=1.D0
         N(1,2)=1.D0
         N(1,3)=1.D0
         L(1,1)=1.D0
         L(1,2)=1.D0
         L(1,3)=-2.D0
C   N ET L DOIVENT ETRE UNITAIRES
      DO 125 J=1,1
      DO 125 K=1,3
         L(J,K)=L(J,K)/2.D0
         N(J,K)=N(J,K)/SQRT3
 125  CONTINUE
      ELSE IF (NOMFAM.EQ.'JOINT_GRAIN') THEN
         CALL UTMESS('F','LCMMSG','JOINT_GRAIN PAS ENCORE DISPONIBLE')
      ELSE IF (NOMFAM.EQ.'RL') THEN
         CALL UTMESS('F','LCMMSG','RL PAS ENCORE DISPONIBLE')
      ELSE IF (NOMFAM.EQ.'UNIAXIAL') THEN
         NBSYS=1
         IF (NUSYS.EQ.0)  GOTO 9999
         N(1,1)=1.D0
         N(1,2)=0.D0
         N(1,3)=0.D0
         L(1,1)=1.D0
         L(1,2)=0.D0
         L(1,3)=0.D0
      ENDIF
C     POUR LE SYSTEME K, EXPRESSION DE N ET L DANS REPERE GLOBAL
      K=NUSYS
      DO 10 J=1,3
         NL(J)=N(K,J)
         LL(J)=L(K,J)
 10   CONTINUE
      CALL UTPVLG(1,3,PGL,NL,NG)
      CALL UTPVLG(1,3,PGL,LL,LG)

      DO 11 J=1,3
         MS(J)=NG(J)*LG(J)
11    CONTINUE
C     SQRT(2) PAR HOMOGENEITE AVEC NMPL3D.
      MS(4)=0.5D0*(NG(1)*LG(2)+NG(2)*LG(1))*SQRT2
      MS(5)=0.5D0*(NG(1)*LG(3)+NG(3)*LG(1))*SQRT2
      MS(6)=0.5D0*(NG(2)*LG(3)+NG(3)*LG(2))*SQRT2
C REMARQUE : INVERSION 13 ET 23 PAR RAPPORT A DEFI_TEXTURE OP0181      

      

9999  CONTINUE      
      END    
