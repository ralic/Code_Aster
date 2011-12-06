      SUBROUTINE CALTAU(COMP,IFA,IS,SIGF,FKOOH,NFS,NSG,TOUTMS,
     &                  TAUS,MUS,MSNS)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/09/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----------------------------------------------------------------
C     
C     MONOCRISTAL : calcul de la scission reduite sur le systeme IS   
C     IN  COMP   : NOM COMPORTEMENT
C     IN  IFA    : NUMERO FAMILLE
C         IS     : NUMERO DU SYST. GLIS. ACTUEL
C         SIGF   : TENSEUR CONTRAINTES ACTUEL (TENSEUR S EN GDEF)
C     IN  FKOOH  : INVERSE TENSEUR HOOKE
C        TOUTMS  :  TOUS LES TENSEURS MUS=sym(MS*NS) en HPP, 
C                   TOUS LES VECTEURS MS ET NS en gdef
C     OUT  TAUS  :  scission reduite sur le systeme IS
C     OUT  MUS   :  sym(MS * NS)
C     OUT  MSNS  :  MS * NS

      INTEGER J, I,IS,IFA,NFS,NSG
      REAL*8 TAUS,MUS(6),MSNS(3,3),ID6(6),NS(3),MS(3),SIGF(6)
      REAL*8 FESIG(3,3),S(3,3),FETFE(3,3),FETFE6(6)
      REAL*8 TOUTMS(NFS,NSG,6),FKOOH(6,6)
      CHARACTER*16 COMP(*)
C     ----------------------------------------------------------------
      DATA ID6/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

      
      IF (COMP(3)(1:5).EQ.'PETIT') THEN
      
C        CALCUL DE LA SCISSION REDUITE =
C        PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
C        TAU      : SCISSION REDUITE TAU=SIG:MUS

         DO 101 I=1,6
            MUS(I)=TOUTMS(IFA,IS,I)
 101      CONTINUE
 
         TAUS=0.D0
         
         DO 10 I=1,6
            TAUS=TAUS+SIGF(I)*MUS(I)
 10      CONTINUE
 
      ELSE
      
C        CONTRAINTES PK2          
C Y contient : SIGF=PK2 (sans les SQRT(2) !), puis les alpha_s
         CALL LCPRMV(FKOOH,SIGF,FETFE6)
         CALL DSCAL(6,2.D0,FETFE6,1)
         CALL DAXPY(6,1.D0,ID6,1,FETFE6,1)

         DO 109 I=1,3
            MS(I)=TOUTMS(IFA,IS,I)
            NS(I)=TOUTMS(IFA,IS,I+3)
 109     CONTINUE

         DO 110 I=1,3
         DO 110 J=1,3
            MSNS(I,J)=MS(I)*NS(J)
 110     CONTINUE
 
         CALL TNSVEC(6,3,FETFE, FETFE6, 1.D0)
         CALL TNSVEC(6,3,S, SIGF, 1.D0)
 
         CALL PMAT(3,FETFE,S,FESIG)
 
         TAUS=0.D0
         DO 20 I=1,3
         DO 20 J=1,3
           TAUS=TAUS + FESIG(I,J)*MSNS(I,J)
 20      CONTINUE
 
      ENDIF

      END
