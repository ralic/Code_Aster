        SUBROUTINE LCMMJI( COEFT,IFA,NMAT,NBCOMM,NECRIS,
     &                     NUMS,VIS,NVI,VINI,DRSDPR,RP)
        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3),NUMS,NVI
        REAL*8 COEFT(NMAT),VIS(3),VINI(NVI)
        REAL*8 DRSDPR(NVI)
        CHARACTER*16 NECRIS
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C RESPONSABLE JMBHH01 J.M.PROIX
C ======================================================================
C  CALCUL DES DERIVEES DES VARIABLES INTERNES DES LOIS MONOCRISTALLINES
C  DERIVEE DE LA FONCTION D'ECROUISSAGE RS(PR)
C       IN  COEFT   :  PARAMETRES MATERIAU
C           IFA :  NUMERO DE FAMILLE
C           NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           NECRIS  :  NOM DE LA LOI D'ECROUISSAGE ISOTROPE
C           NECRCI  :  NOM DE LA LOI D'ECROUISSAGE CINEMATIQUE
C           NUMS    : NUMERO DU SYSTEME DE GLISSEMENT EN COURS
C           VIS : VARIABLES INTERNES DU SYSTEME DE GLISSEMENT COURANT
C           NVI  : NOMBRE TOTAL DE VARIABLES INTERNES
C           VINI : VARIABLES INTERNES DE TOUS LES SYSTEMES
C     OUT:
C          DRSDPR :  D(RS(P))/D(PR)
C           RP    : R(P)
C     ----------------------------------------------------------------
      REAL*8 P,R0,Q,H,B,RP,K,N,B1,B2,Q1,Q2,DRDP,DP,PR,DRDPR
      INTEGER IFL,IEI,IEC,NS, IS
C     ----------------------------------------------------------------

C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P

      CALL LCINVN(NVI, 0.D0, DRSDPR)
      
      IEI=NBCOMM(IFA,3)
      P=VIS(3)
C     NS=NOMBRE TOTAL DE SYSTEMES DE GLISSEMENT         
      NS=(NVI-1-6)/3
      CALL LCINVN(NVI, 0.D0, DRSDPR)

      
      IF (NECRIS.EQ.'ECRO_ISOT1') THEN
         R0=COEFT(IEI-1+1)
         Q=COEFT(IEI-1+2)
         B=COEFT(IEI-1+3)
         H=COEFT(IEI-1+4)
C        R(PS)=R0+Q*SOMME(HSR*(1-EXP(-B*PR))         
         RP=R0
         DO 10 IS=1,NS
C           HSR=H(1-DELTA_RS)+DELTA_RS
            IF (IS.EQ.NUMS) THEN
               RP=RP+Q*(1.D0-EXP(-B*P))
               DRDP=Q*B*EXP(-B*P)
               DRSDPR(IS)=DRDP
            ELSE
               PR=VINI(6+3*(IS-1)+3)
               RP=RP+Q*H*(1.D0-EXP(-B*PR))
               DRDPR=Q*H*B*EXP(-B*PR)
               DRSDPR(IS)=DRDPR
            ENDIF
 10      CONTINUE
         
      ELSEIF (NECRIS.EQ.'ECRO_ISOT2') THEN
         R0=COEFT(IEI-1+1)
         Q1=COEFT(IEI-1+2)
         B1=COEFT(IEI-1+3)
         H =COEFT(IEI-1+4)
         Q2=COEFT(IEI-1+5)
         B2=COEFT(IEI-1+6)         
         RP=R0
         DO 12 IS=1,NS
C           HSR=H(1-DELTA_RS)+DELTA_RS
            IF (IS.EQ.NUMS) THEN
               RP=RP+Q1*(1.D0-EXP(-B1*P))
               DRDP=Q1*B1*EXP(-B1*P)
               DRDP=DRDP+Q2*B2*EXP(-B2*P)
               DRSDPR(IS)=DRDP
            ELSE
               PR=VINI(6+3*(IS-1)+3)
               RP=RP+Q1*H*(1.D0-EXP(-B1*PR))
               DRDPR=Q1*H*B1*EXP(-B1*PR)
               DRSDPR(IS)=DRDPR
            ENDIF
 12      CONTINUE
         RP=RP+Q2*(1.D0-EXP(-B2*P))
  
      ENDIF
           
      END
