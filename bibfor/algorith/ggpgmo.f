      SUBROUTINE GGPGMO(S,H,THETA,DEUXMU,G,DEVPKK,DGDST,DGDEV,TSCHEM)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
CDEB
C---------------------------------------------------------------
C     VITESSE DE DEF. VISQUEUSE ET SA DERIVEE PAR RAPPORT A SIGEQ
C---------------------------------------------------------------
C IN  S     :R: CONTRAINTE EQUIVALENTE SIGMA
C IN  H     :R: PRESSION HYDROSTATIQUE SIGMA
C     THETA :R: PARAMETRE DU SCHEMA D'INTEGRATION (0.5 OU 1)
C                  THETA = 0.5 -> SEMI-IMPLICITE
C                  THETA = 1.0 -> IMPLICITE
C OUT G     :R: VALEUR DE LA FONCTION G
C     DEVPKK:R: COMPOSANTE SPHERIQUE VITESSE DEF. VISCO. (GD/3)
C     DGDST :R: DERIVEE TOTALE DE G PAR RAPPORT A SIGMA
C     DGDEV :R: DERIVEE PARTIELLE DE G PAR RAPPORT A EV (I.E. DPC)
C---------------------------------------------------------------
C            DANS LE CAS DE LA LOI GATT-MONERIE,
C     CETTE ROUTINE CALCULE LES FONCTIONS G ET GD DE L'ECOULEMENT 
C     VISCOPLASTIQUE
C
C     ET LA DERIVEE TOTALE DE CES FONCTIONS G ET GD PAR RAPPORT 
C     A SIGEQ
C---------------------------------------------------------------
CFIN
C---- DEBUT INSERTION C3 ---------------------------------------
C     COMMON POUR LES PARAMETRES DE LA LOI GATT-MONERIE
      COMMON / NMPAGM /AK1,AK2,XN1,XN2,EXPA1,EXPA2,EXPAB1,EXPAB2,A1,A2,
     &                 B1,B2,XW,XQ,XH,SIGE,SIGH,SIGH0,POROM,SGD
      REAL*8           AK1,AK2,XN1,XN2,EXPA1,EXPA2,EXPAB1,EXPAB2,A1,A2,
     &                 B1,B2,XW,XQ,XH,SIGE,SIGH,SIGH0,POROM,SGD
C
      REAL*8           TSCHEM,INV1
      
C-----------------------------------------------------------------------
      REAL*8 CTHETA ,CTHETP ,DEUXMU ,DEVPKK ,DGDEV ,DGDST ,EX1 
      REAL*8 EX2 ,EXP1 ,EXP2 ,G ,H ,PHIP ,PSI1 
      REAL*8 PSI1P ,PSI2 ,PSI2P ,S ,THETA ,TK ,TT 
      REAL*8 XTANH ,ZZ1 ,ZZ2 
C-----------------------------------------------------------------------
      IF (S.EQ.0.D0.AND.H.EQ.0D0)THEN
        G = 0.D0
        DEVPKK = 0.D0
        DGDST = 0.D0
        DGDEV = 0.D0
        GO TO 99
      ELSE
C     POTENTIEL BASSE CONTRAINTE (PSI1)
        EXP1= XN1 + 1.D0
        EX1 = 0.5D0*EXP1
        ZZ1 = A1*((1.5D0*H)**2) + B1*(S**2)
        PSI1= (AK1/EXP1)*(ZZ1**EX1)
C     POTENTIEL FORTE CONTRAINTE (PSI12)
        EXP2= XN2 + 1.D0
        EX2 = 0.5D0*EXP2
        ZZ2 = A2*((1.5D0*H)**2) + B2*(S**2)
        PSI2=(AK2/EXP2)*(ZZ2**EX2)
C     FONCTION COUPLAGE (CTHETA)
        TK = TSCHEM + 273.D0
        INV1 = SQRT(2.D0*S*S/3.D0+3.D0*H*H)
        TT = XW*(INV1**XQ)
        XTANH = TANH( (TK-TT)/XH )
        CTHETA = 0.5D0*(1 + XTANH )
C     CALCUL DE G
        IF (INV1.NE.0.D0) THEN
          PSI1P = S*AK1*B1*(ZZ1**(EX1-1.D0))
          PSI2P = S*AK2*B2*(ZZ2**(EX2-1.D0))
          PHIP = - 2.D0*XW*XQ*S*(INV1**(XQ-2.D0))/(3.D0*XH)
          CTHETP = 0.5D0*(1.D0-XTANH**2)*PHIP
          G = (1.D0-CTHETA)*PSI1P + CTHETA*PSI2P + (PSI2-PSI1)*CTHETP
          DGDST =  (1.D0-CTHETA)*( AK1*B1*(ZZ1**(EX1-1.D0))
     &                    +(XN1-1.D0)*AK1*((B1*S)**2)*(ZZ1**(EX1-2.D0))
     &                    )
     &         + CTHETA*( AK2*B2*(ZZ2**(EX2-1.D0))
     &                     +(XN2-1.D0)*AK2*((B2*S)**2)*(ZZ2**(EX2-2.D0))
     &                  )
     &         + 2.D0*CTHETP*(PSI2P-PSI1P)
     &         + (PSI2-PSI1)*(-(1.D0-XTANH**2)*XW*XQ*(INV1**(XQ-2.D0))
     &                          /(3.D0*XH)
     &                 + CTHETP*(2.D0*(XQ-2.D0)*S/(3.D0*INV1*INV1)
     &                                  - 2.D0*PHIP*XTANH ) )
        ELSE
          G = 0.D0
          DGDST = (1.D0-CTHETA)*( AK1*B1*(ZZ1**(EX1-1.D0)) )
     &            + CTHETA*( AK2*B2*(ZZ2**(EX2-1.D0)) )
        ENDIF
        DEVPKK = (1.D0-CTHETA)*H*AK1*A1*(ZZ1**(EX1-1.D0))
     &           + CTHETA*H*AK2*A2*(ZZ2**(EX2-1.D0))
C---  CORRECTION RM 24/02/2005 
        DEVPKK = DEVPKK*3.D0/4.D0
     &           + (PSI2-PSI1)*0.5D0*(1.D0-XTANH**2)
     &               *( -XW*XQ*H*(INV1**(XQ-2.D0))/XH )
C---- FIN INSERTION C3 -------------------------------------------
        DGDEV = 0.D0
      ENDIF
      G = G*THETA
      DGDST = DGDST*THETA
      DGDEV = DGDEV*THETA
      DEVPKK = DEVPKK*THETA
      IF (DGDST.LT.0.D0) THEN
         WRITE(*,*) 'GATT-MONE, DGDSEQ<0', DGDST,S,H
      ENDIF
   99 CONTINUE
C
      END
