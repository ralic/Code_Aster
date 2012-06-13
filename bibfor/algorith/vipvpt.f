      SUBROUTINE VIPVPT(NBVARI,VINTM,VINTP,ADVICO,VICPVP,DIMCON,P2,
     +       CONGEM,ADCP11,ADCP12,NDIM,PVP0,DP1,DP2,T,DT,MAMOLV,R,RHO11,
     +                          KH,SIGNE,CP11,CP12,YATE,PVP,PVPM,RETCOM)
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      INTEGER       NBVARI,ADVICO,VICPVP,ADCP11,ADCP12,NDIM,DIMCON
      INTEGER       YATE,RETCOM
      REAL*8        VINTM(NBVARI),VINTP(NBVARI),CONGEM(DIMCON),PVP0,DP1
      REAL*8        DP2,T,DT,MAMOLV,R,RHO11,CP11,CP12,PVP,PVPM,P2,SIGNE
      REAL*8        KH
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_21
C ======================================================================
C --- BUT : CALCUL ET STOCKAGE DES PRESSIONS DE VAPEUR INTER PTILDE-----
C -------   DANS LES CAS AVEC AIR DISSOUS ------------------------------
C ======================================================================
      INTEGER       IADZI,IAZK24,UMESS,IUNIFI
      REAL*8        VARPV,EPXMAX,R8PREM
      PARAMETER    (EPXMAX = 5.D0)
      CHARACTER*8   NOMAIL
C ======================================================================
C ======================================================================
C --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
C --- ET VERIFICATION DES COHERENCES -----------------------------------
C ======================================================================
C      VARPV = MAMOLV*(1/R/T-1/KH)* DP2/RHO11-MAMOLV/R/T*DP1/RHO11
      VARPV = MAMOLV/R/T*(DP2-SIGNE*DP1)/RHO11-MAMOLV/RHO11/KH*DP2
      IF (YATE.EQ.1) THEN
         VARPV = VARPV+(CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))*
     +             (1.0D0/(T-DT)-1.0D0/T)*MAMOLV/R
         VARPV = VARPV+(CP12-CP11)*(LOG(T/(T-DT))-(DT/T))*MAMOLV/R
      ENDIF
      IF (VARPV.GT.EPXMAX) THEN
         UMESS  = IUNIFI('MESSAGE')
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3) (1:8)
         WRITE (UMESS,9001) 'VIPVPT','VARPV > EXPMAX A LA MAILLE: ',
     +                                                            NOMAIL
         RETCOM = 1
         GO TO 30
      ENDIF
      VINTP(ADVICO+VICPVP) = - PVP0 +
     +                           (VINTM(ADVICO+VICPVP)+PVP0)*EXP(VARPV)
      PVP  = VINTP(ADVICO+VICPVP) + PVP0
      PVPM = VINTM(ADVICO+VICPVP) + PVP0
      IF ((P2-PVP).LT.0.0D0) THEN
         UMESS  = IUNIFI('MESSAGE')
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3) (1:8)
         WRITE (UMESS,9001) 'VIPVPT','PGAZ-PVAP <=0 A LA MAILLE: ',
     +                                                            NOMAIL
         RETCOM = 1
         GO TO 30
      ENDIF
      IF ((PVP).LT.R8PREM()) THEN
         UMESS  = IUNIFI('MESSAGE')
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3) (1:8)
         WRITE (UMESS,9001) 'VIPVPT','PVAP =0 A LA MAILLE: ',NOMAIL
         RETCOM = 1
         GO TO 30
      ENDIF
C ======================================================================
 30   CONTINUE
C ======================================================================
 9001 FORMAT (A8,2X,A30,2X,A8)
C ======================================================================
      END
