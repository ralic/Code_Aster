      SUBROUTINE LCMMDH(COEFT,IFA,NMAT,NBCOMM,ALPHAP,HSR,NBSYS,IS,
     &                  HS,SOMS1,SOMS2,SOMS3)
      IMPLICIT NONE
      INTEGER IFA,NMAT,NBCOMM(NMAT,3),COPLA(12,3),FOREST(12,9),IS,NBSYS
      REAL*8 COEFT(*),ALPHAP(12),HS,HSR(5,24,24),SOMS1,SOMS2,SOMS3

C TOLE CRP_21
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/07/2010   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
C ======================================================================
C  CALCUL DE LA FONCTION H(OMEGA) POUR LA LOI D'ECOULEMENT  DD-CFC
C       IN  COEFT   :  PARAMETRES MATERIAU
C           IFA     :  NUMERO DE FAMILLE
C           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           NMAT    :  NOMBRE DE MATERIAUX
C           ALPHAP  :  ALPHA =RHO*B**2 (TOTAL) A T+DT
C     OUT:
C           HS      :  FONCTION D'EVOLUTION DENSITE DISLOCATION
C           SOMS1    :  SOMME(j=1,12)(SQRT(a_sj omega_j))
C           SOMS2    :  SOMME(j=forest(s))(SQRT(a_sj) omega_j)
C           SOMS3    :  SOMME(j=copla(s))(SQRT(a_sj omega_j))
C     ----------------------------------------------------------------
      REAL*8 A,B,Y,TERMEA,TERMEB,TERMEY,DENOM,CEFF,RMIN,BETA,NUMER
      REAL*8 ALPHAS,R8MIEM,DCDALS
      INTEGER IEI,NUMHSR,IU,IV,IFL,IS3,IV3
C     ----------------------------------------------------------------

      RMIN=R8MIEM()
      IFL=NBCOMM(IFA,1)
      A     =COEFT(IFL+3)
      B     =COEFT(IFL+4)
      Y     =COEFT(IFL+6)
      IEI=NBCOMM(IFA,3)
      BETA  =COEFT(IEI+2)
      NUMHSR=NINT(COEFT(IEI+5))
      
C     EVOLUTION DE LA DENSITE DE DISLO 
      TERMEA=0.D0
      DENOM=0.D0
      DO 50 IU=1,12
C         PARTIE POSITIVE DE ALPHA
          IF (ALPHAP(IU).GT.0.D0) THEN
             DENOM=DENOM+SQRT(HSR(NUMHSR,IS,IU)*ALPHAP(IU))
          ENDIF
 50   CONTINUE
C     SOMME SUR FOREST(S)
      IF (DENOM.GT.RMIN) THEN
C        TERME AU NUMERATEUR SUR FOREST(S)
         TERMEA=0.D0
         NUMER=0.D0
         DO 51 IV=1,12
            IS3=(IS-1)/3
            IV3=(IV-1)/3
            IF (IS3.NE.IV3) THEN
C              PARTIE POSITIVE DE ALPHA
               IF (ALPHAP(IV).GT.0.D0) THEN
                  NUMER=NUMER+SQRT(HSR(NUMHSR,IS,IV))*ALPHAP(IV)
               ENDIF
            ENDIF
 51      CONTINUE
         TERMEA=A*NUMER/DENOM
      ENDIF
      
C     SOMME SUR COPLA(S)
      TERMEB=0.D0
      IF (NBSYS.EQ.12) THEN        
      DO 52 IV=1,12
         IS3=(IS-1)/3
         IV3=(IV-1)/3
C        PARTIE POSITIVE DE ALPHA
         IF (IS3.EQ.IV3) THEN
         IF (ALPHAP(IV).GT.0.D0) THEN
            TERMEB=TERMEB+SQRT(HSR(NUMHSR,IS,IV)*ALPHAP(IV))
         ENDIF
         ENDIF
 52   CONTINUE
      ELSEIF (NBSYS.EQ.1) THEN
         ALPHAS=ALPHAP(IS)
C        PARTIE POSITIVE DE ALPHA
         IF (ALPHAS.GT.0.D0) THEN
            TERMEB=TERMEB+SQRT(HSR(NUMHSR,IS,IS)*ALPHAS)
         ENDIF
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF
      
      CALL LCMMDC(COEFT,IFA,NMAT,NBCOMM,ALPHAP,IS,CEFF,DCDALS)
      
C     TERME -Y*RHO_S
      IF (ALPHAP(IS).GT.0.D0) THEN
         TERMEY=-Y*ALPHAP(IS)/BETA
      ENDIF
      HS=(TERMEA+TERMEB*B*CEFF+TERMEY)
      SOMS1 = DENOM
      SOMS2 = NUMER
      SOMS3 = TERMEB
      END
