      SUBROUTINE CAPACA(RHO0,RHO11,RHO12,RHO21,RHO22,SAT,
     +           PHI,CSIGM,CP11,CP12,CP21,CP22,K0,ALPHA0,T,COEPS,RETCOM)
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      INTEGER       RETCOM
      REAL*8        RHO0,RHO11,RHO12,RHO21,RHO22,SAT,PHI,CSIGM,ALPHA0,T
      REAL*8        K0,CP11,CP12,CP21,CP22,COEPS
C ======================================================================
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
C ======================================================================
C --- CALCUL DE LA CAPACITE CALORIFIQUE --------------------------------
C ======================================================================
      INTEGER       IADZI,IAZK24,UMESS,IUNIFI
      REAL*8        UMPRHS
      CHARACTER*8   NOMAIL
C ======================================================================
C ======================================================================
C --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
C --- ET VERIFICATION DES COHERENCES -----------------------------------
C ======================================================================
      UMPRHS = RHO0-(RHO11+RHO22)*SAT*PHI-(RHO12+RHO21)*(1.D0-SAT)*PHI
      IF (UMPRHS.LE.0.D0) THEN
         UMESS  = IUNIFI('MESSAGE')
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3) (1:8)
         WRITE (UMESS,9001) 'CAPACA',' RHOS(1-PHI) <=0 A LA MAILLE: ',
     +                                                           NOMAIL
         RETCOM = 1
         GO TO 30
      END IF
      COEPS = UMPRHS*CSIGM + PHI*SAT*(RHO11*CP11+RHO22*CP22) +
     +          PHI* (1.D0-SAT)* (RHO12*CP12+RHO21*CP21)
C ======================================================================
C --- CALCUL DE COEPS SI MECANIQUE SINON ALPHA0=0 ----------------------
C ======================================================================
      COEPS = COEPS - 9.D0*T*K0*ALPHA0*ALPHA0
      IF (COEPS.LE.0.D0) THEN
         UMESS  = IUNIFI('MESSAGE')
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3) (1:8)
         WRITE (UMESS,9001) 'CAPACA',' COEPS <=0 A LA MAILLE: ', NOMAIL
         RETCOM = 1
         GO TO 30
      END IF
C ======================================================================
 30   CONTINUE
C =====================================================================
 9001 FORMAT (A8,2X,A30,2X,A8)
C ======================================================================
      END
