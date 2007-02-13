      SUBROUTINE HUJKSI (CARAC, MATER, R, KSI)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/02/2007   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     --------------------------------------------------------
C     LOI HUJEUX : CALCUL DU COEFFICIENT DE MOBILISATION
C     OU DE SA DERIVEE
C ============================================================
C   IN
C    CARAC (K6) : 'KSI'  CALCUL DE KSI(R) =
C                        [ (R - RHYS) / (RMOB - RHYS) ] **XM
C    		  'DKSIDR'   CALCUL DE LA DERIVEE DE KSI
C    MATER	:  COEFFICIENTS MATERIAU
C    R  	:  ECROUISSAGE COURANT (MECANISME DEVIATOIRE)
C   OUT
C    KSI (R)	:  VALEUR DE KSI OU DKSIDR
C     --------------------------------------------------------
      REAL*8 MATER(20,2), R, KSI, RHYS, RMOB, XM, XM1
      REAL*8 ZERO, UN
      CHARACTER*6   CARAC
      PARAMETER (ZERO = 0.D0)
      PARAMETER (UN = 1.D0)

       RHYS = MATER(15,2)
       RMOB = MATER(16,2)
       XM   = MATER(17,2)
       
       IF (CARAC(1:3).EQ.'KSI') THEN
       
         IF (R.GT.ZERO .AND. R.LE.RHYS) THEN
           KSI = ZERO
         ELSEIF (R.GT.RHYS .AND. R.LE.RMOB) THEN
           KSI = (R - RHYS)**XM /(RMOB - RHYS)**XM
         ELSEIF (R.GT.RMOB) THEN
           KSI = UN
         ELSE
           CALL U2MESS('A','COMPOR1_9')
         ENDIF
       
       ELSEIF (CARAC(1:6).EQ.'DKSIDR') THEN
       
         IF (R.GT.ZERO .AND. R.LE.RHYS) THEN
           KSI = ZERO
         ELSEIF (R.GT.RHYS .AND. R.LE.RMOB) THEN
           XM1 = XM-UN
           KSI = XM*(R - RHYS)**XM1 /(RMOB - RHYS)**XM
         ELSEIF (R.GT.RMOB) THEN
           KSI = ZERO
         ELSE
           CALL U2MESS('A','COMPOR1_9')
         ENDIF
         
       ELSE
         CALL U2MESS('F','COMPOR1_10')
       ENDIF
         
       END
