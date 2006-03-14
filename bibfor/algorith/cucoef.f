      SUBROUTINE CUCOEF(JCOEF,ICOEF,TYPCOE,INST,
     &                  COEF)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/03/2006   AUTEUR MABBAS M.ABBAS 
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
C
      IMPLICIT     NONE
      INTEGER      JCOEF
      INTEGER      ICOEF
      INTEGER      TYPCOE
      REAL*8       INST
      REAL*8       COEF
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CUPREP
C ----------------------------------------------------------------------
C
C RECUPERATION DES COEFFICIENTS DE LA RELATION LINEAIRE
C ET DU PSEUDO-JEU
C
C IN  JCOEF  : ADRESSE JEVEUX DES COEFFICIENTS
C IN  ICOEF  : INDICE DE LA COMPOSANTE DU COEFFICIENT
C IN  TYPCOE : TYPE DU COEFFICIENT (REEL OU FONCTION)
C IN  INST   : VALEUR DE L'INSTANT
C OUT COEF   : VALEUR DU COEFFICIENT
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      IRET
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C ----------------------------------------------------------------------


      IF (TYPCOE.EQ.1) THEN
        COEF = ZR(JCOEF-1+ICOEF)  
      ELSE IF (TYPCOE.EQ.2)  THEN
        CALL FOINTE('F',ZK8(JCOEF-1+ICOEF),1,'INST',INST,COEF,IRET)   
      ELSE
        CALL UTMESS('F','CUCOEF',
     &              'LIAISON UNILATERALE NI REELLE, NI FONCTION')
      ENDIF  

C ----------------------------------------------------------------------
      CALL JEDEMA()
      END
