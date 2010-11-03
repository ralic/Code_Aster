      SUBROUTINE NMCEAI(NUMEDD,DEPDEL,DEPPR1,DEPPR2,DEPOLD,
     &                  SDPILO,RHO   ,ETA   ,F     )
C 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/11/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*24  NUMEDD
      CHARACTER*19  SDPILO,DEPDEL,DEPOLD,DEPPR1,DEPPR2
      REAL*8        ETA,RHO,F
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE - SELECTION PARAMETRE)
C
C CALCUL DU PARAMETRE DE SELECTION DE TYPE ANGL_INCR_DEPL
C      
C ----------------------------------------------------------------------
C
C
C IN  NUMEDD : NUME_DDL
C IN  SDPILO : SD PILOTAGE
C IN  DEPDEL : INCREMENT DE DEPLACEMENT DEPUIS DEBUT PAS DE TEMPS
C IN  DEPOLD : INCREMENT DE DEPLACEMENT PAS DE TEMPS PRECEDENT
C IN  DEPPR1 : INCREMENT DE DEPLACEMENT K-1.F_DONNE
C IN  DEPPR2 : INCREMENT DE DEPLACEMENT K-1.F_PILO
C IN  RHO    : PARAMETRE DE RECHERCHE LINEAIRE
C IN  ETA    : PARAMETRE DE PILOTAGE
C OUT F      : VALEUR DU CRITERE 
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      CHARACTER*8   K8BID
      REAL*8        SCA ,NODUP ,COEF
      INTEGER       JDEPDE,JDU0,JDU1,JDEPOL
      INTEGER       NEQ,IRET,I
      CHARACTER*19  SELPIL
      INTEGER       JPLSL
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      SCA    = 0.D0
      NODUP  = 0.D0
      F      = 0.D0
      CALL DISMOI('F','NB_EQUA',NUMEDD,'NUME_DDL',NEQ,K8BID,IRET)
C
C --- ACCES VECTEUR DE SELCTION CMP DX/DY/DZ
C      
      SELPIL = SDPILO(1:14)//'.PLSL'
      CALL JEVEUO(SELPIL(1:19)//'.VALE','L',JPLSL)
C
C --- ACCES AUX VECTEURS SOLUTIONS
C      
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(DEPPR1(1:19)//'.VALE','L',JDU0)
      CALL JEVEUO(DEPPR2(1:19)//'.VALE','L',JDU1)
      CALL JEVEUO(DEPOLD(1:19)//'.VALE','L',JDEPOL)  
C
C --- CALCUL DE L'ANGLE
C      
      DO 25 I = 1,NEQ
       COEF   = ZR(JPLSL-1+I)
       SCA    = SCA    + (ZR(JDEPOL+I-1)*(ZR(JDEPDE+I-1)
     &                 +  RHO*ZR(JDU0+I-1)
     &                 +  ETA*ZR(JDU1+I-1)))*COEF
       NODUP  = NODUP  + (ZR(JDEPDE+I-1)
     &                 + RHO*ZR(JDU0+I-1)
     &                 + ETA*ZR(JDU1+I-1))**2    
 25   CONTINUE
C   

      F   = SCA / SQRT(NODUP)
      F   = -F
C
      CALL JEDEMA()
      END
