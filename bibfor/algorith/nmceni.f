      SUBROUTINE NMCENI(NUMEDD,DEPDEL,DEPPR1,DEPPR2,RHO   ,
     &                  SDPILO,ETA,ISXFE,F)
C 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/02/2011   AUTEUR MASSIN P.MASSIN 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*24  NUMEDD
      CHARACTER*19  SDPILO,DEPDEL,DEPPR1,DEPPR2
      CHARACTER*19  DDEPL0,DDEPL1
      REAL*8        ETA,RHO,F
      LOGICAL       ISXFE
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE - SELECTION PARAMETRE)
C
C CALCUL DU PARAMETRE DE SELECTION DE TYPE NORM_INCR_DEPL
C      
C ----------------------------------------------------------------------
C
C
C IN  NUMEDD : NUME_DDL
C IN  SDPILO : SD PILOTAGE
C IN  DEPDEL : INCREMENT DE DEPLACEMENT DEPUIS DEBUT PAS DE TEMPS
C IN  DEPPR1 : INCREMENT DE DEPLACEMENT K-1.F_DONNE
C IN  DEPPR2 : INCREMENT DE DEPLACEMENT K-1.F_PILO
C IN  RHO    : PARAMETRE DE RECHERCHE LINEAIRE
C IN  ETA    : PARAMETRE DE PILOTAGE
C IN  ISXFE  : INDIQUE S'IL S'AGIT D'UN MODELE XFEM
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
      CHARACTER*8  K8BID
      INTEGER      JDEPDE,JDU0,JDU1,IDEEQ,JCOEE,JCOEF
      INTEGER      JDEP0,JDEP1
      CHARACTER*19 PROFCH,CHAPIL,CHAPIC      
      INTEGER      NEQ,IRET,I,J,IBID
      REAL*8       DN, DC, DP
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      IF(ISXFE) THEN
         CHAPIL = SDPILO(1:14)//'.PLCR'
         CALL JEVEUO(CHAPIL(1:19)//'.VALE','L',JCOEF)
         CHAPIC = SDPILO(1:14)//'.PLCI'
         CALL JEVEUO(CHAPIC(1:19)//'.VALE','L',JCOEE)
      ENDIF
C
C --- INITIALISATIONS
C
      F      = 0.D0
C
C --- INFORMATIONS SUR NUMEROTATION
C      
      CALL DISMOI('F','NB_EQUA',NUMEDD,'NUME_DDL',NEQ,K8BID ,IRET)
      CALL DISMOI('F','PROF_CHNO',DEPDEL,'CHAM_NO',IBID,PROFCH,IRET)
      CALL JEVEUO(PROFCH(1:19)//'.DEEQ'   ,'L',IDEEQ)      
C
C --- ACCES AUX VECTEURS SOLUTIONS
C      
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(DEPPR1(1:19)//'.VALE','L',JDU0 )
      CALL JEVEUO(DEPPR2(1:19)//'.VALE','L',JDU1 )
  
C
C --- CALCUL DE LA NORME
C
      IF(ISXFE) THEN
      DO 20 I = 1, NEQ
         IF(ZI(IDEEQ-1 + 2*I ).GT.0) THEN
         IF(ZR(JCOEE+I-1).EQ.0.D0) THEN
              F = F + ZR(JCOEF+I-1)**2*
     &        (ZR(JDEPDE-1+I)+RHO*ZR(JDU0-1+I)+
     &        ETA*ZR(JDU1-1+I))**2
            ELSE
           DN = 0.D0
              DC = 0.D0
              DP = 0.D0     
              DO 31 J = I+1, NEQ
              IF(ZR(JCOEE+I-1).EQ.ZR(JCOEE+J-1)) THEN    
                   DN = DN + ZR(JCOEF+I-1)*ZR(JDEPDE+I-1)+   
     &                 ZR(JCOEF+J-1)*ZR(JDEPDE+J-1)
                   DC = DC + ZR(JCOEF+I-1)*ZR(JDU0-1+I)+
     &               ZR(JCOEF+J-1)*ZR(JDU0-1+J)
                   DP = DP + ZR(JCOEF+I-1)*ZR(JDU1-1+I)+
     &               ZR(JCOEF+J-1)*ZR(JDU1-1+J)   
              ENDIF
 31          CONTINUE
             F = F + (DN+RHO*DC+ETA*DP)**2
           ENDIF
         ENDIF
 20   CONTINUE
      ELSE
        DO 30 I = 1, NEQ
          IF (ZI(IDEEQ-1 + 2*I + 2).GT.0) THEN
             F = F + 
     &  (ZR(JDEPDE+I)+RHO*ZR(JDU0+I)+ETA*ZR(JDU1+I))**2
          ENDIF
 30     CONTINUE
      ENDIF
      CALL JEDEMA()
      END
