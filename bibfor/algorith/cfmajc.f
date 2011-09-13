      SUBROUTINE CFMAJC(RESOCO,NEQ   ,NBLIAC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/09/2011   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT     NONE
      CHARACTER*24 RESOCO
      INTEGER      NEQ
      INTEGER      NBLIAC
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
C
C MISE A JOUR DU VECTEUR SOLUTION ITERATION DE CONTACT
C
C ----------------------------------------------------------------------
C
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  NEQ    : NOMBRE D'EQUATIONS
C IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      CHARACTER*32       JEXNUM 
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
      INTEGER      ILIAI,ILIAC
      CHARACTER*19 MU,CM1A,LIAC 
      INTEGER      JMU,JCM1A,JLIAC
      CHARACTER*19 DDELT
      INTEGER      JDDELT
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES STRUCTURES DE DONNEES DE CONTACT
C
      CM1A     = RESOCO(1:14)//'.CM1A'
      MU       = RESOCO(1:14)//'.MU'
      LIAC     = RESOCO(1:14)//'.LIAC'
      CALL JEVEUO(MU    ,'L',JMU   ) 
      CALL JEVEUO(LIAC  ,'L',JLIAC )
C
C --- ACCES AUX CHAMPS DE TRAVAIL
C
      DDELT  = RESOCO(1:14)//'.DDEL'
      CALL JEVEUO(DDELT(1:19)//'.VALE'  ,'E',JDDELT)
C 
C --- AJOUT DES LIAISONS DE CONTACT ACTIVES
C 
      DO 10 ILIAC = 1,NBLIAC
        ILIAI  = ZI(JLIAC-1+ILIAC)
        CALL JEVEUO(JEXNUM(CM1A,ILIAI),'L',JCM1A)
        CALL DAXPY (NEQ,-ZR(JMU-1+ILIAC),ZR(JCM1A),1,ZR(JDDELT),1)
        CALL JELIBE(JEXNUM(CM1A,ILIAI))
 10   CONTINUE 
C 
      CALL JEDEMA() 
C
      END 
