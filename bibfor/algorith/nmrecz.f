      SUBROUTINE NMRECZ(NUMEDD,CNDIRI,CNFINT,CNFEXT,DDEPLA,
     &                  FONC  )
C    
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILLT BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE MABBAS M.ABBAS
C
      IMPLICIT NONE  
      REAL*8       FONC
      CHARACTER*24 NUMEDD
      CHARACTER*19 CNDIRI,CNFINT,CNFEXT,DDEPLA
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (RECHERCHE LINEAIRE)
C
C CALCUL DE LA FONCTION POUR LA RECHERCHE LINEAIRE
C
C ----------------------------------------------------------------------
C
C
C IN  NUMEDD : NOM DU NUME_DDL
C IN  CNDIRI : VECT_ASSE REACTIONS D'APPUI
C IN  CNFINT : VECT_ASSE FORCES INTERIEURES
C IN  CNFEXT : VECT_ASSE FORCES EXTERIEURES
C IN  DDEPLA : INCREMENT DE DEPLACEMENT
C OUT FONC   : VALEUR DE LA FONCTION    
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
      INTEGER      IEQ,NEQ,IRET
      INTEGER      JFEXT,JFINT,JDIRI,JDDEPL
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      CALL DISMOI('F','NB_EQUA',NUMEDD,'NUME_DDL',NEQ,K8BID,IRET)      
C
C --- ACCES OBJETS 
C
      CALL JEVEUO(CNFEXT(1:19)//'.VALE','L',JFEXT)
      CALL JEVEUO(CNFINT(1:19)//'.VALE','L',JFINT)
      CALL JEVEUO(CNDIRI(1:19)//'.VALE','L',JDIRI)
      CALL JEVEUO(DDEPLA(1:19)//'.VALE','L',JDDEPL) 
C
      FONC = 0.D0
      DO 10 IEQ = 1, NEQ  
        FONC = FONC +
     &         ZR(JDDEPL+IEQ-1) * (ZR(JFINT+IEQ-1)+
     &                             ZR(JDIRI+IEQ-1)-
     &                             ZR(JFEXT+IEQ-1))    
  10  CONTINUE
  
      CALL JEDEMA()  
      END
