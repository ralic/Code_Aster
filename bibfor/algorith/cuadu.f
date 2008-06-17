      SUBROUTINE CUADU(DEFICU,RESOCU,NEQ,NBLIAC)
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
       INTEGER      NEQ
       INTEGER      NBLIAC
       CHARACTER*24 DEFICU       
       CHARACTER*24 RESOCU
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : ALGOCU
C ----------------------------------------------------------------------
C
C  ROUTINE MERE POUR LE CALCUL DU SECOND MEMBRE
C
C IN  DEFICU : SD DE DEFINITION (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCU : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C                'E': RESOCU(1:14)//'.MU'
C IN  NEQ    : NOMBRE D'EQUATIONS
C IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
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
      INTEGER       ILIAC,NBDDL 
      INTEGER       LLIAC,JDECAL
      REAL*8        VAL
      CHARACTER*19  LIAC,MU,DELT0
      INTEGER       JLIAC,JMU,JDELT0
      CHARACTER*24  APDDL,APCOEF,APJEU,POINOE
      INTEGER       JAPDDL,JAPCOE,JAPJEU,JPOI
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
      APDDL  = RESOCU(1:14)//'.APDDL'
      LIAC   = RESOCU(1:14)//'.LIAC'
      APCOEF = RESOCU(1:14)//'.APCOEF'
      APJEU  = RESOCU(1:14)//'.APJEU'
      MU     = RESOCU(1:14)//'.MU'
      DELT0  = RESOCU(1:14)//'.DEL0'
      POINOE = DEFICU(1:16)//'.POINOE'      
C ======================================================================
      CALL JEVEUO(APDDL, 'L',JAPDDL)
      CALL JEVEUO(LIAC,  'L',JLIAC )
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APJEU, 'L',JAPJEU)
      CALL JEVEUO(DELT0, 'L',JDELT0)
      CALL JEVEUO(MU,    'E',JMU   )
      CALL JEVEUO(POINOE,'L',JPOI  )         
C ======================================================================
      DO 10 ILIAC = 1, NBLIAC
         LLIAC  = ZI(JLIAC-1+ILIAC)       
         JDECAL = ZI(JPOI+LLIAC-1)
         NBDDL  = ZI(JPOI+LLIAC) - ZI(JPOI+LLIAC-1)               
         CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &               ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL)
         ZR(JMU+ILIAC-1) = ZR(JAPJEU+LLIAC-1) - VAL
 10    CONTINUE
C ======================================================================
       CALL JEDEMA ()
C ======================================================================
       END
