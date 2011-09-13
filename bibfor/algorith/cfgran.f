      SUBROUTINE CFGRAN(RESOCO,NBLIAC,KKLIAI,KKLIAC)
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
      IMPLICIT NONE
      CHARACTER*24 RESOCO
      INTEGER      NBLIAC,KKLIAI,KKLIAC
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
C
C VERIFICATION SI L'ENSEMBLE DES LIAISONS SUPPOSEES TROP GRAND
C
C ----------------------------------------------------------------------
C
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
C OUT KKLIAI : NUMERO DE LA LIAISON AYANT LE MU LE PLUS NEGATIF
C              0 SI TOUS LES MU SONT POSITIF (ON A CONVERGE)
C OUT KKLIAC : NUMERO DE LA LIAISON _ACTIVE_ AYANT LE MU LE PLUS NEGATIF
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
      REAL*8        RMINMU,R8MAEM,VALMU
      CHARACTER*19  LIAC,MU
      INTEGER       JLIAC,JMU
      INTEGER       ILIAC,INDEX
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      KKLIAI = 0
      KKLIAC = 0
      INDEX  = 0
      RMINMU = R8MAEM()
C
C --- ACCES STRUCTURES DE DONNEES DE CONTACT
C          
      MU     = RESOCO(1:14)//'.MU'
      LIAC   = RESOCO(1:14)//'.LIAC'
      CALL JEVEUO(MU,    'L',JMU   )
      CALL JEVEUO(LIAC,  'L',JLIAC )
C
C --- MU MINIMUM
C
      DO 10 ILIAC = 1,NBLIAC
        VALMU  = ZR(JMU+ILIAC-1)
        IF (RMINMU.GT.VALMU) THEN
          RMINMU = VALMU
          INDEX  = ILIAC
        ENDIF
  10  CONTINUE
C
C --- ON REPERE LA LIAISON KKMIN AYANT LE MU LE PLUS NEGATIF
C 
      KKLIAC = INDEX
      IF (KKLIAC.NE.0) THEN
        KKLIAI = ZI(JLIAC-1+KKLIAC)
      ENDIF
C
C --- SI TOUS LES MU SONT > 0 -> ON A CONVERGE (IL Y A CONTACT)
C
      IF (RMINMU.GE.0.D0) THEN
        KKLIAI = 0
        KKLIAC = 0
      ENDIF
C 
      CALL JEDEMA() 
C
      END 
