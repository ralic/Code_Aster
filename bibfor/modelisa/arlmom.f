      SUBROUTINE ARLMOM(MAILAR,MODARL)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*8  MAILAR,MODARL          
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C CREATION DU PSEUDO-MODELE  
C
C ----------------------------------------------------------------------
C
C
C IN  MAILAR : NOM DU PSEUDO-MAILLAGE
C IN  MODARL : NOM DU PSEUDO-MODELE
C
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXATR
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      IFM,NIV
      CHARACTER*24 MODMAI
      INTEGER      JMOMA
      INTEGER      JDIME
      INTEGER      NBMA
      CHARACTER*19 LIGRMO      
      INTEGER      IGREL,IEL,IMA,NUTE,NBELGR 
      INTEGER      IALIEL,ILLIEL,IAUX1
C
      INTEGER      NBGREL,TYPELE,NBELEM
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('ARLEQUIN',IFM,NIV)
C
C --- AFFICHAGE
C      
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN> *** CREATION DU '//
     &                'PSEUDO-MODELE...'   
      ENDIF
C
C --- INFO SUR LE MAILLAGE
C     
      CALL JEVEUO(MAILAR(1:8)//'.DIME','L',JDIME)
      NBMA = ZI(JDIME - 1 + 3)       
C
C --- CREATION DES SDs DE BASE DE MODELE
C           
      MODMAI = MODARL(1:8)//'.MAILLE    '        
      CALL WKVECT(MODMAI,'V V I',NBMA,JMOMA)         
C
C --- ACCES AU LIGREL
C
      LIGRMO = MODARL(1:8)//'.MODELE'       
      CALL JEVEUO(LIGRMO//'.LIEL','L',IALIEL)
      CALL JEVEUO(JEXATR(LIGRMO//'.LIEL','LONCUM'),'L',ILLIEL)
C      
C --- REMPLISSAGE DE LA SD MODELE//'.MAILLE'
C     
      DO 10 IGREL = 1,NBGREL(LIGRMO)
        NUTE   = TYPELE(LIGRMO,IGREL)
        NBELGR = NBELEM(LIGRMO,IGREL)
        IAUX1  = IALIEL-1+ZI(ILLIEL-1+IGREL)-1
        DO 20 IEL = 1,NBELGR
          IMA    =  ZI(IAUX1+IEL)
          IF (IMA.GT.NBMA) THEN
            CALL ASSERT(.FALSE.)
          ELSE
            ZI(JMOMA+IMA-1) = NUTE  
          ENDIF    
   20   CONTINUE           
   10 CONTINUE            
C
C --- AFFICHAGE
C 
      IF (NIV.GE.2) THEN
C        CALL UTIMSD(IFM,2,.TRUE.,.TRUE.,MODARL,1,'V')
        WRITE(IFM,*) '<ARLEQUIN> *** FIN DE CREATION DU '//
     &                'PSEUDO-MODELE...'   
      ENDIF               
C
      CALL JEDEMA()

      END
