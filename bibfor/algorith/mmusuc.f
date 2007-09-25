      SUBROUTINE MMUSUC(NOMA  ,DEFICO,RESOCO)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/09/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8  NOMA
      CHARACTER*24 RESOCO,DEFICO          
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - USURE)
C
C CREATION DES CARTES D'USURE
C      
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEFICO : SD DE DEFINITION DU CONTACT
C IN  RESOCO : SD DE RESOLUTION DU CONTACT
C
C CONTENU DE LA CARTE
C 1  PROFONDEUR D'USURE
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
      INTEGER      IFM,NIV
      INTEGER      CFMMVD,ZMAES      
      CHARACTER*24 MAESCL
      INTEGER      JMAESC
      INTEGER      NTMA,NTPC
      CHARACTER*24 K24BLA,K24BID
      CHARACTER*19 USUMOI,USUPLU,USUFIX,USUINI
      INTEGER      JNCMPM,JNCMPP,JNCMPI,JNCMPX
      INTEGER      JVALVM,JVALVP,JVALVI,JVALVX
      INTEGER      ICMP,IMA,INI
      CHARACTER*2  CH2
      CHARACTER*8  K8BID
      CHARACTER*19 LIGRCF
      INTEGER      NCMPU,NBN
      PARAMETER    (NCMPU=1)
      REAL*8       R8BID
      LOGICAL      LUSURE
      INTEGER      IBID
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C --- USURE ?      
C      
      CALL MMINFP(0     ,DEFICO,K24BLA,'USURE',
     &            IBID  ,R8BID ,K24BID,LUSURE) 
      IF (.NOT. LUSURE) THEN
        GOTO 999
      END IF
C
C --- NBRE COMPOSANTES CARTE USURE
C      
      IF (NCMPU.NE.1) THEN
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- AFFICHAGE
C      
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> CREATION DE LA CARTE DES'//
     &        ' PROFILS USURE' 
      ENDIF 
C      
C --- LIGREL DES ELEMENTS TARDIFS DE CONTACT/FROTTEMENT    
C
      LIGRCF = RESOCO(1:14)//'.LIGR'
C      
C --- RECUPERATION DE QUELQUES DONNEES      
C
      MAESCL = DEFICO(1:16)//'.MAESCL'
      CALL JEVEUO(MAESCL,'L',JMAESC)
C
C --- INITIALISATIONS
C
      NTMA   = ZI(JMAESC)
      NTPC   = 0
      K24BLA = ' ' 
      ZMAES  = CFMMVD('ZMAES')          
C      
C --- NOM DES CARTES D'USURE      
C
      USUMOI = RESOCO(1:14)//'.USUM'
      USUPLU = RESOCO(1:14)//'.USUP'
      USUFIX = RESOCO(1:14)//'.USUF'
      USUINI = RESOCO(1:14)//'.USUI' 
C
C --- CREATION ET INITIALISATION DES CARTES D'USURE :
C      
      CALL ALCART('V',USUMOI,NOMA,'NEUT_R')
      CALL ALCART('V',USUPLU,NOMA,'NEUT_R')
      CALL ALCART('V',USUINI,NOMA,'NEUT_R')
      CALL ALCART('V',USUFIX,NOMA,'NEUT_R')
C
C --- ACCES CARTES
C
      CALL JEVEUO(USUMOI(1:19)//'.NCMP','E',JNCMPM)
      CALL JEVEUO(USUMOI(1:19)//'.VALV','E',JVALVM)
      CALL JEVEUO(USUPLU(1:19)//'.NCMP','E',JNCMPP)
      CALL JEVEUO(USUPLU(1:19)//'.VALV','E',JVALVP)
      CALL JEVEUO(USUINI(1:19)//'.NCMP','E',JNCMPI)
      CALL JEVEUO(USUINI(1:19)//'.VALV','E',JVALVI)
      CALL JEVEUO(USUFIX(1:19)//'.NCMP','E',JNCMPX)
      CALL JEVEUO(USUFIX(1:19)//'.VALV','E',JVALVX)
C
C --- COMPOSANTES
C
      DO 101 ICMP = 1,NCMPU       
        CALL CODENT(ICMP,'G',CH2)
        ZK8(JNCMPM-1+ICMP) = 'X'//CH2
        ZK8(JNCMPP-1+ICMP) = 'X'//CH2
        ZK8(JNCMPI-1+ICMP) = 'X'//CH2
        ZK8(JNCMPX-1+ICMP) = 'X'//CH2  
  101 CONTINUE
C
C --- VALEURS
C 
      DO 21 IMA = 1,NTMA       
        NBN   = ZI(JMAESC+ZMAES*(IMA-1)+3)        
        DO 11 INI = 1,NBN
          ZR(JVALVM-1+1) = 0.D0         
          CALL NOCART(USUMOI,-3         ,K8BID ,'NUM',1,
     &                K8BID ,-(NTPC+INI),LIGRCF,NCMPU)
          ZR(JVALVP-1+1) = 0.D0
          CALL NOCART(USUPLU,-3         ,K8BID ,'NUM',1,
     &                K8BID ,-(NTPC+INI),LIGRCF,NCMPU)
          ZR(JVALVI-1+1) = 0.D0
          CALL NOCART(USUINI,-3         ,K8BID ,'NUM',1,
     &                K8BID ,-(NTPC+INI),LIGRCF,NCMPU)
          ZR(JVALVX-1+1) = 0.D0
          CALL NOCART(USUFIX,-3         ,K8BID ,'NUM',1,
     &                K8BID ,-(NTPC+INI),LIGRCF,NCMPU)
 11     CONTINUE        
        NTPC = NTPC + NBN
 21   CONTINUE 
C
  999 CONTINUE      
C
      CALL JEDEMA()
      END
