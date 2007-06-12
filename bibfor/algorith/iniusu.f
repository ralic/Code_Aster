      SUBROUTINE INIUSU(PREMIE,NOMA,DEFICO,LIGRCF,
     &                  CRUMOI,CRUPLU,CRUFIX,CRUINI)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2007   AUTEUR TORKHANI M.TORKHANI 
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
C TOLE CRP_20

      IMPLICIT NONE
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO       
      CHARACTER*19 LIGRCF 
      CHARACTER*19 CRUMOI,CRUPLU,CRUFIX,CRUINI
      LOGICAL      PREMIE     
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : NMDEPL
C ----------------------------------------------------------------------
C
C CREATION DE LA CARTE DES PROFILS D'USURE
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEFICO : SD DE DEFINITION DU CONTACT
C IN  LIGRCF : LIGRCF POUR ELEMENTS TARDIFS DE CONTACT
C IN  PREMIE : VAUT .TRUE. SI PREMIERE ITERATION 
C              DE NEWTON
C OUT CRUMOI : CARTE D'USURE A L'INSTANT MOINS
C OUT CRUPLU : CARTE D'USURE A L'INSTANT PLUS
C OUT CRUFIX : CARTE D'USURE COURANTE
C OUT CRUINI : COPIE DE LA CARTE D'USURE COURANTE
C
C CONTENU DE LA CARTE
C
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
      INTEGER      CFMMVD,ZMAES
      INTEGER      IFM,NIV,IZONE,IBID  
      INTEGER      JNCMPM,JNCMPP,JVALVM,JVALVP
      INTEGER      K,INO,GD,NTPC,NBNOC
      INTEGER      NCMPU,NCMPMX,JVALEM,JVALEP,JVALEI,JNCMPI,JVALVI
      INTEGER      JNCMPX,JVALVX,JVALEX,JMAESC,IMA,NTMA,NBN,INI
      REAL*8       R8BID
      CHARACTER*2  CH2
      CHARACTER*8  KBID,K1BID,NOMGD
      CHARACTER*24 K24BID,K24BLA
      LOGICAL      LUSURE
      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
 
C
C --- AFFICHAGE
C      
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> CREATION ET INITIALISATION'//
     &        ' DE LA CARTE DES PROFILS USURE' 
      ENDIF 
C
C --- ACCES OBJETS
C
      CALL JEVEUO(DEFICO(1:16)//'.MAESCL','L',JMAESC) 
      ZMAES = CFMMVD('ZMAES')
C
C      CREATION ET INITIALISATION DES CARTES D'USURE :
C      -------------------------

      NOMGD = 'NEUT_R'
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',NOMGD),GD)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K1BID)
      
      NCMPU = 1
      NTMA = ZI(JMAESC)
      NTPC = 0
            
      IF (PREMIE) THEN
        
        CALL ALCART('V',CRUMOI,NOMA,'NEUT_R')
        CALL ALCART('V',CRUPLU,NOMA,'NEUT_R')
        CALL ALCART('V',CRUINI,NOMA,'NEUT_R')
        CALL ALCART('V',CRUFIX,NOMA,'NEUT_R')
        
        CALL JEVEUO(CRUMOI//'.NCMP','E',JNCMPM)
        CALL JEVEUO(CRUMOI//'.VALV','E',JVALVM)
        CALL JEVEUO(CRUPLU//'.NCMP','E',JNCMPP)
        CALL JEVEUO(CRUPLU//'.VALV','E',JVALVP)
        CALL JEVEUO(CRUINI//'.NCMP','E',JNCMPI)
        CALL JEVEUO(CRUINI//'.VALV','E',JVALVI)
        CALL JEVEUO(CRUFIX//'.NCMP','E',JNCMPX)
        CALL JEVEUO(CRUFIX//'.VALV','E',JVALVX)
        
        DO 101,K = 1,NCMPU
          CALL CODENT(K,'G',CH2)
          ZK8(JNCMPM-1+K) = 'X'//CH2
          ZK8(JNCMPP-1+K) = 'X'//CH2
          ZK8(JNCMPI-1+K) = 'X'//CH2
          ZK8(JNCMPX-1+K) = 'X'//CH2
  101   CONTINUE
        
        DO 21 IMA = 1,NTMA
          NBN   = ZI(JMAESC+ZMAES*(IMA-1)+3)
          IZONE = ZI(JMAESC+ZMAES*(IMA-1)+2)
          CALL MMINFP(IZONE,DEFICO,K24BLA,'USURE',
     &                IBID,R8BID,K24BID,LUSURE) 
          IF (.NOT. LUSURE) THEN
            GOTO 99
          END IF
          DO 11 INI = 1,NBN
            ZR(JVALVM-1+1) = 0.D0         
            CALL NOCART(CRUMOI,-3,KBID,'NUM',1,KBID,
     &                  -(NTPC+INI),LIGRCF,NCMPU)
            ZR(JVALVP-1+1) = 0.D0
            CALL NOCART(CRUPLU,-3,KBID,'NUM',1,KBID,
     &                  -(NTPC+INI),LIGRCF,NCMPU)
            ZR(JVALVI-1+1) = 0.D0
            CALL NOCART(CRUINI,-3,KBID,'NUM',1,KBID,
     &                  -(NTPC+INI),LIGRCF,NCMPU)
            ZR(JVALVX-1+1) = 0.D0
            CALL NOCART(CRUFIX,-3,KBID,'NUM',1,KBID,
     &                  -(NTPC+INI),LIGRCF,NCMPU)
 11       CONTINUE
          NTPC = NTPC + NBN
 21     CONTINUE
      
      END IF
      
      CALL JEVEUO(CRUMOI//'.VALE','L',JVALEM)
      CALL JEVEUO(CRUPLU//'.VALE','L',JVALEP)
      CALL JEVEUO(CRUINI//'.VALE','L',JVALEI)
      CALL JEVEUO(CRUFIX//'.VALE','L',JVALEX)
      
      NTPC = 0
      DO 61 IMA = 1,NTMA
        NBNOC = ZI(JMAESC+ZMAES*(IMA-1)+3)
        IZONE = ZI(JMAESC+ZMAES*(IMA-1)+2)
        CALL MMINFP(IZONE,DEFICO,K24BLA,'USURE',
     &              IBID,R8BID,K24BID,LUSURE) 
        IF (.NOT. LUSURE) THEN
          GOTO 99
        END IF
        DO 51 INO = 1,NBNOC
          IF (PREMIE) THEN
            ZR(JVALEM+(NTPC+INO-1)) = 0.D0
            ZR(JVALEP+(NTPC+INO-1)) = 0.D0
            ZR(JVALEI+(NTPC+INO-1)) = 0.D0
            ZR(JVALEX+(NTPC+INO-1)) = 0.D0
          ELSE
            ZR(JVALEM+(NTPC+INO-1))=ZR(JVALEX+(NTPC+INO-1))
          END IF
          JVALEM = JVALEM + (NCMPMX - NCMPU)
          JVALEP = JVALEP + (NCMPMX - NCMPU)
          JVALEI = JVALEI + (NCMPMX - NCMPU) 
          JVALEX = JVALEX + (NCMPMX - NCMPU)
 51     CONTINUE
        NTPC = NTPC + NBNOC
 61   CONTINUE
      
 99   CONTINUE
C --- MENAGE 
C
      CALL JEDETR(CRUMOI//'.NCMP')
      CALL JEDETR(CRUMOI//'.VALV')
      CALL JEDETR(CRUPLU//'.NCMP')
      CALL JEDETR(CRUPLU//'.VALV')
      CALL JEDETR(CRUFIX//'.NCMP')
      CALL JEDETR(CRUFIX//'.VALV')
C
      CALL JEDEMA()      
      END
