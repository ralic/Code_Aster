      SUBROUTINE MAJUSU(PREMIE,NOMA,DEFICO,LIGRCF,
     &                  CRUMOI,CRUPLU,CRUFIX,CRUINI,
     &                  DEPMOI,DEPDEL)
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
      CHARACTER*24 DEPMOI,DEPDEL 
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
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C IN  DEPMOI : DEPLACEMENT INITIAL
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
      INTEGER      CFMMVD,ZTABF,ZCMCF,ZMAES
      INTEGER      IFM,NIV,IBID     
      INTEGER      JNCMPM,JNCMPP,JVALVM,JVALVP,IZONE
      INTEGER      JTYMAI,JTABF,IACNX1,ILCNX1,JCMCF
      
      INTEGER      JDIM,NDIM,NBPC,K,INO,NUNO,GD,JUSU
      INTEGER      NCMPU,NCMPMX,JVALEM,JVALEP,JVALEI,JNCMPI,JVALVI
      INTEGER      JNCMPX,JVALVX,JVALEX,JMAESC,IMA,NTMA,NBN,INI
      INTEGER      JDEPDE,JDEPDL,JDEMDE,JDEMDL,JCNSVR,JCNSLR,NBNOC
      INTEGER      NUMAMA,NUMAES,I,J,NUTYP,IATYMA,NNOM,NNOE,NTPC
      
      REAL*8       KWEAR,HWEAR,VECT1(3),VECT2(3),NORM(3),X(2),FF(9)
      REAL*8       DEPLPE(3),DEPLME(3),DEPLPM(3),DEPLMM(3),LAGSCP
      REAL*8       DEPPT(3),DEPMT(3),DEP(3),DISSIP,C(3,3)
      REAL*8       R8BID
      CHARACTER*2  CH2
      CHARACTER*8  KBID,K1BID,ALIAS,NOMGD
      CHARACTER*8  LICMP4(4),LICMP6(6)
      CHARACTER*19 DEPDES,DEPCN,DEPMOS,DEMCN
      CHARACTER*24 K24BID,K24BLA
      LOGICAL      LUSURE
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C --- AFFICHAGE
C      
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> CREATION DE LA CARTE DES'//
     &        ' PROFILS USURE' 
      ENDIF  
C
C --- ACCES OBJETS
C
      CALL JEVEUO(DEFICO(1:16)//'.NDIMCO','L',JDIM)
      CALL JEVEUO(DEFICO(1:16)//'.TABFIN','L',JTABF)
      CALL JEVEUO(DEFICO(1:16)//'.CARACF','L',JCMCF)
      CALL JEVEUO(DEFICO(1:16)//'.MAESCL','L',JMAESC) 
      CALL JEVEUO(DEFICO(1:16)//'.JEUSUR','L',JUSU) 
      CALL JEVEUO(NOMA//'.CONNEX','L',IACNX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',ILCNX1)   
      CALL JEVEUO(NOMA//'.TYPMAIL','L',JTYMAI)
C
      ZTABF = CFMMVD('ZTABF')
      ZCMCF = CFMMVD('ZCMCF')
      ZMAES = CFMMVD('ZMAES')
      
C
C      CREATION ET INITIALISATION DES CARTES D'USURE :
C      -------------------------

      NOMGD = 'NEUT_R'
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',NOMGD),GD)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K1BID)
      
      NCMPU = 1
            
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
      
      END IF
      
      IF (PREMIE) THEN
        
        DO 101,K = 1,NCMPU
          
          CALL CODENT(K,'G',CH2)
          ZK8(JNCMPM-1+K) = 'X'//CH2
          ZK8(JNCMPP-1+K) = 'X'//CH2
          ZK8(JNCMPI-1+K) = 'X'//CH2
          ZK8(JNCMPX-1+K) = 'X'//CH2
          
  101   CONTINUE
      
      END IF
      
      NTMA = ZI(JMAESC)
      NTPC = 0
      NDIM = ZI(JDIM)
      
      IF (PREMIE) THEN
        
      DO 21 IMA = 1,NTMA
          
      NBN   = ZI(JMAESC+ZMAES*(IMA-1)+3)
      IZONE = ZI(JMAESC+ZMAES*(IMA-1)+2)
      CALL MMINFP(IZONE,DEFICO,K24BLA,'USURE',
     &            IBID,R8BID,K24BID,LUSURE) 
      IF (.NOT. LUSURE) THEN
        GOTO 99
      END IF         
      DO 11 INI = 1,NBN
        
        ZR(JVALVM-1+1) = 0.D0         
        CALL NOCART(CRUMOI,-3,KBID,'NUM',1,KBID,-(NTPC+INI),
     &              LIGRCF,NCMPU)
        ZR(JVALVP-1+1) = 0.D0
        CALL NOCART(CRUPLU,-3,KBID,'NUM',1,KBID,-(NTPC+INI),
     &              LIGRCF,NCMPU)
        ZR(JVALVI-1+1) = 0.D0
        CALL NOCART(CRUINI,-3,KBID,'NUM',1,KBID,-(NTPC+INI),
     &              LIGRCF,NCMPU)
        ZR(JVALVX-1+1) = 0.D0
        CALL NOCART(CRUFIX,-3,KBID,'NUM',1,KBID,-(NTPC+INI),
     &              LIGRCF,NCMPU)
 11   CONTINUE
        
      NTPC = NTPC + NBN

 21   CONTINUE
      END IF
      
      CALL JEVEUO(CRUMOI//'.VALE','L',JVALEM)
      CALL JEVEUO(CRUPLU//'.VALE','L',JVALEP)
      CALL JEVEUO(CRUINI//'.VALE','L',JVALEI)
      CALL JEVEUO(CRUFIX//'.VALE','L',JVALEX)
      
      DEPDES = '&&USCART.DEPPINR'
      DEPMOS = '&&USCART.DEPMINR'
      DEPCN  = '&&USCART.CNSPINR'
      DEMCN  = '&&USCART.CNSMINR'
      
      CALL CNOCNS(DEPDEL,'V',DEPDES)
      CALL CNOCNS(DEPMOI,'V',DEPMOS)
      
      LICMP6(1) = 'DX'
      LICMP6(2) = 'DY'
      LICMP6(3) = 'DZ'
      LICMP6(4) = 'LAGS_C'
      LICMP6(5) = 'LAGS_F1'
      LICMP6(6) = 'LAGS_F2'

      LICMP4(1) = 'DX'
      LICMP4(2) = 'DY'
      LICMP4(3) = 'LAGS_C'
      LICMP4(4) = 'LAGS_F1'

      IF (NDIM.EQ.3) THEN
        CALL CNSRED(DEPDES,0,0,6,LICMP6,'V',DEPCN)
        CALL CNSRED(DEPMOS,0,0,6,LICMP6,'V',DEMCN)
      ELSE IF (NDIM.EQ.2) THEN
        CALL CNSRED(DEPDES,0,0,4,LICMP4,'V',DEPCN)
        CALL CNSRED(DEPMOS,0,0,4,LICMP4,'V',DEMCN)
      END IF
      
      CALL JEVEUO(DEPCN//'.CNSV','L',JDEPDE)
      CALL JEVEUO(DEPCN//'.CNSL','L',JDEPDL)
      CALL JEVEUO(DEMCN//'.CNSV','L',JDEMDE)
      CALL JEVEUO(DEMCN//'.CNSL','L',JDEMDL)
      
      NTPC = 0
      
      IF (NDIM .EQ. 3) THEN
        
      DO 61 IMA = 1,NTMA
          
        NBNOC = ZI(JMAESC+ZMAES* (IMA-1)+3)
        IZONE = ZI(JMAESC+ZMAES* (IMA-1)+2)
        CALL MMINFP(IZONE,DEFICO,K24BLA,'USURE',
     &              IBID,R8BID,K24BID,LUSURE) 
        IF (.NOT. LUSURE) THEN
          GOTO 99
        END IF
        DO 51 INO = 1,NBNOC
           
        KWEAR=ZR(JCMCF+ZCMCF*(IZONE-1)+14)
        HWEAR=ZR(JCMCF+ZCMCF*(IZONE-1)+15)
        NUMAES=NINT(ZR(JTABF+ZTABF*(NTPC+INO-1)+1))
        NUMAMA=NINT(ZR(JTABF+ZTABF*(NTPC+INO-1)+2))
        VECT1(1)=ZR(JTABF+ZTABF*(NTPC+INO-1)+6)
        VECT1(2)=ZR(JTABF+ZTABF*(NTPC+INO-1)+7)
        VECT1(3)=ZR(JTABF+ZTABF*(NTPC+INO-1)+8)
        VECT2(1)=ZR(JTABF+ZTABF*(NTPC+INO-1)+9)
        VECT2(2)=ZR(JTABF+ZTABF*(NTPC+INO-1)+10)
        VECT2(3)=ZR(JTABF+ZTABF*(NTPC+INO-1)+11)
        CALL PROVEC(VECT2,VECT1,NORM)
            
        DO 321 I = 1,NDIM
          DO 311 J = 1,NDIM
            C(I,J) = -1.D0*NORM(I)*NORM(J)
  311     CONTINUE
  321   CONTINUE
            
        DO 331 I = 1,NDIM
          C(I,I) = 1 + C(I,I)
  331   CONTINUE
          
        NUTYP = ZI(JTYMAI-1+NUMAES)
        IF (NUTYP.EQ.2) THEN
          ALIAS = 'SG2'
          NNOE = 2
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
        ELSE IF (NUTYP.EQ.4) THEN
          ALIAS = 'SG3'
          NNOE = 3
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
        ELSE IF (NUTYP.EQ.7) THEN
          ALIAS = 'TR3'
          NNOE = 3
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
          X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
        ELSE IF (NUTYP.EQ.9) THEN
          ALIAS = 'TR6'
          NNOE = 6
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
          X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
        ELSE IF (NUTYP.EQ.12) THEN
          ALIAS = 'QU4'
          NNOE = 4
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
          X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
        ELSE IF (NUTYP.EQ.14) THEN
          ALIAS = 'QU8'
          NNOE = 8
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
          X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
        ELSE IF (NUTYP.EQ.16) THEN
          ALIAS = 'QU9'
          NNOE = 9
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
          X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
        ELSE
          CALL U2MESS('F','ELEMENTS_16')
        END IF
         
        CALL CALFFX(ALIAS,X(1),X(2),FF)
            
        DEPLPE(1) = 0.D0
        DEPLPE(2) = 0.D0
        DEPLPE(3) = 0.D0
        DEPLME(1) = 0.D0
        DEPLME(2) = 0.D0
        DEPLME(3) = 0.D0
        LAGSCP    = 0.D0
        
        DO 31 I = 1,NNOE
          NUNO = ZI(IACNX1+ZI(ILCNX1-1+NUMAES)+I-2)
          DEPLPE(1)=DEPLPE(1)+ZR(JDEPDE-1+6*(NUNO-1)+1)*FF(I)
          DEPLPE(2)=DEPLPE(2)+ZR(JDEPDE-1+6*(NUNO-1)+2)*FF(I)
          DEPLPE(3)=DEPLPE(3)+ZR(JDEPDE-1+6*(NUNO-1)+3)*FF(I)
          DEPLME(1)=DEPLME(1)+ZR(JDEMDE-1+6*(NUNO-1)+1)*FF(I)
          DEPLME(2)=DEPLME(2)+ZR(JDEMDE-1+6*(NUNO-1)+2)*FF(I)
          DEPLME(3)=DEPLME(3)+ZR(JDEMDE-1+6*(NUNO-1)+3)*FF(I)
          LAGSCP   =LAGSCP   +ZR(JDEPDE-1+6*(NUNO-1)+4)*FF(I)
   31   CONTINUE
            
        NUTYP = ZI(JTYMAI-1+NUMAMA)
           
        IF (NUTYP.EQ.2) THEN
          ALIAS = 'SG2'
          NNOM = 2
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
        ELSE IF (NUTYP.EQ.4) THEN
          ALIAS = 'SG3'
          NNOM = 3
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
        ELSE IF (NUTYP.EQ.7) THEN
          ALIAS = 'TR3'
          NNOM = 3
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
          X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
        ELSE IF (NUTYP.EQ.9) THEN
          ALIAS = 'TR6'
          NNOM = 6
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
          X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
        ELSE IF (NUTYP.EQ.12) THEN
          ALIAS = 'QU4'
          NNOM = 4
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
          X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
        ELSE IF (NUTYP.EQ.14) THEN
          ALIAS = 'QU8'
          NNOM = 8
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
          X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
        ELSE IF (NUTYP.EQ.16) THEN
          ALIAS = 'QU9'
          NNOM = 9
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
          X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
        ELSE
          CALL U2MESS('F','ELEMENTS_16')
        END IF
          
        CALL CALFFX(ALIAS,X(1),X(2),FF)
        DEPLPM(1) = 0.D0
        DEPLPM(2) = 0.D0
        DEPLPM(3) = 0.D0
        DEPLMM(1) = 0.D0
        DEPLMM(2) = 0.D0
        DEPLMM(3) = 0.D0
        DO 41 I = 1,NNOM
          NUNO = ZI(IACNX1+ZI(ILCNX1-1+NUMAMA)+I-2)
          DEPLPM(1)=DEPLPM(1)+ZR(JDEPDE-1+6*(NUNO-1)+1)*FF(I)
          DEPLPM(2)=DEPLPM(2)+ZR(JDEPDE-1+6*(NUNO-1)+2)*FF(I)
          DEPLPM(3)=DEPLPM(3)+ZR(JDEPDE-1+6*(NUNO-1)+3)*FF(I)
          DEPLMM(1)=DEPLMM(1)+ZR(JDEMDE-1+6*(NUNO-1)+1)*FF(I)
          DEPLMM(2)=DEPLMM(2)+ZR(JDEMDE-1+6*(NUNO-1)+2)*FF(I)
          DEPLMM(3)=DEPLMM(3)+ZR(JDEMDE-1+6*(NUNO-1)+3)*FF(I)
  41    CONTINUE
          
        DO 322 I=1,NDIM
          DEPPT(I) = 0.D0
          DEPMT(I) = 0.D0
          DO 312  K=1,NDIM
            DEPPT(I)=C(I,K)*(DEPLPE(K)-DEPLPM(K))+DEPPT(I)
            DEPMT(I)=C(I,K)*(DEPLME(K)-DEPLMM(K))+DEPMT(I)
            DEP(I)  =DEPPT(I) - DEPMT(I)
  312     CONTINUE
  322   CONTINUE
          
        DISSIP = 0.D0
        
        DO 54 K = 1,NDIM
          DISSIP = DISSIP + (DEP(K)*DEP(K))
 54     CONTINUE
        
        DISSIP = SQRT(DISSIP)
        
        IF (PREMIE) THEN
          ZR(JVALEM+(NTPC+INO-1)) = 0.D0
          ZR(JVALEP+(NTPC+INO-1)) = 0.D0
          ZR(JVALEI+(NTPC+INO-1)) = 0.D0
          ZR(JVALEX+(NTPC+INO-1)) = 0.D0
        ELSE
          ZR(JVALEM+(NTPC+INO-1))=ZR(JVALEX+(NTPC+INO-1))
        END IF
         
        ZR(JVALEP+(NTPC+INO-1))= ZR(JVALEM+(NTPC+INO-1))+
     &                     ((KWEAR/HWEAR)*DISSIP*ABS(LAGSCP))
            
        ZR(JVALEX+(NTPC+INO-1))= ZR(JVALEP+(NTPC+INO-1))
        ZR(JVALEI+(NTPC+INO-1))= ZR(JVALEP+(NTPC+INO-1))
        ZR(JUSU  +(NTPC+INO-1))= ZR(JVALEX+(NTPC+INO-1))
        JVALEM = JVALEM + (NCMPMX - NCMPU)
        JVALEP = JVALEP + (NCMPMX - NCMPU)
        JVALEI = JVALEI + (NCMPMX - NCMPU) 
        JVALEX = JVALEX + (NCMPMX - NCMPU)
            
 51   CONTINUE
      NTPC = NTPC + NBNOC
 61   CONTINUE
              
 
      ELSE IF (NDIM.EQ.2) THEN
      
      DO 62 IMA = 1,NTMA
        
      NBNOC = ZI(JMAESC+ZMAES* (IMA-1)+3)
      IZONE = ZI(JMAESC+ZMAES* (IMA-1)+2)
      CALL MMINFP(IZONE,DEFICO,K24BLA,'USURE',
     &            IBID,R8BID,K24BID,LUSURE) 
      IF (.NOT. LUSURE) THEN
        GOTO 99
      END IF
      DO 52 INO = 1,NBNOC
                 
        KWEAR = ZR(JCMCF+ZCMCF* (IZONE-1)+14)
        HWEAR = ZR(JCMCF+ZCMCF* (IZONE-1)+15)
        NUMAES = NINT(ZR(JTABF+ZTABF* (NTPC+INO-1)+1))
        NUMAMA = NINT(ZR(JTABF+ZTABF* (NTPC+INO-1)+2))
        VECT1(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+6)
        VECT1(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+7)
        NORM(1) =-VECT1(2)
        NORM(2) =VECT1(1)
        NORM(3) =0.D0
        
        DO 323 I = 1,NDIM
          DO 313 J = 1,NDIM
            C(I,J) = -1.D0*NORM(I)*NORM(J)
  313     CONTINUE
  323   CONTINUE
          
        DO 333 I = 1,NDIM
          C(I,I) = 1 + C(I,I)
  333   CONTINUE
         
        NUTYP = ZI(JTYMAI-1+NUMAES)
       
        IF (NUTYP.EQ.2) THEN
          ALIAS = 'SG2'
          NNOE = 2
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
        ELSE IF (NUTYP.EQ.4) THEN
          ALIAS = 'SG3'
          NNOE = 3
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
        ELSE
          CALL U2MESS('F','ELEMENTS_16')
        END IF
            
        CALL CALFFX(ALIAS,X(1),X(2),FF)
         
        DEPLPE(1) = 0.D0
        DEPLPE(2) = 0.D0
        DEPLME(1) = 0.D0
        DEPLME(2) = 0.D0
        LAGSCP    = 0.D0
          
        DO 32 I = 1,NNOE
          NUNO = ZI(IACNX1+ZI(ILCNX1-1+NUMAES)+I-2)
          DEPLPE(1)=DEPLPE(1)+ZR(JDEPDE-1+4*(NUNO-1)+1)*FF(I)
          DEPLPE(2)=DEPLPE(2)+ZR(JDEPDE-1+4*(NUNO-1)+2)*FF(I)
          DEPLME(1)=DEPLME(1)+ZR(JDEMDE-1+4*(NUNO-1)+1)*FF(I)
          DEPLME(2)=DEPLME(2)+ZR(JDEMDE-1+4*(NUNO-1)+2)*FF(I)
          LAGSCP   =LAGSCP   +ZR(JDEPDE-1+4*(NUNO-1)+3)*FF(I)
   32   CONTINUE
                 
        NUTYP = ZI(JTYMAI-1+NUMAMA)
         
        IF (NUTYP.EQ.2) THEN
          ALIAS = 'SG2'
          NNOM = 2
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
        ELSE IF (NUTYP.EQ.4) THEN
          ALIAS = 'SG3'
          NNOM = 3
          X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
        ELSE
          CALL U2MESS('F','ELEMENTS_16')
        END IF
            
        CALL CALFFX(ALIAS,X(1),X(2),FF)
         
        DEPLPM(1) = 0.D0
        DEPLPM(2) = 0.D0
        DEPLMM(1) = 0.D0
        DEPLMM(2) = 0.D0
        
        DO 42 I = 1,NNOM
          NUNO = ZI(IACNX1+ZI(ILCNX1-1+NUMAMA)+I-2)
          DEPLPM(1)=DEPLPM(1)+ZR(JDEPDE-1+4*(NUNO-1)+1)*FF(I)
          DEPLPM(2)=DEPLPM(2)+ZR(JDEPDE-1+4*(NUNO-1)+2)*FF(I)
          DEPLMM(1)=DEPLMM(1)+ZR(JDEMDE-1+4*(NUNO-1)+1)*FF(I)
          DEPLMM(2)=DEPLMM(2)+ZR(JDEMDE-1+4*(NUNO-1)+2)*FF(I)
   42   CONTINUE
                 
        DO 324 I=1,NDIM
          DEPPT(I) = 0.D0
          DEPMT(I) = 0.D0
          DO 314  K=1,NDIM
            DEPPT(I)= C(I,K)*(DEPLPE(K)-DEPLPM(K))+DEPPT(I)
            DEPMT(I)= C(I,K)*(DEPLME(K)-DEPLMM(K))+DEPMT(I)
            DEP(I)  = DEPPT(I) - DEPMT(I)
  314     CONTINUE
  324   CONTINUE
          
        DISSIP = 0.D0
          
        DO 55 K = 1,NDIM
          DISSIP = DISSIP + (DEP(K)*DEP(K))
   55   CONTINUE
         
        DISSIP = SQRT(DISSIP)
        
        IF (PREMIE) THEN
          ZR(JVALEM+(NTPC+INO-1)) = 0.D0
          ZR(JVALEP+(NTPC+INO-1)) = 0.D0
          ZR(JVALEI+(NTPC+INO-1)) = 0.D0
          ZR(JVALEX+(NTPC+INO-1)) = 0.D0
        ELSE
          ZR(JVALEM+(NTPC+INO-1)) = ZR(JVALEX+(NTPC+INO-1))
        END IF
        
        ZR(JVALEP+(NTPC+INO-1))= ZR(JVALEM+(NTPC+INO-1))+
     &                             ((KWEAR/HWEAR)*
     &                             ABS(DISSIP*LAGSCP))
        ZR(JVALEX+(NTPC+INO-1))= ZR(JVALEP+(NTPC+INO-1))
        ZR(JVALEI+(NTPC+INO-1))= ZR(JVALEP+(NTPC+INO-1))
        ZR(JUSU  +(NTPC+INO-1))= ZR(JVALEX+(NTPC+INO-1))
        JVALEM = JVALEM + (NCMPMX - NCMPU)
        JVALEP = JVALEP + (NCMPMX - NCMPU)
        JVALEI = JVALEI + (NCMPMX - NCMPU) 
        JVALEX = JVALEX + (NCMPMX - NCMPU)
 52   CONTINUE
      NTPC = NTPC + NBNOC
 62   CONTINUE      


      ELSE
        CALL U2MESS('F','ALGORITH6_13')
      END IF
 
 99   CONTINUE
C
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
