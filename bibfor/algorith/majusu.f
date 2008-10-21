      SUBROUTINE MAJUSU(NOMA  ,DEFICO,RESOCO,DEPMOI,DEPDEL)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/10/2008   AUTEUR DESOZA T.DESOZA 
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
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT NONE
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO,RESOCO       
      CHARACTER*24 DEPMOI,DEPDEL           
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - USURE)
C
C MISE A JOUR DES CARTES D'USURE
C      
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEFICO : SD DE DEFINITION DU CONTACT
C IN  RESOCO : SD DE RESOLUTION DU CONTACT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C IN  DEPMOI : DEPLACEMENT INITIAL
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      CHARACTER*32       JEXATR,JEXNOM,JEXNUM
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
      INTEGER      NCMPU
      PARAMETER    (NCMPU=1)
      CHARACTER*8  LICMP4(4),LICMP6(6)           
C      
      INTEGER      CFMMVD,ZTABF,ZMAES
      INTEGER      IFM,NIV    
      INTEGER      IZONE,IBID 
      INTEGER      JTABF,IACNX1,ILCNX1,JCMCF
      INTEGER      JDIM,NDIM,NUNO,JUSU
      INTEGER      I,J,K,INO,IMA
      INTEGER      NCMPMX,JMAESC,NTMA
      INTEGER      JDEPDE,JDEPDL,JDEMDE,JDEMDL,NBPC,GD
      CHARACTER*19 USUMOI,USUPLU,USUFIX,USUINI
      INTEGER      JVALVM,JVALVP,JVALVI,JVALVX      
      INTEGER      NUMMAM,NUMMAE,NNOM,NNOE,NTPC
      REAL*8       KWEAR,HWEAR,TAU1(3),TAU2(3),NORM(3),XPG,YPG,FF(9)
      REAL*8       DEPLPE(3),DEPLME(3),DEPLPM(3),DEPLMM(3),LAGSCP
      REAL*8       DEPPT(3),DEPMT(3),DEP(3),DISSIP,C(3,3)
      REAL*8       R8BID,NOOR,R8PREM
      CHARACTER*8  ALIAS,K8BID,NOMGD
      CHARACTER*19 DEPDES,DEPCN,DEPMOS,DEMCN
      CHARACTER*24 K24BID,K24BLA
      LOGICAL      LUSURE,LBID
C ----------------------------------------------------------------------
      DATA LICMP4
     &   / 'DX'     ,'DY'      ,
     &     'LAGS_C' ,'LAGS_F1' /   
      DATA LICMP6
     &   / 'DX'     ,'DY'      ,'DZ'      ,
     &     'LAGS_C' ,'LAGS_F1' ,'LAGS_F2' /         
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C --- USURE ?      
C      
      K24BLA = ' '
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
C --- NBRE COMPOSANTES DE LA GRANDEUR
C      
      NOMGD = 'NEUT_R'
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',NOMGD),GD)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K8BID) 
C
C --- AFFICHAGE
C      
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> MISE A JOUR DE LA CARTE DES'//
     &        ' PROFILS USURE' 
      ENDIF  
C      
C --- RECUPERATION DE QUELQUES DONNEES      
C
      CALL JEVEUO(DEFICO(1:16)//'.NDIMCO','L',JDIM)
      CALL JEVEUO(DEFICO(1:16)//'.TABFIN','L',JTABF)
      CALL JEVEUO(DEFICO(1:16)//'.CARACF','L',JCMCF)
      CALL JEVEUO(DEFICO(1:16)//'.MAESCL','L',JMAESC) 
      CALL JEVEUO(DEFICO(1:16)//'.JEUSUR','L',JUSU) 
      CALL JEVEUO(NOMA//'.CONNEX','L',IACNX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',ILCNX1)   
C
      ZTABF = CFMMVD('ZTABF')
      ZMAES = CFMMVD('ZMAES')
C
C --- INITIALISATIONS
C
      NTMA   = ZI(JMAESC)
      NTPC   = 0
      NDIM   = ZI(JDIM) 
C      
C --- ACCES AUX CARTES D'USURE      
C
      USUMOI = RESOCO(1:14)//'.USUM'
      USUPLU = RESOCO(1:14)//'.USUP'
      USUFIX = RESOCO(1:14)//'.USUF'
      USUINI = RESOCO(1:14)//'.USUI'       
C   
      CALL JEVEUO(USUMOI//'.VALE','L',JVALVM)
      CALL JEVEUO(USUPLU//'.VALE','L',JVALVP)
      CALL JEVEUO(USUINI//'.VALE','L',JVALVI)
      CALL JEVEUO(USUFIX//'.VALE','L',JVALVX)
C
C --- ACCES AUX CHAMPS RESULTATS REDUITS AUX DEPL+LAGR_CONTACT 
C      
      DEPDES = '&&USCART.DEPPINR'
      DEPMOS = '&&USCART.DEPMINR'
      DEPCN  = '&&USCART.CNSPINR'
      DEMCN  = '&&USCART.CNSMINR'   
      CALL CNOCNS(DEPDEL,'V',DEPDES)
      CALL CNOCNS(DEPMOI,'V',DEPMOS)
      IF (NDIM.EQ.3) THEN
        CALL CNSRED(DEPDES,0,0,6,LICMP6,'V',DEPCN)
        CALL CNSRED(DEPMOS,0,0,6,LICMP6,'V',DEMCN)
      ELSE IF (NDIM.EQ.2) THEN
        CALL CNSRED(DEPDES,0,0,4,LICMP4,'V',DEPCN)
        CALL CNSRED(DEPMOS,0,0,4,LICMP4,'V',DEMCN)
      ELSE
        CALL ASSERT(.FALSE.)  
      END IF
      
      CALL JEVEUO(DEPCN//'.CNSV','L',JDEPDE)
      CALL JEVEUO(DEPCN//'.CNSL','L',JDEPDL)
      CALL JEVEUO(DEMCN//'.CNSV','L',JDEMDE)
      CALL JEVEUO(DEMCN//'.CNSL','L',JDEMDL)

      
      IF (NDIM .EQ. 3) THEN  
        DO 61 IMA = 1,NTMA  
          NBPC  = ZI(JMAESC+ZMAES*(IMA-1)+3)
          IZONE = ZI(JMAESC+ZMAES*(IMA-1)+2)
          CALL MMINFP(IZONE ,DEFICO,K24BLA,'USURE_K',
     &                IBID  ,KWEAR ,K24BID,LBID)  
          CALL MMINFP(IZONE ,DEFICO,K24BLA,'USURE_H',
     &                IBID  ,HWEAR ,K24BID,LBID)                 
          DO 51 INO = 1,NBPC
            NUMMAE   = NINT(ZR(JTABF+ZTABF*(NTPC+INO-1)+1))
            NUMMAM   = NINT(ZR(JTABF+ZTABF*(NTPC+INO-1)+2))
            TAU1(1)  = ZR(JTABF+ZTABF*(NTPC+INO-1)+6)
            TAU1(2)  = ZR(JTABF+ZTABF*(NTPC+INO-1)+7)
            TAU1(3)  = ZR(JTABF+ZTABF*(NTPC+INO-1)+8)
            TAU2(1)  = ZR(JTABF+ZTABF*(NTPC+INO-1)+9)
            TAU2(2)  = ZR(JTABF+ZTABF*(NTPC+INO-1)+10)
            TAU2(3)  = ZR(JTABF+ZTABF*(NTPC+INO-1)+11)
            CALL MMNORM(NDIM,TAU1,TAU2,NORM,NOOR)
            IF (NOOR.LT.R8PREM()) THEN
              CALL ASSERT(.FALSE.)
            ENDIF
            
            DO 321 I = 1,NDIM
              DO 311 J = 1,NDIM
               C(I,J) = -1.D0*NORM(I)*NORM(J)
  311         CONTINUE
  321       CONTINUE
            
            DO 331 I = 1,NDIM
              C(I,I) = 1 + C(I,I)
  331       CONTINUE
C 
C --- MAILLE ESCLAVE
C       
            CALL MMELTY(NOMA,NUMMAE,ALIAS,NNOE,IBID)
            XPG = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
            YPG = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
            CALL MMNONF(NDIM,NNOE,ALIAS,XPG,YPG,FF)
            DEPLPE(1) = 0.D0
            DEPLPE(2) = 0.D0
            DEPLPE(3) = 0.D0
            DEPLME(1) = 0.D0
            DEPLME(2) = 0.D0
            DEPLME(3) = 0.D0
            LAGSCP    = 0.D0        
            DO 31 I = 1,NNOE
              NUNO      = ZI(IACNX1+ZI(ILCNX1-1+NUMMAE)+I-2)
              DEPLPE(1) = DEPLPE(1)+ZR(JDEPDE-1+6*(NUNO-1)+1)*FF(I)
              DEPLPE(2) = DEPLPE(2)+ZR(JDEPDE-1+6*(NUNO-1)+2)*FF(I)
              DEPLPE(3) = DEPLPE(3)+ZR(JDEPDE-1+6*(NUNO-1)+3)*FF(I)
              DEPLME(1) = DEPLME(1)+ZR(JDEMDE-1+6*(NUNO-1)+1)*FF(I)
              DEPLME(2) = DEPLME(2)+ZR(JDEMDE-1+6*(NUNO-1)+2)*FF(I)
              DEPLME(3) = DEPLME(3)+ZR(JDEMDE-1+6*(NUNO-1)+3)*FF(I)
              LAGSCP    = LAGSCP   +ZR(JDEPDE-1+6*(NUNO-1)+4)*FF(I)
   31       CONTINUE
C 
C --- MAILLE MAITRE
C             
            CALL MMELTY(NOMA,NUMMAM,ALIAS,NNOM,IBID)
            XPG = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
            YPG = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
            CALL MMNONF(NDIM,NNOM,ALIAS,XPG,YPG,FF)
            DEPLPM(1) = 0.D0
            DEPLPM(2) = 0.D0
            DEPLPM(3) = 0.D0
            DEPLMM(1) = 0.D0
            DEPLMM(2) = 0.D0
            DEPLMM(3) = 0.D0
            DO 41 I = 1,NNOM
              NUNO = ZI(IACNX1+ZI(ILCNX1-1+NUMMAM)+I-2)
              DEPLPM(1)=DEPLPM(1)+ZR(JDEPDE-1+6*(NUNO-1)+1)*FF(I)
              DEPLPM(2)=DEPLPM(2)+ZR(JDEPDE-1+6*(NUNO-1)+2)*FF(I)
              DEPLPM(3)=DEPLPM(3)+ZR(JDEPDE-1+6*(NUNO-1)+3)*FF(I)
              DEPLMM(1)=DEPLMM(1)+ZR(JDEMDE-1+6*(NUNO-1)+1)*FF(I)
              DEPLMM(2)=DEPLMM(2)+ZR(JDEMDE-1+6*(NUNO-1)+2)*FF(I)
              DEPLMM(3)=DEPLMM(3)+ZR(JDEMDE-1+6*(NUNO-1)+3)*FF(I)
  41        CONTINUE
C
C --- DEPLACEMENT RELATIF ESCLAVE/MAITRE
C          
            DO 322 I=1,NDIM
              DEPPT(I) = 0.D0
              DEPMT(I) = 0.D0
              DO 312  K=1,NDIM
                DEPPT(I) = C(I,K)*(DEPLPE(K)-DEPLPM(K))+DEPPT(I)
                DEPMT(I) = C(I,K)*(DEPLME(K)-DEPLMM(K))+DEPMT(I)
                DEP(I)   = DEPPT(I) - DEPMT(I)
  312         CONTINUE
  322       CONTINUE
C
C --- PUISSANCE USURE
C          
            DISSIP = 0.D0        
            DO 54 K = 1,NDIM
              DISSIP = DISSIP + (DEP(K)*DEP(K))
 54         CONTINUE
            DISSIP = SQRT(DISSIP)
C
C --- CARTES USURE
C        
            ZR(JVALVM+(NTPC+INO-1)) = ZR(JVALVX+(NTPC+INO-1))        
            ZR(JVALVP+(NTPC+INO-1)) = ZR(JVALVM+(NTPC+INO-1))+
     &                     ((KWEAR/HWEAR)*DISSIP*ABS(LAGSCP))      
            ZR(JVALVX+(NTPC+INO-1))= ZR(JVALVP+(NTPC+INO-1))
            ZR(JVALVI+(NTPC+INO-1))= ZR(JVALVP+(NTPC+INO-1))
            ZR(JUSU  +(NTPC+INO-1))= ZR(JVALVX+(NTPC+INO-1))
            JVALVM = JVALVM + (NCMPMX - NCMPU)
            JVALVP = JVALVP + (NCMPMX - NCMPU)
            JVALVI = JVALVI + (NCMPMX - NCMPU) 
            JVALVX = JVALVX + (NCMPMX - NCMPU)     
 51       CONTINUE
          NTPC = NTPC + NBPC
 61     CONTINUE
      ELSE IF (NDIM.EQ.2) THEN
        DO 62 IMA = 1,NTMA
          NBPC  = ZI(JMAESC+ZMAES* (IMA-1)+3)
          IZONE = ZI(JMAESC+ZMAES* (IMA-1)+2)
          CALL MMINFP(IZONE ,DEFICO,K24BLA,'USURE_K',
     &                IBID  ,KWEAR ,K24BID,LBID)  
          CALL MMINFP(IZONE ,DEFICO,K24BLA,'USURE_H',
     &                IBID  ,HWEAR ,K24BID,LBID)     
          DO 52 INO = 1,NBPC
            NUMMAE  = NINT(ZR(JTABF+ZTABF* (NTPC+INO-1)+1))
            NUMMAM   = NINT(ZR(JTABF+ZTABF* (NTPC+INO-1)+2))
            TAU1(1)  = ZR(JTABF+ZTABF* (NTPC+INO-1)+6)
            TAU1(2)  = ZR(JTABF+ZTABF* (NTPC+INO-1)+7)
            CALL MMNORM(NDIM,TAU1,TAU2,NORM,NOOR)
            IF (NOOR.LT.R8PREM()) THEN
              CALL ASSERT(.FALSE.)
            ENDIF
        
            DO 323 I = 1,NDIM
              DO 313 J = 1,NDIM
                C(I,J) = -1.D0*NORM(I)*NORM(J)
  313         CONTINUE
  323       CONTINUE
          
            DO 333 I = 1,NDIM
              C(I,I) = 1 + C(I,I)
  333       CONTINUE
C 
C --- MAILLE ESCLAVE
C       
            CALL MMELTY(NOMA,NUMMAE,ALIAS,NNOE,IBID)
            XPG = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
            YPG = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
            CALL MMNONF(NDIM,NNOE,ALIAS,XPG,YPG,FF)
            DEPLPE(1) = 0.D0
            DEPLPE(2) = 0.D0
            DEPLME(1) = 0.D0
            DEPLME(2) = 0.D0
            LAGSCP    = 0.D0  
            DO 32 I = 1,NNOE
              NUNO = ZI(IACNX1+ZI(ILCNX1-1+NUMMAE)+I-2)
              DEPLPE(1) = DEPLPE(1)+ZR(JDEPDE-1+4*(NUNO-1)+1)*FF(I)
              DEPLPE(2) = DEPLPE(2)+ZR(JDEPDE-1+4*(NUNO-1)+2)*FF(I)
              DEPLME(1) = DEPLME(1)+ZR(JDEMDE-1+4*(NUNO-1)+1)*FF(I)
              DEPLME(2) = DEPLME(2)+ZR(JDEMDE-1+4*(NUNO-1)+2)*FF(I)
              LAGSCP    = LAGSCP   +ZR(JDEPDE-1+4*(NUNO-1)+3)*FF(I)
   32       CONTINUE
C 
C --- MAILLE MAITRE
C             
            CALL MMELTY(NOMA,NUMMAM,ALIAS,NNOM,IBID)
            XPG = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
            YPG = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
            CALL MMNONF(NDIM,NNOM,ALIAS,XPG,YPG,FF)
            DEPLPM(1) = 0.D0
            DEPLPM(2) = 0.D0
            DEPLMM(1) = 0.D0
            DEPLMM(2) = 0.D0
            DO 42 I = 1,NNOM
              NUNO = ZI(IACNX1+ZI(ILCNX1-1+NUMMAM)+I-2)
              DEPLPM(1)=DEPLPM(1)+ZR(JDEPDE-1+4*(NUNO-1)+1)*FF(I)
              DEPLPM(2)=DEPLPM(2)+ZR(JDEPDE-1+4*(NUNO-1)+2)*FF(I)
              DEPLMM(1)=DEPLMM(1)+ZR(JDEMDE-1+4*(NUNO-1)+1)*FF(I)
              DEPLMM(2)=DEPLMM(2)+ZR(JDEMDE-1+4*(NUNO-1)+2)*FF(I)
   42       CONTINUE
C
C --- DEPLACEMENT RELATIF ESCLAVE/MAITRE
C                     
           DO 324 I=1,NDIM
             DEPPT(I) = 0.D0
             DEPMT(I) = 0.D0
             DO 314  K=1,NDIM
               DEPPT(I)= C(I,K)*(DEPLPE(K)-DEPLPM(K))+DEPPT(I)
               DEPMT(I)= C(I,K)*(DEPLME(K)-DEPLMM(K))+DEPMT(I)
               DEP(I)  = DEPPT(I) - DEPMT(I)
  314        CONTINUE
  324      CONTINUE
C
C --- PUISSANCE USURE
C           
            DISSIP = 0.D0        
            DO 55 K = 1,NDIM
              DISSIP = DISSIP + (DEP(K)*DEP(K))
 55         CONTINUE
            DISSIP = SQRT(DISSIP)
C
C --- CARTES USURE
C              
            ZR(JVALVM+(NTPC+INO-1)) = ZR(JVALVX+(NTPC+INO-1))           
            ZR(JVALVP+(NTPC+INO-1)) = ZR(JVALVM+(NTPC+INO-1))+
     &                             ((KWEAR/HWEAR)*
     &                             ABS(DISSIP*LAGSCP))
            ZR(JVALVX+(NTPC+INO-1)) = ZR(JVALVP+(NTPC+INO-1))
            ZR(JVALVI+(NTPC+INO-1)) = ZR(JVALVP+(NTPC+INO-1))
            ZR(JUSU  +(NTPC+INO-1)) = ZR(JVALVX+(NTPC+INO-1))
            JVALVM = JVALVM + (NCMPMX - NCMPU)
            JVALVP = JVALVP + (NCMPMX - NCMPU)
            JVALVI = JVALVI + (NCMPMX - NCMPU) 
            JVALVX = JVALVX + (NCMPMX - NCMPU)
 52       CONTINUE
          NTPC = NTPC + NBPC
 62     CONTINUE      
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
C
C --- MENAGE 
C
      CALL JEDETR(USUMOI//'.NCMP')
      CALL JEDETR(USUMOI//'.VALV')
      CALL JEDETR(USUPLU//'.NCMP')
      CALL JEDETR(USUPLU//'.VALV')
      CALL JEDETR(USUFIX//'.NCMP')
      CALL JEDETR(USUFIX//'.VALV')
C
 999  CONTINUE      
C
      CALL JEDEMA()      
      END
