      SUBROUTINE MAJUSU(NOMA  ,DEFICO,RESOCO,DEPMOI,DEPDEL)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*19 DEPMOI,DEPDEL           
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
C      
      INTEGER      CFMMVD,ZTABF
      INTEGER      IFM,NIV    
      INTEGER      IBID 
      INTEGER      IACNX1,ILCNX1
      INTEGER      CFDISI,NDIMG,NZOCO
      INTEGER      NBMAE,JDECME
      INTEGER      NUNO,NDD1
      INTEGER      I,J,K,IPTC
      INTEGER      IZONE,IMAE,IPTM
      CHARACTER*24 TABFIN,JEUSUR
      INTEGER      JTABF ,JUSU
      INTEGER      NCMPMX
      INTEGER      GD,NPTM,NNM,NNE
      CHARACTER*19 USUMOI,USUPLU,USUFIX,USUINI
      INTEGER      JVALVM,JVALVP,JVALVI,JVALVX      
      INTEGER      POSMAE,NUMMAM,NUMMAE
      REAL*8       MMINFR,KWEAR,HWEAR
      INTEGER      MMINFI
      REAL*8       TAU1(3),TAU2(3),NORM(3),FF(9)
      REAL*8       KSIPC1,KSIPC2,KSIPR1,KSIPR2
      REAL*8       DEPLPE(3),DEPLME(3),DEPLPM(3),DEPLMM(3),LAGSCP
      REAL*8       DEPPT(3),DEPMT(3),DEP(3),DISSIP,C(3,3)
      REAL*8       NOOR,R8PREM
      CHARACTER*8  ALIASE,ALIASM,K8BID,NOMGD
      CHARACTER*19 DEPCN,DEMCN
      INTEGER      JDEPDE,JDEMDE
      LOGICAL      MMINFL,CFDISL,LUSURE,LCTFC,LVERI    
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('CONTACT',IFM,NIV)     
C
C --- USURE ?      
C      
      LUSURE = CFDISL(DEFICO,'EXIS_USURE')
C
      IF (.NOT. LUSURE) THEN
        GOTO 999
      END IF
C
C --- INITIALISATIONS
C  
      LCTFC  = .TRUE.
      NZOCO  = CFDISI(DEFICO,'NZOCO')
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
C --- ACCES SD CONTACT      
C
      TABFIN = RESOCO(1:14)//'.TABFIN'
      JEUSUR = RESOCO(1:14)//'.JEUSUR'
      CALL JEVEUO(TABFIN,'L',JTABF )
      CALL JEVEUO(JEUSUR,'E',JUSU  ) 
      CALL JEVEUO(NOMA//'.CONNEX','L',IACNX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',ILCNX1)   
C
      ZTABF = CFMMVD('ZTABF')
C
C --- INITIALISATIONS
C
      NDIMG  = CFDISI(DEFICO,'NDIM' )
      NZOCO  = CFDISI(DEFICO,'NZOCO')     
C      
C --- ACCES AUX CARTES D'USURE      
C
      USUMOI = RESOCO(1:14)//'.USUM'
      USUPLU = RESOCO(1:14)//'.USUP'
      USUFIX = RESOCO(1:14)//'.USUF'
      USUINI = RESOCO(1:14)//'.USUI'       
C   
      CALL JEVEUO(USUMOI//'.VALE','E',JVALVM)
      CALL JEVEUO(USUPLU//'.VALE','E',JVALVP)
      CALL JEVEUO(USUINI//'.VALE','E',JVALVI)
      CALL JEVEUO(USUFIX//'.VALE','E',JVALVX)
C
C --- REDUCTION DES CHAMPS
C
      DEPCN  = '&&USCART.CNSPINR'
      DEMCN  = '&&USCART.CNSMINR'  
      CALL MMMRED(NDIMG ,LCTFC ,DEPDEL,DEPCN ,NDD1  )
      CALL MMMRED(NDIMG ,LCTFC ,DEPMOI,DEMCN ,NDD1  )      
C
      CALL JEVEUO(DEPCN//'.CNSV','L',JDEPDE)
      CALL JEVEUO(DEMCN//'.CNSV','L',JDEMDE)
C
C --- BOUCLE SUR LES ZONES
C
      IPTC   = 1
      DO 10 IZONE = 1,NZOCO
C
C --- OPTIONS SUR LA ZONE DE CONTACT
C      
        KWEAR  = MMINFR(DEFICO,'USURE_K',IZONE )
        HWEAR  = MMINFR(DEFICO,'USURE_H',IZONE ) 
        LVERI  = MMINFL(DEFICO,'VERIF' ,IZONE )
        NBMAE  = MMINFI(DEFICO,'NBMAE' ,IZONE )
        JDECME = MMINFI(DEFICO,'JDECME',IZONE )             
C 
C ----- MODE VERIF: ON SAUTE LES POINTS
C  
        IF (LVERI) THEN
          GOTO 25
        ENDIF
C
C ----- BOUCLE SUR LES MAILLES ESCLAVES
C      
        DO 20 IMAE = 1,NBMAE      
C
C ------- POSITION DE LA MAILLE ESCLAVE
C
          POSMAE = JDECME + IMAE        
C
C ------- NOMBRE DE POINTS SUR LA MAILLE ESCLAVE
C            
          CALL MMINFM(POSMAE,DEFICO,'NPTM',NPTM  )        
C
C ------- BOUCLE SUR LES POINTS
C      
          DO 30 IPTM = 1,NPTM 
C
C --------- INFOS
C  
            NUMMAM  = NINT(ZR(JTABF+ZTABF*(IPTC-1)+2))  
            NUMMAE  = NINT(ZR(JTABF+ZTABF*(IPTC-1)+1))
            KSIPR1  = ZR(JTABF+ZTABF*(IPTC-1)+5)
            KSIPR2  = ZR(JTABF+ZTABF*(IPTC-1)+6)
            KSIPC1  = ZR(JTABF+ZTABF*(IPTC-1)+3)
            KSIPC2  = ZR(JTABF+ZTABF*(IPTC-1)+4)
C
C --------- VECTEURS DIRECTEURS DU PLAN DE CONTACT
C
            TAU1(1) = ZR(JTABF+ZTABF*(IPTC-1)+7)
            TAU1(2) = ZR(JTABF+ZTABF*(IPTC-1)+8)
            TAU1(3) = ZR(JTABF+ZTABF*(IPTC-1)+9)
            TAU2(1) = ZR(JTABF+ZTABF*(IPTC-1)+10)
            TAU2(2) = ZR(JTABF+ZTABF*(IPTC-1)+11)
            TAU2(3) = ZR(JTABF+ZTABF*(IPTC-1)+12)           
C
C --------- NORMALE
C                  
            CALL MMNORM(NDIMG ,TAU1  ,TAU2  ,NORM  ,NOOR  )
            IF (NOOR.LT.R8PREM()) THEN
              CALL ASSERT(.FALSE.)
            ENDIF
C
C --------- MATRICE DE PROJECTION
C
            DO 321 I = 1,NDIMG
              DO 311 J = 1,NDIMG
               C(I,J) = -1.D0*NORM(I)*NORM(J)
  311         CONTINUE
  321       CONTINUE
            
            DO 331 I = 1,NDIMG
              C(I,I) = 1 + C(I,I)
  331       CONTINUE
  
C
C --------- MAILLE ESCLAVE
C   
            CALL MMELTY(NOMA  ,NUMMAE,ALIASE,NNE  ,IBID  )
            CALL MMNONF(NDIMG ,NNE   ,ALIASE,KSIPC1,KSIPC2,
     &                  FF    )
C
C --------- GLISSEMENTS MAILLE ESCLAVE
C       
            DEPLPE(1) = 0.D0
            DEPLPE(2) = 0.D0
            DEPLPE(3) = 0.D0
            DEPLME(1) = 0.D0
            DEPLME(2) = 0.D0
            DEPLME(3) = 0.D0
            LAGSCP    = 0.D0        
            DO 31 I = 1,NNE
              NUNO      = ZI(IACNX1+ZI(ILCNX1-1+NUMMAE)+I-2)
              DEPLPE(1) = DEPLPE(1)+ZR(JDEPDE-1+NDD1*(NUNO-1)+1)*FF(I)
              DEPLPE(2) = DEPLPE(2)+ZR(JDEPDE-1+NDD1*(NUNO-1)+2)*FF(I)
              DEPLME(1) = DEPLME(1)+ZR(JDEMDE-1+NDD1*(NUNO-1)+1)*FF(I)
              DEPLME(2) = DEPLME(2)+ZR(JDEMDE-1+NDD1*(NUNO-1)+2)*FF(I)
              IF (NDIMG.EQ.2) THEN
                LAGSCP    = LAGSCP   +ZR(JDEPDE-1+4*(NUNO-1)+3)*FF(I)
              ELSEIF (NDIMG.EQ.3) THEN
                LAGSCP    = LAGSCP   +ZR(JDEPDE-1+NDD1*(NUNO-1)+4)*FF(I)
                DEPLPE(3) = DEPLPE(3)+ZR(JDEPDE-1+NDD1*(NUNO-1)+3)*FF(I)
                DEPLME(3) = DEPLME(3)+ZR(JDEMDE-1+NDD1*(NUNO-1)+3)*FF(I)
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
   31       CONTINUE
C
C --------- MAILLE MAITRE
C   
            CALL MMELTY(NOMA  ,NUMMAM,ALIASM,NNM   ,IBID  )
            CALL MMNONF(NDIMG ,NNM   ,ALIASM,KSIPR1,KSIPR2,
     &                  FF    )   
C
C --------- GLISSEMENTS MAILLE MAITRE
C       
            DEPLPM(1) = 0.D0
            DEPLPM(2) = 0.D0
            DEPLPM(3) = 0.D0
            DEPLMM(1) = 0.D0
            DEPLMM(2) = 0.D0
            DEPLMM(3) = 0.D0 
            DO 41 I = 1,NNM
              NUNO      = ZI(IACNX1+ZI(ILCNX1-1+NUMMAM)+I-2)
              DEPLPM(1) = DEPLPM(1)+ZR(JDEPDE-1+NDD1*(NUNO-1)+1)*FF(I)
              DEPLPM(2) = DEPLPM(2)+ZR(JDEPDE-1+NDD1*(NUNO-1)+2)*FF(I)
              DEPLMM(1) = DEPLMM(1)+ZR(JDEMDE-1+NDD1*(NUNO-1)+1)*FF(I)
              DEPLMM(2) = DEPLMM(2)+ZR(JDEMDE-1+NDD1*(NUNO-1)+2)*FF(I)
              IF (NDIMG.EQ.3) THEN
                DEPLPM(3) = DEPLPM(3)+ZR(JDEPDE-1+NDD1*(NUNO-1)+3)*FF(I)
                DEPLMM(3) = DEPLMM(3)+ZR(JDEMDE-1+NDD1*(NUNO-1)+3)*FF(I)
              ENDIF
  41        CONTINUE     
C
C --------- DEPLACEMENT RELATIF ESCLAVE/MAITRE
C          
            DO 322 I=1,NDIMG
              DEPPT(I) = 0.D0
              DEPMT(I) = 0.D0
              DO 312  K=1,NDIMG
                DEPPT(I) = C(I,K)*(DEPLPE(K)-DEPLPM(K))+DEPPT(I)
                DEPMT(I) = C(I,K)*(DEPLME(K)-DEPLMM(K))+DEPMT(I)
                DEP(I)   = DEPPT(I)
  312         CONTINUE
  322       CONTINUE
C
C --------- PUISSANCE USURE
C          
            DISSIP = 0.D0        
            DO 54 K = 1,NDIMG
              DISSIP = DISSIP + (DEP(K)*DEP(K))
 54         CONTINUE
            DISSIP = SQRT(DISSIP)    
C
C --------- CARTES USURE
C        
            ZR(JVALVM+(IPTC-1)) = ZR(JVALVX+(IPTC-1))
            ZR(JVALVP+(IPTC-1)) = ZR(JVALVM+(IPTC-1))+
     &                     ((KWEAR/HWEAR)*DISSIP*ABS(LAGSCP))      
            ZR(JVALVX+(IPTC-1)) = ZR(JVALVP+(IPTC-1))
            ZR(JVALVI+(IPTC-1)) = ZR(JVALVP+(IPTC-1))
            ZR(JUSU  +(IPTC-1)) = ZR(JVALVX+(IPTC-1))
            JVALVM = JVALVM + (NCMPMX - NCMPU)
            JVALVP = JVALVP + (NCMPMX - NCMPU)
            JVALVI = JVALVI + (NCMPMX - NCMPU) 
            JVALVX = JVALVX + (NCMPMX - NCMPU)     
C
C --------- LIAISON DE CONTACT SUIVANTE
C
            IPTC   = IPTC + 1 
  30      CONTINUE
  20    CONTINUE
  25    CONTINUE
  10  CONTINUE 
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
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> ... USURE MOINS'
        CALL NMDEBG('VECT',USUMOI,6)
        WRITE (IFM,*) '<CONTACT> ... USURE PLUS'
        CALL NMDEBG('VECT',USUPLU,6)      
        WRITE (IFM,*) '<CONTACT> ... USURE FIXE'
        CALL NMDEBG('VECT',USUFIX,6)
      ENDIF      
C
 999  CONTINUE      
C
      CALL JEDEMA()      
      END
