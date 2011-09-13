      SUBROUTINE MMMRES(NOMA  ,INST  ,DEFICO,RESOCO,DEPPLU,
     &                  DEPDEL,VEASSE,CNSINR,CNSPER)
C
C MODIF ALGORITH  DATE 12/09/2011   AUTEUR ABBAS M.ABBAS 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT     NONE
      CHARACTER*8  NOMA
      REAL*8       INST(*)
      CHARACTER*19 CNSINR,CNSPER
      CHARACTER*19 VEASSE(*)
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*19 DEPDEL,DEPPLU
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - POST-TRAITEMENT)
C
C REMPLIR LE CHAM_NO_S POUR L ARCHIVAGE DU CONTACT PAR NMARCH
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DE DEFINITION DU CONTACT
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C IN  DEPPLU : DEPLACEMENT COURANT
C IN  NOMA   : NOM DU MAILLAGE
C IN  INST   : PARAMETRES INTEGRATION EN TEMPS (T+, DT, THETA)
C IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
C OUT CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
C OUT CNSPER : CHAM_NO_S POUR L'ARCHIVAGE DES PERCUSSIONS
C
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32 JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------
C
      INTEGER      IPTC
      INTEGER      IZONE,IMAE,IPTM
      INTEGER      NDD1,NPTM,NBMAE
      INTEGER      JCONT,JFROT
      INTEGER      IACNX1,ILCNX1
      INTEGER      NZOCO
      INTEGER      JDEPDE,JDEPLU
      INTEGER      NDIMG,MMINFI
      INTEGER      CFMMVD,ZTABF,ZRESU,ZPERC
      INTEGER      NUMNOE
      INTEGER      POSMAE
      INTEGER      JDECME
      REAL*8       GLI,GLI1,GLI2
      REAL*8       RN,RNX,RNY,RNZ
      REAL*8       RTAX,RTAY,RTAZ
      REAL*8       RTGX,RTGY,RTGZ
      REAL*8       R,RX,RY,RZ
      REAL*8       IMP,IMPX,IMPY,IMPZ
      REAL*8       CONT,LAGSF
      CHARACTER*8  LICNT3(3)
      CHARACTER*19 FCONTS,FFROTS,DEPDES,DEPCN
      CHARACTER*19 FCTCN,FFROCN
      CHARACTER*19 FCONT,FFROT
      CHARACTER*24 GLIE,GLIM
      INTEGER      JGLIE,JGLIM
      CHARACTER*24 TABFIN,APJEU
      INTEGER      JTABF ,JAPJEU
      CHARACTER*24 JEUSUR
      INTEGER      JUSU  
      INTEGER      CFDISI
      REAL*8       DELTAT,EPS
      INTEGER      IFM,NIV
      LOGICAL      CFDISL,MMINFL,LFROT,LUSUR,LVERI,LNOEU
      INTEGER      JCNSVR,JCNSLR,JCNSVP,JCNSLP
      PARAMETER (EPS=1.D-6)
C ----------------------------------------------------------------------
      DATA LICNT3
     &   / 'DX'     ,'DY'      ,'DZ'      /
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C --- TYPE DE CONTACT
C      
      LFROT  = CFDISL(DEFICO,'FROTTEMENT')
C
C --- INITIALISATIONS
C       
      NZOCO  = CFDISI(DEFICO,'NZOCO' )
      NDIMG  = CFDISI(DEFICO,'NDIM' )
      DELTAT = INST(2)
C
C --- TOUTES LES ZONES EN INTEGRATION AUX NOEUDS ?
C
      LNOEU = CFDISL(DEFICO,'ALL_INTEG_NOEUD')
      IF (.NOT.LNOEU) THEN
        CALL U2MESS('A','CONTACT3_16')
        GOTO 999
      ENDIF
C
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C
      TABFIN = RESOCO(1:14)//'.TABFIN'
       APJEU = RESOCO(1:14)//'.APJEU '
      JEUSUR = RESOCO(1:14)//'.JEUSUR'
      CALL JEVEUO(TABFIN,'L',JTABF)
      CALL JEVEUO(APJEU ,'L',JAPJEU)
      CALL JEVEUO(JEUSUR,'L',JUSU)
C
      ZTABF  = CFMMVD('ZTABF')
      ZPERC  = CFMMVD('ZPERC')
      ZRESU  = CFMMVD('ZRESU') 
C
C --- NOM DES OBJETS LOCAUX
C
      FCONT  = '&&MMMRES.CONT'
      FCONTS = '&&MMMRES.CONT_S'
      FCTCN  = '&&MMMRES.FCTCN'
      FFROT  = '&&MMMRES.FROT'
      FFROTS = '&&MMMRES.FROT_S'
      FFROCN = '&&MMMRES.FROTCN'
      DEPDES = '&&MMMRES.DEPDES'
      DEPCN  = '&&MMMRES.DEPCN'
      GLIE   = '&&MMMRES.GLIE'
      GLIM   = '&&MMMRES.GLIM'
C
C --- ACCES AU MAILLAGE
C
      CALL JEVEUO(JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',ILCNX1)
      CALL JEVEUO(NOMA(1:8)//'.CONNEX','L',IACNX1)
C
C --- REDUCTION DU CHAM_NO_S DES DDL EN UN CHAM_NO_S DES LAGRANGES
C --- DE CONTACT/FROTTEMENT
C
      CALL MMMRED(NDIMG ,LFROT ,DEPDEL,DEPCN ,NDD1  )
C
C --- CALCULER LES GLISSEMENTS
C  
      CALL MMMREG(NOMA  ,DEFICO,RESOCO,DEPCN ,NDD1  ,
     &            GLIE  ,GLIM  )     
      CALL JEVEUO(GLIE  ,'L',JGLIE )
      CALL JEVEUO(GLIM  ,'L',JGLIM )
C      
C --- ACCES AU CHAM_NO_S POUR LES DEPLACEMENTS/LAGRANGES
C
      CALL JEVEUO(DEPCN(1:19)//'.CNSV','L',JDEPDE)
      CALL JEVEUO(DEPPLU(1:19)//'.VALE','L',JDEPLU)
C
C --- ACCES AU CHAM_NO_S POUR LE CONTACT
C
      CALL JEVEUO(CNSINR(1:19)//'.CNSV','E',JCNSVR)
      CALL JEVEUO(CNSINR(1:19)//'.CNSL','E',JCNSLR)
C
C --- ACCES AU CHAM_NO_S POUR LES PERCUSSIONS
C --- ON NE REMET PAS A ZERO D'UN PAS A L'AUTRE
C
      CALL JEVEUO(CNSPER(1:19)//'.CNSV','E',JCNSVP)
      CALL JEVEUO(CNSPER(1:19)//'.CNSL','E',JCNSLP)
C
C --- FORCES NODALES DE CONTACT
C
      CALL NMCHEX(VEASSE,'VEASSE','CNELTC',FCONT)
      CALL CNOCNS(FCONT,'V',FCONTS)
      CALL CNSRED(FCONTS,0,0,NDIMG,LICNT3,'V',FCTCN)
      CALL JEVEUO(FCTCN//'.CNSV','L',JCONT)
C
C --- FORCES NODALES DE FROTTEMENT
C
      IF (LFROT) THEN
        CALL NMCHEX(VEASSE,'VEASSE','CNELTF',FFROT)
        CALL CNOCNS(FFROT,'V',FFROTS)
        CALL CNSRED(FFROTS,0,0,NDIMG,LICNT3,'V',FFROCN)
        CALL JEVEUO(FFROCN//'.CNSV','L',JFROT)
      ENDIF
C
C --- BOUCLE SUR LES ZONES
C
      IPTC   = 1
      DO 10 IZONE = 1,NZOCO
C
C --- OPTIONS SUR LA ZONE DE CONTACT
C             
        LVERI  = MMINFL(DEFICO,'VERIF' ,IZONE )
        LUSUR  = MMINFL(DEFICO,'USURE' ,IZONE )        
        NBMAE  = MMINFI(DEFICO,'NBMAE' ,IZONE )
        JDECME = MMINFI(DEFICO,'JDECME',IZONE )
        LFROT  = MMINFL(DEFICO,'FROTTEMENT_ZONE',IZONE)
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
C --------- INIT
C          
            GLI  = 0.D0
            GLI1 = 0.D0
            GLI2 = 0.D0
            RTAX = 0.D0
            RTAY = 0.D0
            RTAZ = 0.D0
            RTGX = 0.D0
            RTGY = 0.D0
            RTGZ = 0.D0
            RN   = 0.D0
            RNX  = 0.D0
            RNY  = 0.D0
            RNZ  = 0.D0          
C
C --------- INFOS
C  
            NUMNOE  = ZR(JTABF+ZTABF*(IPTC-1)+24)
            IF (NUMNOE.LE.0) THEN 
              GOTO 99
            ENDIF           
            CONT    = ZR(JTABF+ZTABF*(IPTC-1)+22)
C
C --------- RECUPERATION DES FORCES NODALES DE CONTACT
C
            IF (CONT.GE.1.D0) THEN
              IF (NDIMG.EQ.3) THEN
                RNX = ZR(JCONT-1+3*(NUMNOE-1)+1)
                RNY = ZR(JCONT-1+3*(NUMNOE-1)+2)
                RNZ = ZR(JCONT-1+3*(NUMNOE-1)+3)
                RN  = SQRT(RNX**2+RNY**2+RNZ**2)
              ELSEIF (NDIMG.EQ.2) THEN
                RNX = ZR(JCONT-1+2*(NUMNOE-1)+1)
                RNY = ZR(JCONT-1+2*(NUMNOE-1)+2)
                RN  = SQRT(RNX**2+RNY**2)              
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF 
C
C --------- FROTTEMENT 
C      
              IF (LFROT) THEN                               
C
C ------------- CALCUL DU GLISSEMENT
C
                IF (NDIMG.EQ.3) THEN
                  GLI1 = ZR(JGLIE+2*(IPTC-1))-ZR(JGLIM+2*(IPTC-1))
                  GLI2 = ZR(JGLIE+2*(IPTC-1)+1) -
     &                   ZR(JGLIM+2*(IPTC-1)+1)
                  GLI  = SQRT(GLI1**2+GLI2**2)
                ELSEIF (NDIMG.EQ.2) THEN
                  GLI1 = ZR(JGLIE+IPTC-1) - ZR(JGLIM+IPTC-1)
                  GLI  = ABS(GLI1)
                ELSE
                  CALL ASSERT(.FALSE.)
                ENDIF
C
C ------------- NORME DU MULTIPLICATEUR DE LAGRANGE DU FROTTEMENT
C
                IF (NDIMG.EQ.3) THEN
                  LAGSF = SQRT(ZR(JDEPDE-1+NDD1*(NUMNOE-1)+5)**2+
     &                         ZR(JDEPDE-1+NDD1*(NUMNOE-1)+6)**2)
                ELSEIF (NDIMG.EQ.2) THEN
                  LAGSF = ABS (ZR(JDEPDE-1+NDD1*(NUMNOE-1)+4))
                ELSE
                  CALL ASSERT(.FALSE.)
                ENDIF
C
C ------------- GLISSANT OU ADHERENT
C            
                IF (LAGSF.GE.0.999D0) THEN
C
C --------------- GLISSANT
C
                  IF (NDIMG.EQ.3) THEN
                    CONT   = 2.D0
                    RTGX   = ZR(JFROT-1+3*(NUMNOE-1)+1)
                    RTGY   = ZR(JFROT-1+3*(NUMNOE-1)+2)
                    RTGZ   = ZR(JFROT-1+3*(NUMNOE-1)+3)
                  ELSEIF (NDIMG.EQ.2) THEN
                    CONT   = 2.D0
                    RTGX   = ZR(JFROT-1+2*(NUMNOE-1)+1)
                    RTGY   = ZR(JFROT-1+2*(NUMNOE-1)+2)
                  ELSE
                    CALL ASSERT(.FALSE.)
                  ENDIF
                ELSE
C
C --------------- ADHERENT
C
                  IF (NDIMG.EQ.3) THEN
                    RTAX   = ZR(JFROT-1+3*(NUMNOE-1)+1)
                    RTAY   = ZR(JFROT-1+3*(NUMNOE-1)+2)
                    RTAZ   = ZR(JFROT-1+3*(NUMNOE-1)+3)
                  ELSEIF (NDIMG.EQ.2) THEN
                    RTAX   = ZR(JFROT-1+2*(NUMNOE-1)+1)
                    RTAY   = ZR(JFROT-1+2*(NUMNOE-1)+2)
                  ELSE
                    CALL ASSERT(.FALSE.)
                  ENDIF
                ENDIF
              ELSE
                LAGSF   = 0.D0
                CONT    = 2.D0
              ENDIF
            ENDIF  
C
C --------- REACTIONS TOTALES
C
            RX     = RNX + RTAX + RTGX
            RY     = RNY + RTAY + RTGY
            RZ     = RNZ + RTAZ + RTGZ
            R      = SQRT(RX**2.D0+RY**2.D0+RZ**2.D0)  
C
C --------- CALCUL DES PERCUSSIONS
C
            IF (R .LE. EPS) THEN
              IMP    = 0.D0
              IMPX   = 0.D0
              IMPY   = 0.D0
              IMPZ   = 0.D0
            ELSE
              IMP    = ZR(JCNSVP+ZPERC*(NUMNOE-1)+1-1) + R*DELTAT
              IMPX   = ZR(JCNSVP+ZPERC*(NUMNOE-1)+2-1) + RX*DELTAT
              IMPY   = ZR(JCNSVP+ZPERC*(NUMNOE-1)+3-1) + RY*DELTAT
              IMPZ   = ZR(JCNSVP+ZPERC*(NUMNOE-1)+4-1) + RZ*DELTAT
            ENDIF
C
C --------- ARCHIVAGE DES RESULTATS DANS LE CHAM_NO_S VALE_CONT
C
            ZR(JCNSVR+ZRESU*(NUMNOE-1)+1 -1) = CONT
            ZR(JCNSVR+ZRESU*(NUMNOE-1)+2 -1) = -ZR(JAPJEU+IPTC-1)
            ZL(JCNSLR+ZRESU*(NUMNOE-1)+1 -1) = .TRUE.
            ZL(JCNSLR+ZRESU*(NUMNOE-1)+2 -1) = .TRUE.
            IF (LUSUR) THEN
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+20-1) = ZR(JUSU-1+IPTC)
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+20-1) = .TRUE.
            ENDIF
            
            
            IF (NDIMG.EQ.3) THEN
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+3 -1) = RN
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+4 -1) = RNX
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+5 -1) = RNY
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+6 -1) = RNZ
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+7 -1) = GLI1
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+8 -1) = GLI2
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+9 -1) = GLI
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+10-1) = RTAX
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+11-1) = RTAY
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+12-1) = RTAZ
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+13-1) = RTGX
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+14-1) = RTGY
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+15-1) = RTGZ
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+16-1) = RX
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+17-1) = RY
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+18-1) = RZ
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+19-1) = R 
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+21-1) = IMP
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+22-1) = IMPX
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+23-1) = IMPY  
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+24-1) = IMPZ
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+3 -1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+4 -1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+5 -1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+6 -1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+7 -1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+8 -1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+9 -1) = .TRUE.              
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+10-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+11-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+12-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+13-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+14-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+15-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+16-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+17-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+18-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+19-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+21-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+22-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+23-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+24-1) = .TRUE.
            ELSEIF (NDIMG.EQ.2) THEN
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+3 -1) = RN
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+4 -1) = RNX
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+5 -1) = RNY
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+7 -1) = GLI1
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+9 -1) = GLI              
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+10-1) = RTAX
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+11-1) = RTAY
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+13-1) = RTGX
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+14-1) = RTGY
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+16-1) = RX
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+17-1) = RY
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+19-1) = R 
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+21-1) = IMP
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+22-1) = IMPX
              ZR(JCNSVR+ZRESU*(NUMNOE-1)+23-1) = IMPY              
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+3 -1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+4 -1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+5 -1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+7 -1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+9 -1) = .TRUE.              
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+10-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+11-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+13-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+14-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+16-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+17-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+19-1) = .TRUE. 
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+21-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+22-1) = .TRUE.
              ZL(JCNSLR+ZRESU*(NUMNOE-1)+23-1) = .TRUE.
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
C
C --------- ARCHIVAGE DES RESULTATS DANS LE CHAM_NO_S VALE_PERC
C
            ZR(JCNSVP+ZPERC*(NUMNOE-1)+1-1) = IMP
            ZR(JCNSVP+ZPERC*(NUMNOE-1)+2-1) = IMPX
            ZR(JCNSVP+ZPERC*(NUMNOE-1)+3-1) = IMPY     
            ZL(JCNSLP+ZPERC*(NUMNOE-1)+1-1) = .TRUE.
            ZL(JCNSLP+ZPERC*(NUMNOE-1)+2-1) = .TRUE.
            ZL(JCNSLP+ZPERC*(NUMNOE-1)+3-1) = .TRUE.
            
            IF (NDIMG.EQ.3) THEN
              ZR(JCNSVP+ZPERC*(NUMNOE-1)+4-1) = IMPZ
              ZL(JCNSLP+ZPERC*(NUMNOE-1)+4-1) = .TRUE.
            ENDIF
  99        CONTINUE          
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
      CALL JEDETR(FCONT)
      CALL DETRSD('CHAMP',FCONTS)
      CALL DETRSD('CHAMP',FCTCN)
      CALL JEDETR(FFROT)
      CALL DETRSD('CHAMP',FFROTS)
      CALL DETRSD('CHAMP',FFROCN)
      CALL DETRSD('CHAMP',DEPDES)
      CALL DETRSD('CHAMP',DEPCN)
      CALL JEDETR(GLIE)
      CALL JEDETR(GLIM)
C
  999 CONTINUE
      CALL JEDEMA()
      END
