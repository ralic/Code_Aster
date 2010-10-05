      SUBROUTINE CFRESU(NOMA  ,NUMINS,INST  ,DEFICO,RESOCO,
     &                  DEPPLU,DEPDEL,DDEPLA,CNSINR,CNSPER)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/10/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT     NONE
      INTEGER      NUMINS
      REAL*8       INST(*)
      CHARACTER*8  NOMA
      CHARACTER*19 CNSINR,CNSPER
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*19 DEPPLU,DEPDEL,DDEPLA
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE DISCRETE - POST-TRAITEMENT)
C
C CREER LE CHAM_NO_S POUR ARCHIVAGE DU CONTACT PAR NMARCH
C
C ----------------------------------------------------------------------
C
C
C IN  NUMINS : NUMERO DU PAS DE CHARGE
C IN  INST   : INST(1) = INSTANT COURANT DE CALCUL
C              INST(2) = INCREMENT TEMPOREL
C IN  DEFICO : SD DE DEFINITION DU CONTACT
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  DEPPLU : DEPLACEMENT COURANT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE 
C IN  DDEPLA : INCREMENT DE DEPLACEMENTS CALCULE EN IGNORANT LE CONTACT
C IN  NOMA   : NOM DU MAILLAGE
C OUT CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
C OUT CNSPER : CHAM_NO_S POUR L'ARCHIVAGE DES PERCUSSIONS
C
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
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
      INTEGER      CFMMVD,ZRESU,ZPERC,ZTACF
      INTEGER      CFDISD
      INTEGER      IFM,NIV
      INTEGER      NUMNOE,ILIAC,KK,ICMP,ILIAI
      INTEGER      NDIMG,NBLIAI,NBLIAC,NBDDL,NEQ
      INTEGER      BTOTAL,JDECAL,LLIAC
      INTEGER      NESMAX
      REAL*8       GLIX,GLIY,GLIT
      REAL*8       TESTMU,TESTCF,COEFPT
      REAL*8       VAL1,VAL2,VARC,R8PREM,R8MIEM
      REAL*8       R,RX,RY,RZ
      REAL*8       RN,RNX,RNY,RNZ,HN
      REAL*8       RTAX,RTAY,RTAZ,RTGX,RTGY,RTGZ
      REAL*8       TAU1(3),TAU2(3),NORM(3)
      REAL*8       DELTAT,INSTAP,R8BID
      CHARACTER*2  TYPEC0,TYPLIA
      CHARACTER*19 LIAC,ATMU,AFMU,MU,TYPL
      INTEGER      JLIAC,JATMU,JAFMU,JMU,JTYPL
      CHARACTER*24 APDDL,APJEU,APCOFR
      INTEGER      JAPDDL,JAPJEU,JAPCOF
      CHARACTER*24 TANGCO,TACFIN
      INTEGER      JTANGO,JTACF
      CHARACTER*24 APPOIN,NUMLIA
      INTEGER      JAPPTR,JNUMLI 
      INTEGER      JCNSVR,JCNSLR
      INTEGER      JDEPLU,JDEPDE,JDDEPL
      LOGICAL      CFDISL,LPENAC,LCTFD,LPENAF
      LOGICAL      LAG2D
      REAL*8       IMP,IMPX,IMPY,IMPZ
      REAL*8       EPS
      INTEGER      JCNSVP,JCNSLP     
      PARAMETER    (EPS=1.D-6)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('CONTACT',IFM,NIV)
C
C --- INITIALISATIONS
C
      INSTAP = INST(1)
      DELTAT = INST(2) 
      TYPEC0 = 'C0'
C
C --- FONCTIONNALITES ACTIVEES
C
      LCTFD  = CFDISL(DEFICO,'FROT_DISCRET')
      LPENAC = CFDISL(DEFICO,'CONT_PENA'   )
      LPENAF = CFDISL(DEFICO,'FROT_PENA'   ) 
      LAG2D  = CFDISL(DEFICO,'FROT_LAGR_2D')           
C
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C 
      AFMU   = RESOCO(1:14)//'.AFMU'
      APCOFR = RESOCO(1:14)//'.APCOFR'
      APDDL  = RESOCO(1:14)//'.APDDL'
      APJEU  = RESOCO(1:14)//'.APJEU'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      NUMLIA = RESOCO(1:14)//'.NUMLIA' 
      ATMU   = RESOCO(1:14)//'.ATMU'
      TYPL   = RESOCO(1:14)//'.TYPL'
      LIAC   = RESOCO(1:14)//'.LIAC'
      MU     = RESOCO(1:14)//'.MU'
      TANGCO = RESOCO(1:14)//'.TANGCO'
      TACFIN = RESOCO(1:14)//'.TACFIN'     
C 
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APJEU, 'L',JAPJEU)
      CALL JEVEUO(TACFIN,'L',JTACF )  
      CALL JEVEUO(NUMLIA,'L',JNUMLI)    
      CALL JEVEUO(APDDL, 'L',JAPDDL)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(ATMU,  'L',JATMU )
      IF (LCTFD) THEN      
        CALL JEVEUO(APCOFR,'L',JAPCOF)
        CALL JEVEUO(AFMU,  'L',JAFMU )
      ENDIF
      IF (LPENAC) THEN
        CALL JEVEUO(AFMU,  'L',JAFMU )
      ENDIF
      CALL JEVEUO(TYPL  ,'L',JTYPL)
      CALL JEVEUO(LIAC,  'L',JLIAC)
      CALL JEVEUO(MU,    'L',JMU   )
      CALL JEVEUO(TANGCO,'L',JTANGO)  
C 
      ZRESU  = CFMMVD('ZRESU')
      ZPERC  = CFMMVD('ZPERC')
      ZTACF  = CFMMVD('ZTACF')  
C
C --- CARACTERISTIQUES DU CONTACT
C   
      NBLIAI = CFDISD(RESOCO,'NBLIAI')
      NBLIAC = CFDISD(RESOCO,'NBLIAC')
      NDIMG  = CFDISD(RESOCO,'NDIM'  )
      NEQ    = CFDISD(RESOCO,'NEQ'   )   
      BTOTAL = CFDISD(RESOCO,'BTOTAL')
      NESMAX = CFDISD(RESOCO,'NESMAX')
C
C --- ACCES AUX CHAM_NO POUR LES DEPLACEMENTS
C          
      CALL JEVEUO(DEPPLU(1:19)//'.VALE','L',JDEPLU)
      CALL JEVEUO(DDEPLA(1:19)//'.VALE','L',JDDEPL)
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)     
C
C --- ACCES AU CHAM_NO_S POUR LE CONTACT
C
      CALL JEVEUO(CNSINR(1:19)//'.CNSV','E',JCNSVR)
      CALL JEVEUO(CNSINR(1:19)//'.CNSL','E',JCNSLR)
C
C --- ACCES AU CHAM_NO_S POUR LES PERCUSSIONS
C
      CALL JEVEUO(CNSPER(1:19)//'.CNSV','E',JCNSVP)
      CALL JEVEUO(CNSPER(1:19)//'.CNSL','E',JCNSLP)
C
C --- INITIALISATIONS DES CHAM_NO_S: NOEUDS ACTIFS
C
      DO 10 ILIAI = 1,NBLIAI
        NUMNOE = ZI(JNUMLI+3*(ILIAI-1)+3-1)
        DO 11 ICMP = 1,ZRESU
          ZL(JCNSLR-1+ZRESU*(NUMNOE-1)+ICMP) = .TRUE.
          ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+ICMP) = 0.D0
 11     CONTINUE    
        DO 12 ICMP = 1,ZPERC
          ZL(JCNSLP-1+ZPERC*(NUMNOE-1)+ICMP) = .TRUE.
          ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+ICMP) = 0.D0
 12     CONTINUE
C
C ----- JEU
C             
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+2 ) = ZR(JAPJEU+ILIAI-1)   
 10   CONTINUE
C
C ======================================================================
C
C --- CALCUL DES REACTIONS ET FORCES
C
C ======================================================================
C
      DO 20 ILIAC = 1,BTOTAL
C
C ----- NOEUD EN CONTACT
C
         VARC = 2.0D0
C
C ----- INITIALISATIONS: CONTACT SANS FROTTEMENT
C
         RTAX = 0.0D0
         RTAY = 0.0D0
         RTAZ = 0.0D0
         RTGX = 0.0D0
         RTGY = 0.0D0
         RTGZ = 0.0D0
         HN   = 0.0D0
C
C ----- REPERAGE DE LA LIAISON
C
         LLIAC  = ZI(JLIAC+ILIAC-1)
         JDECAL = ZI(JAPPTR+LLIAC-1)
         NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
C
C ----- NUMERO DU NOEUD
C
         NUMNOE = ZI(JNUMLI+3*(LLIAC-1)+3-1)         
C
C ----- TANGENTES ET NORMALE
C        
         TAU1(1) = ZR(JTANGO+6*(LLIAC-1)+1-1)
         TAU1(2) = ZR(JTANGO+6*(LLIAC-1)+2-1)
         TAU1(3) = ZR(JTANGO+6*(LLIAC-1)+3-1)
         TAU2(1) = ZR(JTANGO+6*(LLIAC-1)+4-1)
         TAU2(2) = ZR(JTANGO+6*(LLIAC-1)+5-1)
         TAU2(3) = ZR(JTANGO+6*(LLIAC-1)+6-1) 
         CALL MMNORM(NDIMG,TAU1,TAU2,NORM,R8BID)   
C        
C --- CALCUL DES REACTIONS NORMALES DE CONTACT
C
         IF (LPENAC) THEN
           CALL CFRESA(NDIMG,
     &                 ZR(JAFMU+ZI(JAPDDL+JDECAL)-1),
     &                 NORM,
     &                 RNX,RNY,RNZ,RN)
         ELSE
           CALL CFRESA(NDIMG,
     &                 ZR(JATMU+ZI(JAPDDL+JDECAL)-1),
     &                 NORM,
     &                 RNX,RNY,RNZ,RN)
         ENDIF
C        
C --- CALCUL DES RESULTATS DU FROTTEMENT
C
         IF (LCTFD) THEN
C
C --- NOEUD SUPPOSE EN CONTACT GLISSANT
C
           VARC = 2.0D0
C
C --- CALCUL DES GLISSEMENTS
C
           CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                 ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL1)
           CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                 ZI(JAPDDL+JDECAL),ZR(JDDEPL),VAL2)
           GLIX = VAL1 + VAL2
           GLIY = 0.D0

           IF (NDIMG.EQ.3) THEN
             CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                   ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL1)
             CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                   ZI(JAPDDL+JDECAL),ZR(JDDEPL),VAL2)
             GLIY = VAL1 + VAL2
           ENDIF

           GLIT = SQRT(GLIX**2+GLIY**2)
C
C --- CALCUL DES FORCES TANGENTIELLES 
C
           IF (LPENAF) THEN
C 
C --- CAS DU FROTTEMENT PENALISE
C
C
C --- CALCUL DES FORCES TANGENTIELLES DE GLISSEMENT
C
              CALL CFRESB(NDIMG,LAG2D,'GL',
     &                    ZR(JAFMU+ZI(JAPDDL+JDECAL)-1),
     &                    ZR(JTANGO+6*(LLIAC-1)),
     &                    RTGX,RTGY,RTGZ)
C
C --- DETERMINATION GLISSEMENT/ADHERENCE
C
              TESTMU = ZR(JMU-1+3*NBLIAI+LLIAC)
              COEFPT = ZR(JTACF+ZTACF*(LLIAC-1)+2)
              TESTCF = SQRT(COEFPT)           
              IF (TESTCF.GT.R8MIEM()) THEN
                 IF (ABS((TESTMU-TESTCF)/TESTCF).GT.R8PREM()) THEN
                    VARC = 1.0D0
                    RTAX = RTGX
                    RTAY = RTGY
                    RTAZ = RTGZ
                    RTGX = 0.0D0
                    RTGY = 0.0D0
                    RTGZ = 0.0D0
                    HN   = 0.0D0
                 ELSE
                    VARC = 2.0D0
                    RTAX   = 0.0D0
                    RTAY   = 0.0D0
                    RTAZ   = 0.0D0
                    HN     = 0.0D0
                 ENDIF
              ENDIF
           ELSE 
C 
C --- CAS DU FROTTEMENT DUALISE 
C
              IF (ZK8(JTYPL-1+ILIAC).EQ.TYPEC0) THEN
                 DO 50 KK = ILIAC+1, BTOTAL

                    IF (ZI(JLIAC-1+KK).EQ.LLIAC) THEN
C
C --- NOEUD EN CONTACT ADHERENT
C
                      VARC   = 1.0D0

                      IF (LAG2D) THEN
                        TYPLIA = '  '                       
                      ELSE
                        TYPLIA = ZK8(JTYPL-1+KK)(1:2)
                      ENDIF
C
C --- CALCUL DES FORCES TANGENTIELLES D'ADHERENCE
C

                      CALL CFRESB(NDIMG,LAG2D,TYPLIA,
     &                            ZR(JATMU+ZI(JAPDDL+JDECAL)-1),
     &                            ZR(JTANGO+6*(LLIAC-1)),
     &                            RTAX,RTAY,RTAZ)
                      GOTO 100
                    ENDIF
 50              CONTINUE
C
C --- NOEUD EN CONTACT GLISSANT
C
                 VARC   = 2.0D0
C
C --- CALCUL DES FORCES TANGENTIELLES DE GLISSEMENT
C
                 CALL CFRESB(NDIMG,LAG2D,'GL',
     &                       ZR(JAFMU+ZI(JAPDDL+JDECAL)-1),
     &                       ZR(JTANGO+6*(LLIAC-1)),
     &                       RTGX,RTGY,RTGZ)

 100             CONTINUE
                 
              ELSE
C
C --- UNE LIAISON DE FROTTEMENT ADHERENT A DEJA ETE TRAITE
C --- DANS LA BOUCLE 50: ON NE RECOMMENCE PAS!
C
                 GOTO 150        

              ENDIF 
           ENDIF
         ENDIF
C
C --- CALCUL DES FORCES RESULTANTES DE CONTACT/FROTTEMENT
C
         RX     = RNX + RTAX + RTGX
         RY     = RNY + RTAY + RTGY
         RZ     = RNZ + RTAZ + RTGZ
         R      = SQRT(RX**2+RY**2+RZ**2)
C
C --- ECRITURE DANS LE CHAM_NO
C
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+1 ) = VARC      
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+3 ) = RN
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+4 ) = RNX
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+5 ) = RNY
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+6 ) = RNZ
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+16) = RX
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+17) = RY
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+18) = RZ
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+19) = R
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+20) = HN        
C
C ---- CALCUL DES PERCUSSIONS
C       
         IF (R .LE. EPS) THEN
         
           IMP  = 0.D0
           IMPX = 0.D0
           IMPY = 0.D0
           IMPZ = 0.D0
           ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+1) = 0.D0
           ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+2) = 0.D0
           ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+3) = 0.D0
           ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+4) = 0.D0
           
         ELSE
           
           IMP = ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+1) + R*DELTAT
           ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+1) = IMP
           
           IMPX = ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+2) + RX*DELTAT
           ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+2) = IMPX
           
           IMPY = ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+3) + RY*DELTAT
           ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+3) = IMPY
           
           IMPZ = ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+4) + RZ*DELTAT
           ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+4) = IMPZ
           
         ENDIF
C         
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+21)= IMP
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+22)= IMPX
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+23)= IMPY
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+24)= IMPZ            
C
C --- DONNEES DU FROTTEMENT
C
         IF (LCTFD) THEN
           ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+7 ) = GLIX
           ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+8 ) = GLIY
           ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+9 ) = GLIT
           ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+10) = RTAX
           ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+11) = RTAY
           ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+12) = RTAZ
           ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+13) = RTGX
           ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+14) = RTGY
           ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+15) = RTGZ
         ENDIF
C
C --- COORDONNEES DU POINT DE CONTACT
C
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+25)= 0.D0
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+26)= 0.D0
         ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+27)= 0.D0

 150  CONTINUE
  20  CONTINUE
C
C --- ECRITURE DES RELATIONS DE CONTACT A LA FIN DU PAS DE TEMPS
C
      IF (NIV.GE.2) THEN
         CALL CFIMP3(DEFICO,RESOCO,NOMA  ,IFM   ,NUMINS,
     &               INSTAP,NBLIAI,NBLIAC,JCNSVR)
      ENDIF
C
      CALL JEDEMA()
C ======================================================================
      END
