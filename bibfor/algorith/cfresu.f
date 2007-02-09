      SUBROUTINE CFRESU(NUMINS,INSTAP,MATASS,DEFICO,RESOCO,DEPDEL,
     &                  DDEPLA,NOMA,CNSINR)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2007   AUTEUR TORKHANI M.TORKHANI 
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
C TOLE CRP_20
      IMPLICIT     NONE
      INTEGER      NUMINS
      REAL*8       INSTAP
      CHARACTER*8  NOMA
      CHARACTER*19 MATASS
      CHARACTER*19 CNSINR
      CHARACTER*24 DEFICO
      CHARACTER*24 RESOCO
      CHARACTER*24 DEPDEL
      CHARACTER*24 DDEPLA
C
C ======================================================================
C ROUTINE APPELEE PAR : OP0070
C ======================================================================
C
C CREATION DU CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
C ECRITURE DANS LE FICHIER MESSAGE DES INFOS SUR LE CONTACT
C A CONVERGENCE DE NEWTON (FIN DU PAS DE TEMPS)
C
C IN  NUMINS : NUMERO DU PAS DE CHARGE
C IN  INSTAP : INSTANT DE CALCUL
C IN  MATASS : MATRICE DE RIGIDITE ASSEMBLE
C IN  DEFICO : SD DE DEFINITION DU CONTACT
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE 
C IN  DDEPLA : INCREMENT DE DEPLACEMENTS CALCULE EN IGNORANT LE CONTACT
C IN  NOMA   : NOM DU MAILLAGE
C OUT CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
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
      INTEGER      CFMMVD,CFDISI,ZAPPA,ZRESU
      INTEGER      IFM,NIV
      INTEGER      POSNOE,NUMNOE,II,KK,IBID
      INTEGER      NDIM,NBLIAI,NESCL,NBLIAC,NBDDL,NEQ
      INTEGER      BTOTAL,JDECAL,LLIAC
      INTEGER      LLF,LLF1,LLF2,NESMAX
      INTEGER      IMETH,IZONE,ICMP,ICONTA
      REAL*8       GLIX,GLIY,GLIT
      REAL*8       TESTMU,TESTCF
      REAL*8       VAL1,VAL2,VARC,R8PREM,R8MIEM
      REAL*8       R,RX,RY,RZ
      REAL*8       RN,RNX,RNY,RNZ,HN
      REAL*8       RTAX,RTAY,RTAZ,RTGX,RTGY,RTGZ
      CHARACTER*2  TYPEC0,TYPLIA
      CHARACTER*8  LICMPR(20),NOMCMP(20)
      CHARACTER*19 COCO,LIAC,ATMU,AFMU,MU,CONVEC
      INTEGER      JCOCO,JLIAC,JATMU,JAFMU,JMU,LMAT,JVECC
      CHARACTER*24 APPARI,NDIMCO,CONTNO,APPOIN
      INTEGER      JAPPAR,JDIM,JNOCO,JAPPTR
      CHARACTER*24 APDDL,APJEU,APCOFR,PENAL
      INTEGER      JAPDDL,JAPJEU,JAPCOF,JPENA
      CHARACTER*24 TANGCO,NORMCO
      INTEGER      JNORMO,JTANGO
      INTEGER      JCNSVR,JCNSLR,JDEPDE,JDDEPL
      INTEGER      TYPALC,TYPALF
      LOGICAL      LAG2D
C ----------------------------------------------------------------------
      DATA NOMCMP
     &   / 'CONT','JEU' ,'RN'  ,
     &     'RNX' ,'RNY' ,'RNZ' ,
     &     'GLIX','GLIY','GLI' ,
     &     'RTAX','RTAY','RTAZ',
     &     'RTGX','RTGY','RTGZ',
     &     'RX'  ,'RY'  ,'RZ'  ,
     &     'R'   ,'HN'/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C ======================================================================
C --- SORTIE SI PAS DE CONTACT 
C ======================================================================
      APPARI = RESOCO(1:14)//'.APPARI'
      CALL JEEXIN(APPARI,ICONTA)
       
      IF (ICONTA.EQ.0) THEN
        GOTO 999
      ENDIF

C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
      AFMU   = RESOCO(1:14)//'.AFMU'
      APCOFR = RESOCO(1:14)//'.APCOFR'
      APDDL  = RESOCO(1:14)//'.APDDL'
      APJEU  = RESOCO(1:14)//'.APJEU'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      ATMU   = RESOCO(1:14)//'.ATMU'
      COCO   = RESOCO(1:14)//'.COCO'
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CONVEC = RESOCO(1:14)//'.CONVEC'
      LIAC   = RESOCO(1:14)//'.LIAC'
      MU     = RESOCO(1:14)//'.MU'
      NDIMCO = DEFICO(1:16)//'.NDIMCO'
      NORMCO = RESOCO(1:14)//'.NORMCO'
      PENAL  = DEFICO(1:16)//'.PENAL'
      TANGCO = RESOCO(1:14)//'.TANGCO'
C ======================================================================
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(CONTNO,'L',JNOCO )
      CALL JEVEUO(APJEU, 'L',JAPJEU)

      CALL CFDISC(DEFICO,'              ',TYPALC,TYPALF,IBID,IBID)
      
      ZAPPA  = CFMMVD('ZAPPA')
      ZRESU  = CFMMVD('ZRESU')
      
C --- CHANGEZ LA TAILLE DE LICMPR,NOMCMP   
      IF (ZRESU.GT.20) CALL ASSERT(.FALSE.)
      

C ======================================================================
C
C --- CREATION DU CHAM_NO_S POUR LE CONTACT
C
C ======================================================================
      NESCL  = ZI(JAPPAR)
      NBLIAI = NESCL
      CNSINR = '&&RESUCO.CNSINR'

      DO 1 ICMP = 1,ZRESU
        LICMPR(ICMP) = NOMCMP(ICMP)
 1    CONTINUE

      CALL CNSCRE(NOMA,'INFC_R',ZRESU,LICMPR,'V',CNSINR)
      CALL JEVEUO(CNSINR//'.CNSV','E',JCNSVR)
      CALL JEVEUO(CNSINR//'.CNSL','E',JCNSLR)

      DO 10 II = 1,NBLIAI
         POSNOE = ZI(JAPPAR+ZAPPA*(II-1)+1)
         NUMNOE = ZI(JNOCO +POSNOE-1)
         DO 11 ICMP = 1,ZRESU
            ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+ICMP) = 0.0D0
            ZL(JCNSLR-1+ (NUMNOE-1)*ZRESU+ICMP) = .TRUE.
 11      CONTINUE
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+2 ) = ZR(JAPJEU+II-1)
 10   CONTINUE
C
C --- CONTACT SANS CALCUL: DETERMINATION DES INTERPENETRATIONS
C
      IF (TYPALC.EQ.5) THEN
        CALL CFSANS(DEFICO,RESOCO,NOMA,JCNSVR)
        GOTO 999
      ENDIF
C ======================================================================
      CALL JEVEUO(APDDL, 'L',JAPDDL)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(ATMU,  'L',JATMU )
      IF (TYPALF.NE.0) THEN      
        CALL JEVEUO(APCOFR,'L',JAPCOF)
        CALL JEVEUO(AFMU,  'L',JAFMU )
      ENDIF
C CAS DE LA METHODE PENALISEE: ON UTILISE AFMU
      IF (ABS(TYPALC).EQ.1) THEN
        CALL JEVEUO(AFMU,  'L',JAFMU )
      ENDIF
      CALL JEVEUO(COCO,  'L',JCOCO)
      CALL JEVEUO(CONVEC,'L',JVECC)
      CALL JEVEUO(DDEPLA(1:19)//'.VALE','L',JDDEPL)
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(LIAC,  'L',JLIAC)
      CALL JEVEUO(MU,    'L',JMU   )
      CALL JEVEUO(NDIMCO,'L',JDIM  )
      CALL JEVEUO(NORMCO,'L',JNORMO)
      CALL JEVEUO(PENAL, 'L',JPENA )
      CALL JEVEUO(TANGCO,'L',JTANGO)
C
C --- CARACTERISTIQUES DU CONTACT
C
      CALL CFDISD(JCOCO,
     &            NDIM,NBLIAC,LLF,LLF1,LLF2)

C
C --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE MECANIQUE
C --- (ADHERENCE A LA METHODE DE NEWTON)
C
      CALL JEVEUO(MATASS//'.&INT','L',LMAT)
C ======================================================================
C --- INITIALISATION DES VARIABLES 
C ======================================================================

      TYPEC0 = 'C0'
      NESMAX = ZI(JDIM+8)
      NEQ    = ZI(LMAT+2)
      IZONE  = 1
      IMETH  = CFDISI(DEFICO,'METHODE',IZONE)
      BTOTAL = NBLIAC + LLF + LLF1 + LLF2
      IF (IMETH.EQ.2) THEN
        LAG2D = .TRUE.
      ELSE
        LAG2D = .FALSE.
      ENDIF

C ======================================================================
C
C --- CALCUL DES REACTIONS ET FORCES
C
C ======================================================================

      DO 20 II = 1,BTOTAL
C
C --- NOEUD EN CONTACT
C
         VARC = 2.0D0
C
C --- INITIALISATIONS: CONTACT SANS FROTTEMENT
C
         RTAX = 0.0D0
         RTAY = 0.0D0
         RTAZ = 0.0D0
         RTGX = 0.0D0
         RTGY = 0.0D0
         RTGZ = 0.0D0        
C
C --- REPERAGE DE LA LIAISON
C
         LLIAC  = ZI(JLIAC+II-1)
         JDECAL = ZI(JAPPTR+LLIAC-1)
         NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
C        
C --- CALCUL DES REACTIONS NORMALES DE CONTACT
C
         IF (ABS(TYPALC).EQ.1) THEN
           CALL CFRESA(NDIM,
     &                 ZR(JAFMU+ZI(JAPDDL+JDECAL)-1),
     &                 ZR(JNORMO+3*(LLIAC-1)),
     &                 RNX,RNY,RNZ,RN)
         ELSE
           CALL CFRESA(NDIM,
     &                 ZR(JATMU+ZI(JAPDDL+JDECAL)-1),
     &                 ZR(JNORMO+3*(LLIAC-1)),
     &                 RNX,RNY,RNZ,RN)
         ENDIF
C        
C --- CALCUL DES RESULTATS DU FROTTEMENT
C
         IF (TYPALF.NE.0) THEN
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

           IF (NDIM.EQ.3) THEN
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
           IF (ABS(TYPALF).EQ.1) THEN
C 
C --- CAS DU FROTTEMENT PENALISE
C
C
C --- CALCUL DES FORCES TANGENTIELLES DE GLISSEMENT
C
              CALL CFRESB(NDIM,LAG2D,'GL',
     &                    ZR(JAFMU+ZI(JAPDDL+JDECAL)-1),
     &                    ZR(JTANGO+6*(LLIAC-1)),
     &                    RTGX,RTGY,RTGZ)
C
C --- DETERMINATION GLISSEMENT/ADHERENCE
C
              TESTMU = ZR(JMU-1+3*NBLIAI+LLIAC)
              TESTCF = SQRT(ZR(JPENA-1+2*LLIAC))
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
              IF (ZK8(JVECC-1+II).EQ.TYPEC0) THEN
                 DO 50 KK = II+1, BTOTAL

                    IF (ZI(JLIAC-1+KK).EQ.LLIAC) THEN
C
C --- NOEUD EN CONTACT ADHERENT
C
                      VARC   = 1.0D0

                      IF (LAG2D) THEN
                        TYPLIA = '  '                       
                      ELSE
                        TYPLIA = ZK8(JVECC-1+KK)
                      ENDIF
C
C --- CALCUL DES FORCES TANGENTIELLES D'ADHERENCE
C

                      CALL CFRESB(NDIM,LAG2D,TYPLIA,
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
                 CALL CFRESB(NDIM,LAG2D,'GL',
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
C --- NUMERO DU NOEUD
C
         POSNOE = ZI(JAPPAR+ZAPPA*(LLIAC-1)+1)
         NUMNOE = ZI(JNOCO +POSNOE-1)
C
C --- ECRITURE DANS LE CHAM_NO
C
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+1 ) = VARC
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+3 ) = RN
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+4 ) = RNX
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+5 ) = RNY
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+6 ) = RNZ
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+16) = RX
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+17) = RY
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+18) = RZ
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+19) = R
         ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+20) = HN
C
C --- DONNEES DU FROTTEMENT
C
         IF (TYPALF.NE.0) THEN
           ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+7 ) = GLIX
           ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+8 ) = GLIY
           ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+9 ) = GLIT
           ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+10) = RTAX
           ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+11) = RTAY
           ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+12) = RTAZ
           ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+13) = RTGX
           ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+14) = RTGY
           ZR(JCNSVR-1+ (NUMNOE-1)*ZRESU+15) = RTGZ
         ENDIF
 150  CONTINUE
  20  CONTINUE
C
C --- ECRITURE DES RELATIONS DE CONTACT A LA FIN DU PAS DE TEMPS
C
      IF (NIV.GE.2) THEN
         CALL CFIMP3(DEFICO,RESOCO,NOMA,IFM,NUMINS,INSTAP,NBLIAI,
     &               NBLIAC,JCNSVR)
      ENDIF

 999  CONTINUE
C ======================================================================
      CALL JEDEMA()
C ======================================================================
      END
