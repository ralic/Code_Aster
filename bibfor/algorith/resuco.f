      SUBROUTINE RESUCO(NUMORD,INSTAP,DEFICO,RESOCO,DEPDEL,
     &                  DDEPLA,NOMA,CNSINR,ITERAT)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/06/2004   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
      IMPLICIT     NONE
      INTEGER NUMORD,ITERAT
      REAL*8 INSTAP
      CHARACTER*8 NOMA
      CHARACTER*19 CNSINR
      CHARACTER*24 DEFICO,RESOCO,DEPDEL,DDEPLA
C ======================================================================
C COMMANDE STAT_NON_LINE / CONTACT UNILATERAL
C ECRITURE DANS LE FICHIER MESSAGE DES COUPLES NOEUD / MAILLE (OU NOEUD)
C EFFECTIVEMENT EN CONTACT A CONVERGENCE DE NEWTON (FIN DU PAS DE TEMPS)
C ======================================================================
C     IN   NUMORD : NUMERO DU PAS DE CHARGE
C     IN   INSTAP : INSTANT DE CALCUL
C     IN   DEFICO : SD DE DEFINITION DU CONTACT
C     IN   RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C     IN   NOMA   : NOM DU MAILLAGE
C     OUT  CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
C ======================================================================
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
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
C --------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------
C ======================================================================
      LOGICAL LBID
      INTEGER IFM,NIV,ICONTA,POS1,POS2,NUM1,NUM2,NDIM,II,JJ,KK
      INTEGER JAPPAR,JCOCO,JLIAC,NBLIAC,JNOCO,JMACO,LLIAC,NBLIAI,NESCL
      INTEGER JAPJEU,JDEPDE,JDDEPL,IPENA
      INTEGER IBID,JCNSVR,JCNSLR,JDIM
      INTEGER JATMU,JAFMU,JMU,JDECAL,JAPPTR,JAPCOE,JAPDDL,NBDDL,NEQ,LMAT
      INTEGER JAPJFY,JAPJFX,JAPCOF,JMETH,LLF,LLF1,LLF2,NESMAX
      INTEGER JNORMO,JTANGO,JVECC
      REAL*8 AJEUFX,AJEUFY,AJEUFT,RN,R,COE,TESTMU,TESTCF,PROD
      REAL*8 VAL1,VAL2,VARC,R8BID,R8PREM,R8MIEM,PROJ,PROJ1,PROJ2
      REAL*8 RNX,RNY,RNZ,RX,RY,RZ,RTAX,RTAY,RTAZ,RTGX,RTGY,RTGZ
      CHARACTER*2  TYPEC0, TYPEF0, TYPEF1, TYPEF2
      CHARACTER*7  CHAIN
      CHARACTER*8  NOM1,NOM2,LICMPR(19),KVEC,FTMP

      CHARACTER*19 COCO,LIAC,ATMU,AFMU,MU,MATASS,CONVEC

      CHARACTER*24 APPARI,NDIMCO,METHCO,CONTNO,CONTMA,APPOIN,APCOEF
      CHARACTER*24 APDDL,APJEU,APCOFR,APJEFX,APJEFY,PENAL
      CHARACTER*24 TANGCO,NORMCO
      INTEGER      TYPALC,TYPALF,FROT3D,MATTAN
C ======================================================================
      CNSINR = '&&RESUCO.CNSINR'
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      TYPEF1 = 'F1'
      TYPEF2 = 'F2'
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C --- RECUPERATION STRUCTURE DE DONNEES DE CONTACT

C --- STRUCTURE DE DONNEES DE CONTACT
C --- TRAITEMENT DU CONTACT : NOUVELLE VERSION (NMCONT)

      APPARI = RESOCO(1:14)//'.APPARI'
      CALL JEEXIN(APPARI,ICONTA)
      IF (ICONTA.NE.0) THEN

C ------ RECUPERATION DU DESCRIPTEUR DE LA MATRICE MECANIQUE
C        (ADHERENCE A LA METHODE DE NEWTON)
C --- LA VARIABLE ITERAT DE NMMATR EST MISE A 1 POUR
C --- FORCER LA RECHERCHE DU NOM DE LA MATRASS

        CALL NMMAFR(ITERAT,RESOCO(1:14),DEFICO,MATASS)

C --- CARACTERISTIQUES DU CONTACT
        CALL CFDISC(DEFICO,RESOCO(1:14),TYPALC,TYPALF,FROT3D,MATTAN)

        CALL JEVEUO(MATASS//'.&INT','L',LMAT)
        CALL JEVEUO(APPARI,'L',JAPPAR)
        COCO = RESOCO(1:14)//'.COCO'
        CALL JEVEUO(COCO,'L',JCOCO)
        LIAC = RESOCO(1:14)//'.LIAC'
        CALL JEVEUO(LIAC,'L',JLIAC)
        CONVEC = RESOCO(1:14)//'.CONVEC'
        CALL JEVEUO(CONVEC,'L',JVECC)

        NEQ    = ZI(LMAT+2)
        NESCL  = ZI(JAPPAR)
        NBLIAI = NESCL


        CALL CFDISD(JCOCO,
     &              NDIM,IBID,NBLIAC,IBID,IBID,
     &              LLF,LLF1,LLF2)


        CONTNO = DEFICO(1:16)//'.NOEUCO'
        CONTMA = DEFICO(1:16)//'.MAILCO'
        METHCO = DEFICO(1:16)//'.METHCO'
        NDIMCO = DEFICO(1:16)//'.NDIMCO'
        PENAL  = DEFICO(1:16)//'.PENAL'
        APPOIN = RESOCO(1:14)//'.APPOIN'
        APCOEF = RESOCO(1:14)//'.APCOEF'
        APCOFR = RESOCO(1:14)//'.APCOFR'
        NORMCO = RESOCO(1:14)//'.NORMCO'
        TANGCO = RESOCO(1:14)//'.TANGCO'
        APDDL  = RESOCO(1:14)//'.APDDL'
        APJEU  = RESOCO(1:14)//'.APJEU'
        APJEFX = RESOCO(1:14)//'.APJEFX'
        APJEFY = RESOCO(1:14)//'.APJEFY'
        ATMU   = RESOCO(1:14)//'.ATMU'
        AFMU   = RESOCO(1:14)//'.AFMU'
        MU     = RESOCO(1:14)//'.MU'

        CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
        CALL JEVEUO(DDEPLA(1:19)//'.VALE','L',JDDEPL)
        CALL JEVEUO(CONTNO,'L',JNOCO )
        CALL JEVEUO(CONTMA,'L',JMACO )
        CALL JEVEUO(METHCO,'L',JMETH )
        CALL JEVEUO(APPOIN,'L',JAPPTR)
        CALL JEVEUO(APCOEF,'L',JAPCOE)
        CALL JEVEUO(APDDL, 'L',JAPDDL)
        CALL JEVEUO(APJEU, 'L',JAPJEU)

        IF (TYPALF.NE.0) THEN
          
          CALL JEVEUO(APJEFX,'L',JAPJFX)
          CALL JEVEUO(APJEFY,'L',JAPJFY)
          CALL JEVEUO(APCOFR,'L',JAPCOF)
          CALL JEVEUO(AFMU,  'L',JAFMU )
        ENDIF
C CAS DE LA METHODE PENALISEE: ON UTILISE AFMU
        IF (ABS(TYPALC).EQ.1) THEN
          CALL JEVEUO(AFMU,  'L',JAFMU )
        ENDIF

        CALL JEVEUO(NORMCO,'L',JNORMO)
        CALL JEVEUO(TANGCO,'L',JTANGO)
        CALL JEVEUO(ATMU,  'L',JATMU )

        CALL JEVEUO(MU,    'L',JMU   )
        CALL JEVEUO(NDIMCO,'L',JDIM  )
        CALL JEVEUO(PENAL, 'L',IPENA )

        NESMAX = ZI(JDIM+8)

C --- ECRITURE DES RELATIONS DE CONTACT A LA FIN DU PAS DE TEMPS

        IF (NIV.EQ.2) THEN
          WRITE (IFM,*)
          WRITE (IFM,'(''<CONTACT_2> '',10X,153(''*''))')
          WRITE (IFM,'(''<CONTACT_2> '',10X,''*'',11X,A28,112X,''*'')')
     &      'LISTE DES COUPLES EN CONTACT'
          WRITE (IFM,
     &    '(''<CONTACT_2> '',10X,''*'',15X,A14,I6,116X,          ''*'')'
     &      ) 'NUMERO D''ORDRE',NUMORD
          WRITE (IFM,
     &'(''<CONTACT_2> '',10X,''*'',15X,A7,3X,1PE12.5,
     &  114X,''*'')') 'INSTANT',INSTAP
          WRITE (IFM,'(''<CONTACT_2> '',10X,153(''*''))')

          IF (NBLIAC.EQ.0) WRITE (IFM,
     &'(''<CONTACT_2> '',10X,''*'',
     &                               18X,
     &A14,135X,''*'')') 'PAS DE CONTACT'
        END IF

C ---- RECUPERATION DES DONNEES POUR CHAQUE LIAISON
C ---- PREPARATION POUR L'ARCHIVAGE
C --- CREATION DU CHAM_NO_S POUR LE CONTACT
        LICMPR(1)  = 'CONT'
        LICMPR(2)  = 'JEU'
        LICMPR(3)  = 'RN'
        LICMPR(4)  = 'RNX'
        LICMPR(5)  = 'RNY'
        LICMPR(6)  = 'RNZ'
        LICMPR(7)  = 'GLIX'
        LICMPR(8)  = 'GLIY'
        LICMPR(9)  = 'GLI'
        LICMPR(10) = 'RTAX'
        LICMPR(11) = 'RTAY'
        LICMPR(12) = 'RTAZ'
        LICMPR(13) = 'RTGX'
        LICMPR(14) = 'RTGY'
        LICMPR(15) = 'RTGZ'
        LICMPR(16) = 'RX'
        LICMPR(17) = 'RY'
        LICMPR(18) = 'RZ'
        LICMPR(19) = 'R'

C --- CREATION DU CHAM_NO_S POUR LE CONTACT
        CALL CNSCRE(NOMA,'INFC_R',19,LICMPR,'V',CNSINR)
        CALL JEVEUO(CNSINR//'.CNSV','E',JCNSVR)
        CALL JEVEUO(CNSINR//'.CNSL','E',JCNSLR)

        DO 10 II = 1,NBLIAI
           POS1 = ZI(JAPPAR+3*(II-1)+1)
           NUM1 = ZI(JNOCO +POS1    -1)
           ZR(JCNSVR-1+ (NUM1-1)*19+1 ) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+2 ) = ZR(JAPJEU+II-1)
           ZR(JCNSVR-1+ (NUM1-1)*19+3 ) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+4 ) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+5 ) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+6 ) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+7 ) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+8 ) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+9 ) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+10) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+11) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+12) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+13) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+14) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+15) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+16) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+17) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+18) = 0.0D0
           ZR(JCNSVR-1+ (NUM1-1)*19+19) = 0.0D0
 10     CONTINUE
C
        IF ( ZI(JMETH+6).EQ.-1 ) THEN
C ======================================================================
C -- CAS DU CONTACT PENALISE -------------------------------------------
C ======================================================================
           DO 20 II = 1, NBLIAC
              LLIAC  = ZI(JLIAC+II-1)
              JDECAL = ZI(JAPPTR+LLIAC-1)
              NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)

              PROJ   = ZR(JAFMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JNORMO +3*(LLIAC-1))
     &               + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+1)
              RNZ = 0.0D0
              IF (NDIM.EQ.3) THEN
                 PROJ = PROJ
     &                + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+2)
                 RNZ = PROJ*ZR(JNORMO +3*(LLIAC-1)+2)
              ENDIF
              RNX = PROJ*ZR(JNORMO +3*(LLIAC-1))
              RNY = PROJ*ZR(JNORMO +3*(LLIAC-1)+1)
              RN     = SQRT(RNX**2+RNY**2+RNZ**2)
              VARC   = 2.0D0
              RX     = RNX
              RY     = RNY
              RZ     = RNZ
              R      = SQRT(RX**2+RY**2+RZ**2)

              POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
              NUM1 = ZI(JNOCO +POS1    -1)
              ZR(JCNSVR-1+ (NUM1-1)*19+1)  = VARC
              ZR(JCNSVR-1+ (NUM1-1)*19+3)  = RN
              ZR(JCNSVR-1+ (NUM1-1)*19+4)  = RNX
              ZR(JCNSVR-1+ (NUM1-1)*19+5)  = RNY
              ZR(JCNSVR-1+ (NUM1-1)*19+6)  = RNZ
              ZR(JCNSVR-1+ (NUM1-1)*19+16) = RX
              ZR(JCNSVR-1+ (NUM1-1)*19+17) = RY
              ZR(JCNSVR-1+ (NUM1-1)*19+18) = RZ
              ZR(JCNSVR-1+ (NUM1-1)*19+19) = R
 20        CONTINUE
        ELSE IF ( ZI(JMETH+6).EQ.0 .OR.
     +            ZI(JMETH+6).EQ.1      ) THEN
C ======================================================================
C -- CAS DU CONTACT EN CONTRAINTES ACTIVES OU LAGRANGIEN ---------------
C ======================================================================
           DO 30 II = 1, NBLIAC
              LLIAC  = ZI(JLIAC+II-1)
              JDECAL = ZI(JAPPTR+LLIAC-1)
              NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)

              PROJ   = ZR(JATMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JNORMO +3*(LLIAC-1))
     &               + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+1)
              RNZ = 0.0D0
              IF (NDIM.EQ.3) THEN
                 PROJ = PROJ
     &                + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+2)
                 RNZ  = PROJ * ZR(JNORMO +3*(LLIAC-1)+2)
              ENDIF
              RNX = PROJ*ZR(JNORMO +3*(LLIAC-1))
              RNY = PROJ*ZR(JNORMO +3*(LLIAC-1)+1)
              RN     = SQRT(RNX**2+RNY**2+RNZ**2)
              VARC   = 2.0D0
              RX     = RNX
              RY     = RNY
              RZ     = RNZ
              R      = SQRT(RX**2+RY**2+RZ**2)
              POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
              NUM1 = ZI(JNOCO +POS1    -1)
              ZR(JCNSVR-1+ (NUM1-1)*19+1)  = VARC
              ZR(JCNSVR-1+ (NUM1-1)*19+3)  = RN
              ZR(JCNSVR-1+ (NUM1-1)*19+4)  = RNX
              ZR(JCNSVR-1+ (NUM1-1)*19+5)  = RNY
              ZR(JCNSVR-1+ (NUM1-1)*19+6)  = RNZ
              ZR(JCNSVR-1+ (NUM1-1)*19+16) = RX
              ZR(JCNSVR-1+ (NUM1-1)*19+17) = RY
              ZR(JCNSVR-1+ (NUM1-1)*19+18) = RZ
              ZR(JCNSVR-1+ (NUM1-1)*19+19) = R
 30        CONTINUE
        ELSE IF ( ZI(JMETH+6).EQ.2 ) THEN
C ======================================================================
C -- CAS DU FROTTEMENT LAGRANGIEN EN DIMENSION 2 -----------------------
C ======================================================================
           DO 40 II = 1, NBLIAC + LLF
              IF (ZK8(JVECC-1+II).EQ.TYPEC0) THEN
                 LLIAC  = ZI(JLIAC+II-1)
                 JDECAL = ZI(JAPPTR+LLIAC-1)
                 NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
                 PROJ   = ZR(JATMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JNORMO +3*(LLIAC-1)  )
     &                  + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+1)
                 RNX    = PROJ * ZR(JNORMO +3*(LLIAC-1)  )
                 RNY    = PROJ * ZR(JNORMO +3*(LLIAC-1)+1)
                 RN     = SQRT(RNX**2+RNY**2)
                 CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                               ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL1)
                 CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                               ZI(JAPDDL+JDECAL),ZR(JDDEPL),VAL2)
                 AJEUFX = VAL1 + VAL2
                 DO 50 KK = II+1, NBLIAC+LLF
                    IF (ZI(JLIAC-1+KK).EQ.LLIAC) THEN
C ======================================================================
C --- ADHERENCE --------------------------------------------------------
C ======================================================================
                       VARC   = 1.0D0
                       PROJ   = ZR(JATMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)  )
     &                        + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+1)
                       RTAX   = PROJ * ZR(JTANGO +6*(LLIAC-1)  )
                       RTAY   = PROJ * ZR(JTANGO +6*(LLIAC-1)+1)
                       RTGX   = 0.0D0
                       RTGY   = 0.0D0
                       GOTO 100
                    ENDIF
 50              CONTINUE
C ======================================================================
C --- GLISSEMENT -------------------------------------------------------
C ======================================================================
                 VARC   = 2.0D0
                 PROJ   = ZR(JAFMU+ZI(JAPDDL+JDECAL)-1)
     &                                       * ZR(JTANGO +6*(LLIAC-1)  )
     &                  + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+1)
                 RTAX   = 0.0D0
                 RTAY   = 0.0D0
                 RTGX   = PROJ * ZR(JTANGO +6*(LLIAC-1)  )
                 RTGY   = PROJ * ZR(JTANGO +6*(LLIAC-1)+1)
 100             CONTINUE
                 RX     = RNX + RTAX + RTGX
                 RY     = RNY + RTAY + RTGY
                 R      = SQRT(RX**2+RY**2)
                 POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
                 NUM1 = ZI(JNOCO +POS1    -1)
                 ZR(JCNSVR-1+ (NUM1-1)*19+1 ) = VARC
                 ZR(JCNSVR-1+ (NUM1-1)*19+3 ) = RN
                 ZR(JCNSVR-1+ (NUM1-1)*19+4 ) = RNX
                 ZR(JCNSVR-1+ (NUM1-1)*19+5 ) = RNY
                 ZR(JCNSVR-1+ (NUM1-1)*19+7 ) = AJEUFX
                 ZR(JCNSVR-1+ (NUM1-1)*19+10) = RTAX
                 ZR(JCNSVR-1+ (NUM1-1)*19+11) = RTAY
                 ZR(JCNSVR-1+ (NUM1-1)*19+13) = RTGX
                 ZR(JCNSVR-1+ (NUM1-1)*19+14) = RTGY
                 ZR(JCNSVR-1+ (NUM1-1)*19+16) = RX
                 ZR(JCNSVR-1+ (NUM1-1)*19+17) = RY
                 ZR(JCNSVR-1+ (NUM1-1)*19+19) = R
              ENDIF
 40        CONTINUE
        ELSE IF ( ZI(JMETH+6).EQ.3 ) THEN
C ======================================================================
C -- CAS DU FROTTEMENT PENALISE ----------------------------------------
C ======================================================================
           DO 60 II = 1, NBLIAC
              VARC   = 2.0D0
              LLIAC  = ZI(JLIAC+II-1)
              JDECAL = ZI(JAPPTR+LLIAC-1)
              NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
              PROJ   = ZR(JATMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JNORMO +3*(LLIAC-1))
     &               + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+1)
              RNZ    = 0.0D0
              RTGZ   = 0.0D0
              AJEUFY = 0.0D0
              IF (NDIM.EQ.3) THEN
                 PROJ = PROJ
     &                + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+2)
                 RNZ  = PROJ * ZR(JNORMO +3*(LLIAC-1)+2)
                 CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                               ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL1)
                 CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                               ZI(JAPDDL+JDECAL),ZR(JDDEPL),VAL2)
                 AJEUFY = VAL1 + VAL2
              ENDIF
              RNX    = PROJ * ZR(JNORMO +3*(LLIAC-1)  )
              RNY    = PROJ * ZR(JNORMO +3*(LLIAC-1)+1)
              RN     = SQRT(RNX**2+RNY**2+RNZ**2)
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                               ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL1)
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                               ZI(JAPDDL+JDECAL),ZR(JDDEPL),VAL2)
              AJEUFX = VAL1 + VAL2
              AJEUFT = SQRT(AJEUFX**2+AJEUFY**2)
              PROJ1  = ZR(JAFMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)  )
     &               + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+1)
              PROJ2  = ZR(JAFMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+3)
     &               + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+4)
              IF (NDIM.EQ.3) THEN
                 PROJ1 = PROJ1
     &               + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+2)
                 PROJ2 = PROJ2
     &               + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+5)
                 RTGZ  = PROJ1 * ZR(JTANGO +6*(LLIAC-1)+2) +
     &                   PROJ2 * ZR(JTANGO +6*(LLIAC-1)+5)
              ENDIF
              RTGX   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)  ) +
     &                 PROJ2 * ZR(JTANGO +6*(LLIAC-1)+3)
              RTGY   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)+1) +
     &                 PROJ2 * ZR(JTANGO +6*(LLIAC-1)+4)
              RTAX   = 0.0D0
              RTAY   = 0.0D0
              RTAZ   = 0.0D0
              TESTMU = ZR(JMU-1+3*NBLIAI+LLIAC)
              TESTCF = SQRT(ZR(IPENA-1+2*LLIAC))
              IF (TESTCF.GT.R8MIEM()) THEN
                 IF (ABS((TESTMU-TESTCF)/TESTCF).GT.R8PREM()) THEN
                    VARC = 1.0D0
                    RTAX = RTGX
                    RTAY = RTGY
                    RTAZ = RTGZ
                    RTGX = 0.0D0
                    RTGY = 0.0D0
                    RTGZ = 0.0D0
                 ENDIF
              ENDIF
              RX     = RNX + RTAX + RTGX
              RY     = RNY + RTAY + RTGY
              RZ     = RNZ + RTAZ + RTGZ
              R      = SQRT(RX**2+RY**2+RZ**2)

              POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
              NUM1 = ZI(JNOCO +POS1    -1)
              ZR(JCNSVR-1+ (NUM1-1)*19+1 ) = VARC
              ZR(JCNSVR-1+ (NUM1-1)*19+3 ) = RN
              ZR(JCNSVR-1+ (NUM1-1)*19+4 ) = RNX
              ZR(JCNSVR-1+ (NUM1-1)*19+5 ) = RNY
              ZR(JCNSVR-1+ (NUM1-1)*19+6 ) = RNZ
              ZR(JCNSVR-1+ (NUM1-1)*19+7 ) = AJEUFX
              ZR(JCNSVR-1+ (NUM1-1)*19+8 ) = AJEUFY
              ZR(JCNSVR-1+ (NUM1-1)*19+9 ) = AJEUFT
              ZR(JCNSVR-1+ (NUM1-1)*19+10) = RTAX
              ZR(JCNSVR-1+ (NUM1-1)*19+11) = RTAY
              ZR(JCNSVR-1+ (NUM1-1)*19+12) = RTAZ
              ZR(JCNSVR-1+ (NUM1-1)*19+13) = RTGX
              ZR(JCNSVR-1+ (NUM1-1)*19+14) = RTGY
              ZR(JCNSVR-1+ (NUM1-1)*19+15) = RTGZ
              ZR(JCNSVR-1+ (NUM1-1)*19+16) = RX
              ZR(JCNSVR-1+ (NUM1-1)*19+17) = RY
              ZR(JCNSVR-1+ (NUM1-1)*19+18) = RZ
              ZR(JCNSVR-1+ (NUM1-1)*19+19) = R

 60        CONTINUE
        ELSE IF ( ZI(JMETH+6).EQ.4 ) THEN
C ======================================================================
C -- CAS DU FROTTEMENT LAGRANGIEN EN DIMENSION 3 -----------------------
C ======================================================================
           DO 80 II = 1, NBLIAC+LLF+LLF1+LLF2
              IF ( ZK8(JVECC-1+II).EQ.TYPEC0 ) THEN
                 LLIAC  = ZI(JLIAC+II-1)
                 JDECAL = ZI(JAPPTR+LLIAC-1)
                 NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
                 PROJ   = ZR(JATMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JNORMO +3*(LLIAC-1))
     &                  + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+1)
     &                  + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+2)
                 RNX    = PROJ * ZR(JNORMO +3*(LLIAC-1)  )
                 RNY    = PROJ * ZR(JNORMO +3*(LLIAC-1)+1)
                 RNZ    = PROJ * ZR(JNORMO +3*(LLIAC-1)+2)
                 RN     = SQRT(RNX**2+RNY**2+RNZ**2)
                 CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                               ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL1)
                 CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                               ZI(JAPDDL+JDECAL),ZR(JDDEPL),VAL2)
                 AJEUFX = VAL1 + VAL2
                 CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                               ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL1)
                 CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                               ZI(JAPDDL+JDECAL),ZR(JDDEPL),VAL2)
                 AJEUFY = VAL1 + VAL2
                 AJEUFT = SQRT(AJEUFX**2+AJEUFY**2)
                 DO 90 KK = II+1, NBLIAC+LLF+LLF1+LLF2
                    IF (ZI(JLIAC-1+KK).EQ.LLIAC) THEN
                       IF (ZK8(JVECC-1+KK).EQ.TYPEF0) THEN
C ======================================================================
C --- ADHERENCE SUIVANT LES DEUX DIRECTIONS ----------------------------
C ======================================================================
                          VARC   = 1.0D0
                          PROJ1  = ZR(JATMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)  )
     &                           + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+1)
     &                           + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+2)
                          PROJ2  = ZR(JATMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+3)
     &                           + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+4)
     &                           + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+5)
                          RTAX   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)  ) +
     &                             PROJ2 * ZR(JTANGO +6*(LLIAC-1)+3)
                          RTAY   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)+1) +
     &                             PROJ2 * ZR(JTANGO +6*(LLIAC-1)+4)
                          RTAZ   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)+2) +
     &                             PROJ2 * ZR(JTANGO +6*(LLIAC-1)+5)
                          RTGX   = 0.0D0
                          RTGY   = 0.0D0
                          RTGZ   = 0.0D0
                       ELSE IF (ZK8(JVECC-1+KK).EQ.TYPEF1) THEN
C ======================================================================
C --- ADHERENCE SUIVANT LA PREMIERE DIRECTION --------------------------
C ======================================================================
                          VARC   = 1.0D0
                          PROJ1  = ZR(JATMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)  )
     &                           + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+1)
     &                           + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+2)
                          RTAX   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)  )
                          RTAY   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)+1)
                          RTAZ   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)+2)
                          RTGX   = 0.0D0
                          RTGY   = 0.0D0
                          RTGZ   = 0.0D0
                       ELSE IF (ZK8(JVECC-1+KK).EQ.TYPEF2) THEN
C ======================================================================
C --- ADHERENCE SUIVANT LA SECONDE DIRECTION ---------------------------
C ======================================================================
                          VARC   = 1.0D0
                          PROJ2  = ZR(JATMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+3)
     &                           + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+4)
     &                           + ZR(JATMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+5)
                          RTAX   = PROJ2 * ZR(JTANGO +6*(LLIAC-1)+3)
                          RTAY   = PROJ2 * ZR(JTANGO +6*(LLIAC-1)+4)
                          RTAZ   = PROJ2 * ZR(JTANGO +6*(LLIAC-1)+5)
                          RTGX   = 0.0D0
                          RTGY   = 0.0D0
                          RTGZ   = 0.0D0
                       ENDIF
                       GOTO 300
                    ENDIF
 90              CONTINUE
C ======================================================================
C --- GLISSEMENT -------------------------------------------------------
C ======================================================================
                 VARC   = 2.0D0
                 RTAX   = 0.0D0
                 RTAY   = 0.0D0
                 RTAZ   = 0.0D0
                 PROJ1  = ZR(JAFMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)  )
     &                  + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+1)
     &                  + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+2)
                 PROJ2  = ZR(JAFMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+3)
     &                  + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+4)
     &                  + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+5)
                 RTGX   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)  ) +
     &                    PROJ2 * ZR(JTANGO +6*(LLIAC-1)+3)
                 RTGY   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)+1) +
     &                    PROJ2 * ZR(JTANGO +6*(LLIAC-1)+4)
                 RTGZ   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)+2) +
     &                    PROJ2 * ZR(JTANGO +6*(LLIAC-1)+5)
 300             CONTINUE
                 RX     = RNX + RTAX + RTGX
                 RY     = RNY + RTAY + RTGY
                 RZ     = RNZ + RTAZ + RTGZ
                 R      = SQRT(RX**2+RY**2+RZ**2)

                 POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
                 NUM1 = ZI(JNOCO +POS1    -1)
                 ZR(JCNSVR-1+ (NUM1-1)*19+1 ) = VARC
                 ZR(JCNSVR-1+ (NUM1-1)*19+3 ) = RN
                 ZR(JCNSVR-1+ (NUM1-1)*19+4 ) = RNX
                 ZR(JCNSVR-1+ (NUM1-1)*19+5 ) = RNY
                 ZR(JCNSVR-1+ (NUM1-1)*19+6 ) = RNZ
                 ZR(JCNSVR-1+ (NUM1-1)*19+7 ) = AJEUFX
                 ZR(JCNSVR-1+ (NUM1-1)*19+8 ) = AJEUFY
                 ZR(JCNSVR-1+ (NUM1-1)*19+9 ) = AJEUFT
                 ZR(JCNSVR-1+ (NUM1-1)*19+10) = RTAX
                 ZR(JCNSVR-1+ (NUM1-1)*19+11) = RTAY
                 ZR(JCNSVR-1+ (NUM1-1)*19+12) = RTAZ
                 ZR(JCNSVR-1+ (NUM1-1)*19+13) = RTGX
                 ZR(JCNSVR-1+ (NUM1-1)*19+14) = RTGY
                 ZR(JCNSVR-1+ (NUM1-1)*19+15) = RTGZ
                 ZR(JCNSVR-1+ (NUM1-1)*19+16) = RX
                 ZR(JCNSVR-1+ (NUM1-1)*19+17) = RY
                 ZR(JCNSVR-1+ (NUM1-1)*19+18) = RZ
                 ZR(JCNSVR-1+ (NUM1-1)*19+19) = R

              ENDIF
 80        CONTINUE
        ELSE IF ( ZI(JMETH+6).EQ.5 ) THEN
C ======================================================================
C -- CAS DU FROTTEMENT PENALISE EN CONTACT -----------------------------
C ======================================================================
           DO 110 II = 1, NBLIAC
              VARC   = 2.0D0
              LLIAC  = ZI(JLIAC+II-1)
              JDECAL = ZI(JAPPTR+LLIAC-1)
              NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
              PROJ   = ZR(JAFMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JNORMO +3*(LLIAC-1))
     &               + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+1)
              RNZ    = 0.0D0
              RTGZ   = 0.0D0
              AJEUFY = 0.0D0
              IF (NDIM.EQ.3) THEN
                 PROJ = PROJ
     &                + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JNORMO +3*(LLIAC-1)+2)
                 RNZ  = PROJ * ZR(JNORMO +3*(LLIAC-1)+2)
                 CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                               ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL1)
                 CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                               ZI(JAPDDL+JDECAL),ZR(JDDEPL),VAL2)
                 AJEUFY = VAL1 + VAL2
              ENDIF
              RNX    = PROJ * ZR(JNORMO +3*(LLIAC-1)  )
              RNY    = PROJ * ZR(JNORMO +3*(LLIAC-1)+1)
              RN     = SQRT(RNX**2+RNY**2+RNZ**2)
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                               ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL1)
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                               ZI(JAPDDL+JDECAL),ZR(JDDEPL),VAL2)
              AJEUFX = VAL1 + VAL2
              AJEUFT = SQRT(AJEUFX**2+AJEUFY**2)
              PROJ1  = ZR(JAFMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)  )
     &               + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+1)
              PROJ2  = ZR(JAFMU+ZI(JAPDDL+JDECAL)-1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+3)
     &               + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+1)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+4)
              IF (NDIM.EQ.3) THEN
                 PROJ1 = PROJ1
     &               + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+2)
                 PROJ2 = PROJ2
     &               + ZR(JAFMU+ZI(JAPDDL+JDECAL)-1+2)
     &                                      * ZR(JTANGO +6*(LLIAC-1)+5)
                 RTGZ  = PROJ1 * ZR(JTANGO +6*(LLIAC-1)+2) +
     &                   PROJ2 * ZR(JTANGO +6*(LLIAC-1)+5)
              ENDIF
              RTGX   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)  ) +
     &                 PROJ2 * ZR(JTANGO +6*(LLIAC-1)+3)
              RTGY   = PROJ1 * ZR(JTANGO +6*(LLIAC-1)+1) +
     &                 PROJ2 * ZR(JTANGO +6*(LLIAC-1)+4)
              RTAX   = 0.0D0
              RTAY   = 0.0D0
              RTAZ   = 0.0D0
              TESTMU = ZR(JMU-1+3*NBLIAI+LLIAC)
              TESTCF = SQRT(ZR(IPENA-1+2*LLIAC))
              IF (TESTCF.GT.R8MIEM()) THEN
                 IF (ABS((TESTMU-TESTCF)/TESTCF).GT.R8PREM()) THEN
                    VARC = 1.0D0
                    RTAX = RTGX
                    RTAY = RTGY
                    RTAZ = RTGZ
                    RTGX = 0.0D0
                    RTGY = 0.0D0
                    RTGZ = 0.0D0
                 ENDIF
              ENDIF
              RX     = RNX + RTAX + RTGX
              RY     = RNY + RTAY + RTGY
              RZ     = RNZ + RTAZ + RTGZ
              R      = SQRT(RX**2+RY**2+RZ**2)

              POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
              NUM1 = ZI(JNOCO +POS1    -1)
              ZR(JCNSVR-1+ (NUM1-1)*19+1 ) = VARC
              ZR(JCNSVR-1+ (NUM1-1)*19+3 ) = RN
              ZR(JCNSVR-1+ (NUM1-1)*19+4 ) = RNX
              ZR(JCNSVR-1+ (NUM1-1)*19+5 ) = RNY
              ZR(JCNSVR-1+ (NUM1-1)*19+6 ) = RNZ
              ZR(JCNSVR-1+ (NUM1-1)*19+7 ) = AJEUFX
              ZR(JCNSVR-1+ (NUM1-1)*19+8 ) = AJEUFY
              ZR(JCNSVR-1+ (NUM1-1)*19+9 ) = AJEUFT
              ZR(JCNSVR-1+ (NUM1-1)*19+10) = RTAX
              ZR(JCNSVR-1+ (NUM1-1)*19+11) = RTAY
              ZR(JCNSVR-1+ (NUM1-1)*19+12) = RTAZ
              ZR(JCNSVR-1+ (NUM1-1)*19+13) = RTGX
              ZR(JCNSVR-1+ (NUM1-1)*19+14) = RTGY
              ZR(JCNSVR-1+ (NUM1-1)*19+15) = RTGZ
              ZR(JCNSVR-1+ (NUM1-1)*19+16) = RX
              ZR(JCNSVR-1+ (NUM1-1)*19+17) = RY
              ZR(JCNSVR-1+ (NUM1-1)*19+18) = RZ
              ZR(JCNSVR-1+ (NUM1-1)*19+19) = R

 110       CONTINUE
        ENDIF

        DO 120 II = 1,NBLIAI

C ----- RECHERCHE CONNECTIVITE
           POS1 = ZI(JAPPAR+3* (II-1)+1)
           NUM1 = ZI(JNOCO+POS1-1)
           CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
           POS2 = ZI(JAPPAR+3* (II-1)+2)
           IF (POS2.GT.0) THEN
              CHAIN = 'MAILLE '
              NUM2 = ZI(JMACO+POS2-1)
              CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUM2),NOM2)
           ELSE IF (POS2.LT.0) THEN
              CHAIN = 'NOEUD '
              NUM2 = ZI(JNOCO+ABS(POS2)-1)
              CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM2),NOM2)
           ELSE IF (POS2.EQ.0) THEN
              CHAIN = ' '
              NOM2 = ' '
           ENDIF
C ----- FIN RECHERCHE CONNECTIVITE
           RN = ZR(JCNSVR-1+ (NUM1-1)*19+3 )
           R  = ZR(JCNSVR-1+ (NUM1-1)*19+19)

C RAPPORT REACTION_TANGENTIELLE/REACTION_NORMALE           

           COE = 0.0D0
           IF ( RN.NE.0.D0 ) THEN
              PROD = (ABS(R)-ABS(RN))
              
C  ON VERIFIE QUE LA RAECTION TANGENTIELLE EXISTE ET EST POSITIVE

              IF ( PROD/ABS(R).LT.-R8PREM() ) THEN
                 CALL UTMESS('F','RESUCO','NORME TANGENTIELLE
     &             DE FROTTEMENT NEGATIVE')
              ENDIF
C               OUI -> CALCUL DU RAPPORT
              COE = PROD/RN
           ENDIF

           IF (NIV.EQ.2) THEN
              VARC   = ZR(JCNSVR-1+ (NUM1-1)*19+1 )
              AJEUFT = ZR(JCNSVR-1+ (NUM1-1)*19+9 )
              IF (VARC.NE.0.0D0) THEN
              WRITE (IFM,1001) '* ','LIAISON ACTIVE   ',II,' * NOEUD ',
     &          NOM1,' * ',CHAIN,NOM2,' *','JEU ',ZR(JAPJEU+II-1),' *',
     &          'RN ',RN,' *','GLI ',AJEUFT,' *','R  ',R,
     &          ' *','RT/RN  ',COE,' *'
              ELSE
              WRITE (IFM,1001) '* ','LIAISON INACTIVE ',II,' * NOEUD ',
     &          NOM1,' * ',CHAIN,NOM2,' *','JEU ',ZR(JAPJEU+II-1),' *',
     &          'RN ',RN,' *','GLI ',AJEUFT,' *','R  ',R,
     &          ' *','RT/RN  ',COE,' *'
              ENDIF
           ENDIF

           ZL(JCNSLR-1+ (NUM1-1)*19+1 ) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+2 ) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+3 ) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+4 ) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+5 ) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+6 ) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+7 ) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+8 ) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+9 ) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+10) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+11) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+12) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+13) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+14) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+15) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+16) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+17) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+18) = .TRUE.
           ZL(JCNSLR-1+ (NUM1-1)*19+19) = .TRUE.

 120   CONTINUE
C ======================================================================
       IF (NIV.EQ.2) THEN
          WRITE (IFM,'(''<CONTACT_2> '',10X,153(''*''))')
          WRITE (IFM,*)
       ENDIF
      ENDIF
C ======================================================================
      CALL JEDEMA()
C ======================================================================
 1001 FORMAT ('<CONTACT_2> ',10X,A2,A17,I5,A9,A8,A3,A7,A8,A2,A4,1PE12.5,
     &       A2,A4,1PE12.5,A2,A4,1PE12.5,A2,A4,1PE12.5,A2,A6,1PE12.5,A2)
C ======================================================================
      END
