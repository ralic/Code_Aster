       SUBROUTINE CFADH(RESOCO,DEFICO,NOMA,NDIM,
     &                  INDIC,NBLIAC,NBLIAI,AJLIAI,SPLIAI,
     &                  LLF,LLF1,LLF2)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/11/2004   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C 
      IMPLICIT     NONE
      CHARACTER*24 RESOCO
      CHARACTER*24 DEFICO 
      CHARACTER*8  NOMA
      INTEGER      NDIM
      INTEGER      INDIC
      INTEGER      NBLIAC
      INTEGER      AJLIAI
      INTEGER      SPLIAI
      INTEGER      LLF
      INTEGER      LLF1
      INTEGER      LLF2
      INTEGER      NBLIAI
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : FRO2GD/FROLGD
C ----------------------------------------------------------------------
C
C  VERIFICATION QUE LES LIAISONS SONT BIEN ADHERENTES
C
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C                'E': RESOCO(1:14)//'.MU'
C IN  NOMA   : NOM DU MAILLAGE
C IN  NDIM   : DIMENSION DU PROBLEME
C OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON 
C             -1 ON A ENLEVE UNE LIAISON
C IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
C I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES 
C I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON CORRECTE DU CALCUL 
C              DE LA MATRICE DE CONTACT ACM1AT
C I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
C I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
C              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX 
C               DIRECTIONS SIMULTANEES (EN 3D)
C I/O LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA 
C               PREMIERE DIRECTION (EN 3D)
C I/O LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA 
C               SECONDE DIRECTION (EN 3D)
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      BTOTAL,LLIAC,LLJAC,POSIT,LIAISO
      INTEGER      II,JJ
      INTEGER      COMPT0, COMPTS, COMPTN, JSPLF0
      INTEGER      COMPT1, COMPT2, COMPTU, COMPTV
      REAL*8       XK, XCOMP, XQUOT, XCOS
      CHARACTER*1  TYPESP
      CHARACTER*2  TYPEC0, TYPEF0, TYPEF1, TYPEF2, TYPLIA
      CHARACTER*19 LIAC,CONVEC,MU
      INTEGER      JLIAC,JVECC,JMU
      CHARACTER*24 APPARI,CONTNO,CONTMA,FROTE
      INTEGER      JAPPAR,JNOCO,JMACO,IFRO
      INTEGER      IFM,NIV
C
C ----------------------------------------------------------------------
C
      CALL INFNIV(IFM,NIV)
      CALL JEMARQ()
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
      APPARI = RESOCO(1:14)//'.APPARI'
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CONTMA = DEFICO(1:16)//'.MAILCO' 
      LIAC   = RESOCO(1:14)//'.LIAC'
      CONVEC = RESOCO(1:14)//'.CONVEC'
      MU     = RESOCO(1:14)//'.MU'
      FROTE  = DEFICO(1:16)//'.FROTE'
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(LIAC,  'L',JLIAC )
      CALL JEVEUO(CONVEC,'L',JVECC )
      CALL JEVEUO(MU,    'E',JMU   )
      CALL JEVEUO(FROTE, 'L',IFRO  )
C ======================================================================
C --- INITIALISATION DES VARIABLES 
C ======================================================================
      TYPESP = 'S'
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      TYPEF1 = 'F1'
      TYPEF2 = 'F2'
C
      CALL WKVECT ('&&CFADH.SUPLF0','V V I',LLF+LLF1+LLF2,JSPLF0)
      IF (NDIM.EQ.3) THEN
C ======================================================================
C --- CAS 3D 
C ======================================================================
        COMPT0 = 0
        COMPT1 = 0
        COMPT2 = 0
        COMPTS = 0
        COMPTU = 0
        COMPTV = 0
C ======================================================================
C --- CAS D'UNE LIAISON DE FROTTEMENT SUIVANT LES DEUX DIRECTIONS 
C ======================================================================
        DO 20 JJ = 1, NBLIAC + LLF + LLF1 + LLF2
          IF (ZK8(JVECC-1+JJ).EQ.TYPEF0) THEN
            COMPTN = 0
            COMPT0 = COMPT0 + 1
            LLJAC  = ZI(JLIAC-1+JJ)
            DO 30 II = 1, JJ-1
              LLIAC = ZI(JLIAC-1+II)
              IF ( ZK8(JVECC-1+II).EQ.TYPEC0 ) THEN
                 COMPTN = COMPTN + 1
              ENDIF
              IF ( LLIAC.EQ.LLJAC ) THEN
                 XQUOT = 0.0D0
                 XK    = ZR(IFRO -1+LLIAC)
C ======================================================================
C --- NORME DE MU POUR LE FROTTEMENT 
C ======================================================================
                 XCOMP  = SQRT(  ZR(JMU-1+NBLIAC    +COMPT0)**2 +
     +                           ZR(JMU-1+NBLIAC+LLF+COMPT0)**2 )
C ======================================================================
C --- TEST SUR MU POUR LE CONTACT 
C ======================================================================
                 IF ( ZR(JMU-1+COMPTN).GT.0.0D0 ) THEN
                    XQUOT = XCOMP/ZR(JMU-1+COMPTN)
                 ENDIF
                 IF ( ABS(XQUOT).GE.XK ) THEN
                    COMPTS = COMPTS + 1
                    ZI(JSPLF0-1+COMPTS+COMPTU+COMPTV) = JJ
                 ELSE
                    ZR(JMU-1+NBLIAC+COMPT0-COMPTS) =
     +                                     ZR(JMU-1+NBLIAC+COMPT0)
                    ZR(JMU-1+NBLIAC+LLF+COMPT0-COMPTS) =
     +                                   ZR(JMU-1+NBLIAC+LLF+COMPT0)
                 ENDIF
                 GOTO 20
              ENDIF
 30         CONTINUE
          ELSE IF (ZK8(JVECC-1+JJ).EQ.TYPEF1) THEN
            COMPTN = 0
            COMPT1 = COMPT1 + 1
            LLJAC  = ZI(JLIAC-1+JJ)
            DO 40 II = 1, JJ-1
               LLIAC = ZI(JLIAC-1+II)
               IF ( ZK8(JVECC-1+II).EQ.TYPEC0 ) THEN
                  COMPTN = COMPTN + 1
               ENDIF
               IF ( LLIAC.EQ.LLJAC ) THEN
                  XQUOT = 0.0D0
                  XK    = ZR(IFRO -1+LLIAC)
C ======================================================================
C --- NORME DE MU POUR LE FROTTEMENT 
C ======================================================================
                  XCOMP = ABS(ZR(JMU-1+NBLIAC+(NDIM-1)*LLF+COMPT1))
C ======================================================================
C --- TEST SUR MU POUR LE CONTACT 
C ======================================================================
                  IF ( ZR(JMU-1+COMPTN).GT.0.0D0 ) THEN
                     XQUOT = XCOMP/ZR(JMU-1+COMPTN)
                  ENDIF
                  IF ( ABS(XQUOT).GE.XK ) THEN
                     COMPTU = COMPTU + 1
                     ZI(JSPLF0-1+COMPTS+COMPTU+COMPTV) = JJ
                  ELSE
                     ZR(JMU-1+NBLIAC+(NDIM-1)*LLF+COMPT1-COMPTU) =
     +                           ZR(JMU-1+NBLIAC+(NDIM-1)*LLF+COMPT1)
                  ENDIF
                  GOTO 20
                ENDIF
 40         CONTINUE
          ELSE IF (ZK8(JVECC-1+JJ).EQ.TYPEF2) THEN
            COMPTN = 0
            COMPT2 = COMPT2 + 1
            LLJAC  = ZI(JLIAC-1+JJ)
            DO 50 II = 1, JJ-1
               LLIAC = ZI(JLIAC-1+II)
               IF ( ZK8(JVECC-1+II).EQ.TYPEC0 ) THEN
                  COMPTN = COMPTN + 1
               ENDIF
               IF ( LLIAC.EQ.LLJAC ) THEN
                  XQUOT = 0.0D0
                  XK    = ZR(IFRO -1+LLIAC)
C ======================================================================
C --- NORME DE MU POUR LE FROTTEMENT 
C ======================================================================
                  XCOMP=
     +               ABS(ZR(JMU-1+NBLIAC+(NDIM-1)*LLF+LLF1+COMPT2))
C ======================================================================
C --- TEST SUR MU POUR LE CONTACT 
C ======================================================================
                  IF ( ZR(JMU-1+COMPTN).GT.0.0D0 ) THEN
                     XQUOT = XCOMP/ZR(JMU-1+COMPTN)
                  ENDIF
                  IF ( ABS(XQUOT).GE.XK ) THEN
                     COMPTV = COMPTV + 1
                     ZI(JSPLF0-1+COMPTS+COMPTU+COMPTV) = JJ
                  ELSE
                     ZR(JMU-1+NBLIAC+(NDIM-1)*LLF+LLF1+COMPT2-COMPTV) =
     +                       ZR(JMU-1+NBLIAC+(NDIM-1)*LLF+LLF1+COMPT2)
                  ENDIF
                  GOTO 20
               ENDIF
 50         CONTINUE
          ENDIF
 20     CONTINUE
        IF ((COMPTS+COMPTU+COMPTV).NE.0) THEN
          DO 60 JJ  = 1, COMPTS+COMPTU+COMPTV
             II     = COMPTS + COMPTU + COMPTV - JJ + 1
             POSIT  = ZI(JSPLF0-1+II)
             LIAISO = ZI(JLIAC-1+POSIT)
             TYPLIA = ZK8(JVECC-1+POSIT)
             CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,
     +             LLF,LLF1,LLF2,RESOCO,TYPESP,POSIT,LIAISO,TYPLIA)
             IF (NIV.GE.2) THEN
              CALL CFIMP2(IFM,NOMA,LIAISO,TYPLIA,TYPESP,'GLI',0.D0,
     &                    JAPPAR,JNOCO,JMACO)
             ENDIF
 60       CONTINUE
          DO 70 JJ = 1, LLF
             ZR(JMU-1+NBLIAC+LLF+JJ) =
     +                               ZR(JMU-1+NBLIAC+LLF+COMPTS+JJ)
 70       CONTINUE
          DO 80 JJ = 1, LLF1
             ZR(JMU-1+NBLIAC+(NDIM-1)*LLF+JJ) =
     +                    ZR(JMU-1+NBLIAC+(NDIM-1)*(LLF+COMPTS)+JJ)
 80       CONTINUE
          DO 90 JJ = 1, LLF2
             ZR(JMU-1+NBLIAC+(NDIM-1)*LLF+LLF1+JJ) =
     +           ZR(JMU-1+NBLIAC+(NDIM-1)*(LLF+COMPTS)+LLF1+COMPTU+JJ)
 90       CONTINUE
        ENDIF
      ELSE
C ======================================================================
C --- CAS 2D 
C ======================================================================
        COMPT0 = 0
        COMPTS = 0
        BTOTAL = NBLIAC + LLF
        DO 300 II = 1, BTOTAL
          COMPTN = 0
C ======================================================================
C --- CAS D'UNE LIAISON DE FROTTEMENT CAS GENERAL EN 2D 
C ======================================================================
          IF (ZK8(JVECC-1+II).EQ.TYPEF0) THEN
            COMPT0 = COMPT0 + 1
             LLIAC  = ZI(JLIAC-1+II)
             XK     = ZR(IFRO-1+LLIAC)
             XCOMP  = ABS( ZR(JMU-1+NBLIAC+COMPT0) )
             DO 310 JJ = 1, II - 1
               IF (ZK8(JVECC-1+JJ).EQ.TYPEC0) THEN
                  COMPTN = COMPTN + 1
                  LLJAC  = ZI(JLIAC-1+JJ)
                  IF (LLJAC.EQ.LLIAC) THEN
                     GOTO 312
                  ENDIF
               ENDIF
 310         CONTINUE
             CALL UTMESS ('F','CFADH','CAS 2D IMPOSSIBLE')
          ENDIF
          GOTO 300
 312      CONTINUE
          XQUOT = 0.0D0
          IF ( ZR(JMU-1+COMPTN).GT.0.0D0 ) THEN
             XQUOT = XCOMP/ZR(JMU-1+COMPTN)
          ENDIF
          IF ( ABS(XQUOT).GE.XK ) THEN
             COMPTS = COMPTS + 1
             ZI(JSPLF0-1+COMPTS) = II
             XCOS   = ZR(JMU-1+NBLIAC+COMPT0)/XCOMP
             ZR(JMU+3*NBLIAI-1+LLIAC) = XCOS * XK
          ELSE
             ZR(JMU-1+NBLIAC+COMPT0-COMPTS) =
     +                           ZR(JMU-1+NBLIAC+COMPT0)
             ZR(JMU+3*NBLIAI-1+LLIAC) = 0.0D0
          ENDIF
 300    CONTINUE
        DO 160 JJ  = 1, COMPTS
          II     = COMPTS - JJ + 1
          POSIT  = ZI(JSPLF0-1+II)
          LIAISO = ZI(JLIAC-1+POSIT)
          TYPLIA = ZK8(JVECC-1+POSIT)
               CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,
     +                  LLF,LLF1,LLF2,RESOCO,TYPESP,POSIT,LIAISO,TYPLIA)
               IF (NIV.GE.2) THEN
                CALL CFIMP2(IFM,NOMA,LIAISO,'F3',TYPESP,'GLI',0.D0,
     &                      JAPPAR,JNOCO,JMACO)
               ENDIF
 160    CONTINUE
      ENDIF
      CALL JEDETR('&&CFADH.SUPLF0')
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
