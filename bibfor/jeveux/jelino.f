      SUBROUTINE JELINO ( CUNIT , CLAS, CCOM )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 06/09/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
      IMPLICIT NONE
      CHARACTER*(*)       CUNIT ,CLAS, CCOM
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR PERMETTANT D'IMPRIMER LA LISTE DES NOMS D'OBJETS
C JEVEUX CREES ENTRE DEUX APPELS
C LE PREMIER APPEL SERT A INITIALISER LE PROCESSUS ET A CREER UNE COPIE
C DE L'OBJET SYSTEME $$HCOD ASSOCIE A LA CLASSE PASSEE EN ARGUMENT
C
C IN  CUNIT  : NOM LOCAL DU FICHIER D'IMPRESSION
C IN  CLAS   : NOM DE LA CLASSE ASSOCIEE 
C IN  CCOM   : MESSAGE D'INFORMATION
C
C ----------------------------------------------------------------------
      INTEGER          N
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     +                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     +                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      INTEGER          JLTYP   , JLONG   , JDATE   , JIADD   , JIADM   ,
     +                 JLONO   , JHCOD   , JCARA   , JLUTI   , JMARQ  
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      INTEGER          JGENR   , JTYPE   , JDOCU   , JORIG   , JRNOM   
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C ----------------------------------------------------------------------
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
C---------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C---------- FIN  COMMUNS NORMALISES  JEVEUX ----------------------------
      CHARACTER*1      KCLAS
      CHARACTER*8      CBID
      CHARACTER*24     NOMHC
      CHARACTER*75     CMESS
      INTEGER          JULIST,IUNIFI,IC,IRET,LG,K,ICOMP,JHC,IV
C DEB ------------------------------------------------------------------
      JULIST = IUNIFI ( CUNIT )
      IF ( JULIST .EQ. 0 ) GOTO 9999
      KCLAS = CLAS
      IC = INDEX ( CLASSE , KCLAS)
      IF ( IC .EQ. 0 ) THEN
        CMESS = 'LA CLASSE'//KCLAS//' N''EST PAS OUVERTE'
        CALL JVMESS ( 'F' , 'JELINO01' , CMESS )
      ENDIF
      NOMHC = '________'//NOMBAS(IC)//'CP_HCOD_'
      CALL JEEXIN(NOMHC,IRET)
      IF ( IRET .EQ. 0 ) THEN
        CALL WKVECT(NOMHC,'G V I',NRHCOD(IC),JHC)
        CMESS = 'CREATION D''UNE COPIE DU REPERTOIRE DE NOM DE LA BASE '
     &        //'ASSOCIEE A LA CLASSE '//KCLAS
        CALL JVMESS('I','JELINO02',CMESS)
      ELSE
        CALL JELIRA(NOMHC,'LONMAX',LG,CBID) 
        IF ( LG .LT. NRHCOD(IC) ) THEN
          CMESS = 'IMPOSSIBLE D''EXPLOITER LA SAUVEGARDE DU REPERTOIRE '
     &        //'ASSOCIEE A LA CLASSE '//KCLAS
          CALL JVMESS('I','JELINO1',CMESS)
          CMESS = 'LE REPERTOIRE A ETE REDIMENSIONNE'
          CALL JVMESS('I','JELINO1',CMESS)
          CALL JEDETR(NOMHC)
          CALL WKVECT(NOMHC,'V V I',NRHCOD(IC),JHC)
        ELSE
          CALL JEVEUO(NOMHC,'L',JHC) 
C
C ON COMPARE LE CONTENU DE $$HCOD ET CP_HCOD_
C      
          WRITE(JULIST,*) ' '
          WRITE(JULIST,*) CCOM
          WRITE(JULIST,*) 'DEBUT DE LA LISTE DES IDENTIFICATEURS CREES '
          ICOMP = 0
          DO 30 K=1,NRHCOD(IC)
            IV = HCOD(JHCOD(IC)+K)
            IF ( IV .GT. 0 ) THEN
              IF (ZI(JHC+K-1) .NE. IV ) THEN
                ICOMP = ICOMP + 1
                WRITE(JULIST,'(I6,1X,A32)') ICOMP, RNOM(JRNOM(IC)+IV)
              ENDIF
            ENDIF
30        CONTINUE
          WRITE(JULIST,*) 'FIN DE LA LISTE DES IDENTIFICATEURS CREES '
        ENDIF  
      ENDIF
C
C ON RECOPIE LE CONTENU DE $$HCOD DANS LE VECTEUR CP_HCOD_
C      
      DO 10 K=1,NRHCOD(IC)
        ZI(JHC+K-1) = HCOD(JHCOD(IC)+K)
10    CONTINUE
 9999 CONTINUE
C FIN ------------------------------------------------------------------
      END
