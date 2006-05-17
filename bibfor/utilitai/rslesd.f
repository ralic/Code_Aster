      SUBROUTINE RSLESD (RESULT,NUORD,MODELE,MATERI,CARELE,EXCIT,
     &                   IEXCIT)
      IMPLICIT NONE
      INTEGER      NUORD,IEXCIT
      CHARACTER*8  RESULT,MODELE,CARELE,MATERI
      CHARACTER*19 EXCIT
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 16/05/2006   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     BUT:
C         LIRE OU ECRIRE DES NOMS DE CONCEPT DE LA SD RESULTAT ET
C         D'EXPLOITER DES OBJETS DE LA SD CORRESPONDANT AUX CHARGES.
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   RESULT : NOM DE LA SD RESULTAT
C IN   NUORD  : NUMERO D'ORDRE
C
C      SORTIE :
C-------------
C OUT  MODELE : NOM DU MODELE
C OUT  MATERI : NOM DU CHAMP MATERIAU
C OUT  CARELE : NOM DE LA CARACTERISTIQUE ELEMENTAIRE CARA_ELEM
C OUT  EXCIT  : NOM DE LA SD INFO_CHARGE
C OUT  IEXCIT : INDICE DEFINISSANT L'ORIGINE DU CHARGEMENT
C                      UTILISE LORS DES CALCLULS
C                      0 : LE CHARGEMENT EST ISSU DE LA SD RESULTAT
C                      1 : LE CHARGEMENT EST FOURNI PAR L'UTILISATEUR
C
C ......................................................................

C --------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER      JPARA,N,N1,N2,N3,N4,IEX,JLCHA,JINFC,JFCHA,NCHA
      INTEGER      ILU,ISD,NCHALU,NCHASD,LCHALU,FCHALU

      CHARACTER*6  NOMPRO
      CHARACTER*8  BLAN8,NOMSD,NOMLU,FONCLU,K8B,FONCSD
      CHARACTER*16 TYPE,NOMCMD
      CHARACTER*19 KCHA, KFON
      CHARACTER*24 EXCISD

      PARAMETER(NOMPRO='RSLESD')
      
C ----------------------------------------------------------------------

      CALL JEMARQ()
C
C--- INITIALISATIONS
C               123456789012345678901234
      BLAN8  = '        '
      KCHA   = '&&'//NOMPRO//'.CHARGE    '
      KFON   = '&&'//NOMPRO//'.FONC_MULT '
      IEXCIT = 0
      N4     = 0
C      
      CALL GETRES(K8B,TYPE,NOMCMD)
C
C==========================================================
C
C     T R A I T E M E N T  DU  M O D E L E 
C
C========================================================== 
C     

C      WRITE(6,*)'rslesd01'

C---  RECUPERATION DU NOM DU MODELE
C
      CALL GETVID(' ','MODELE'    ,0,1,1,NOMLU,N1)
C
C      WRITE(6,*)'rslesd01-MODELE=',MODELE

      CALL RSADPA(RESULT,'L',1,'MODELE',NUORD,0,JPARA,K8B)
      NOMSD=ZK8(JPARA)
C      WRITE(6,*)'rslesd01-RESULT=',RESULT
C      WRITE(6,*)'rslesd01-NOMSD=',NOMSD

C 
C--- VERIFICATIONS ET AFFECTATIONS
C
      IF (NOMSD.NE.' ')THEN
        IF (N1.EQ.0) THEN
           MODELE = NOMSD 
        ELSEIF (NOMSD.EQ.NOMLU) THEN
           MODELE = NOMLU
        ELSE
          CALL UTMESS('F',NOMPRO,
     &         ' LE MODELE FOURNI PAR L''UTILISATEUR EST'
     &       //' DIFFERENT DE CELUI PRESENT DANS LA SD RESULTAT.')
        ENDIF
      ELSE
        IF (N1.NE.0) THEN
           MODELE = NOMLU 
        ELSE
           MODELE = BLAN8
        ENDIF
      ENDIF        

C      WRITE(6,*)'rslesd02'

C
C--- SI LE MODELE EST ABSENT DE LA SD RESULTAT ET S'IL EST FOURNI PAR
C    L'UTILISATEUR , ON LE STOCKE DANS LA SD RESULTAT
C
      IF(NOMSD.EQ.' '.AND.NOMLU.NE.' ') THEN
         CALL RSADPA(RESULT,'E',1,'MODELE',NUORD,0,JPARA,K8B)
         ZK8(JPARA)=MODELE
      ENDIF


C      WRITE(6,*)'rslesd03'
C      WRITE(6,*)'rslesd02-MODELE=',MODELE
C      
C==========================================================
C
C     T R A I T E M E N T   D U   C A R A _ E L E M
C
C========================================================== 
C
C--- RECUPERATION DU NOM DU CARA_ELEM
C
      IF((NOMCMD.NE.'CALC_ERREUR'   ).AND.
     &   (NOMCMD.NE.'CALC_G'        ))THEN
C      WRITE(6,*)'rslesd-GET_VID'
        CALL GETVID(' ','CARA_ELEM',0,1,1,NOMLU,N2)

C      WRITE(6,*)'rslesd04'
C
        CALL RSADPA(RESULT,'L',1,'CARAELEM',NUORD,0,JPARA,K8B)
        NOMSD=ZK8(JPARA)
C 
C--- VERIFICATIONS ET AFFECTATIONS
C
        IF (NOMSD.NE.' ')THEN
C      WRITE(6,*)'rslesd-IF01=',CARELE
          IF (N2.EQ.0) THEN
C      WRITE(6,*)'rslesd-IF02=',CARELE
             CARELE = NOMSD 
C      WRITE(6,*)'rslesd-IF03=',CARELE
          ELSEIF (NOMSD.EQ.NOMLU) THEN
             CARELE = NOMLU
C      WRITE(6,*)'rslesd-IF04=',CARELE
          ELSE
C      WRITE(6,*)'rslesd-IF05=',CARELE
             CALL UTMESS('A',NOMPRO,
     &            ' LE CARA_ELEM FOURNI PAR L''UTILISATEUR'
     &          //' EST DIFFERENT DE CELUI PRESENT DANS LA SD RESULTAT,'
     &          //' ON POURSUIT LES CALCULS AVEC LE CARA_ELEM FOURNI'
     &          //' PAR L''UTILISATEUR.')
             CARELE = NOMLU
          ENDIF
        ELSE
          IF (N2.NE.0) THEN
C      WRITE(6,*)'rslesd-IF06=',CARELE
             CARELE = NOMLU 
          ELSE
C      WRITE(6,*)'rslesd-IF07=',CARELE         
             CARELE = BLAN8
          ENDIF
        ENDIF

C      WRITE(6,*)'rslesd-IF08=',CARELE
C
C--- SI LE CARA_ELEM EST ABSENT DE LA SD RESULTAT ET S'IL EST FOURNI PAR
C    L'UTILISATEUR , ON LE STOCKE DANS LA SD RESULTAT
C  

C      WRITE(6,*)'rslesd05'

        IF(NOMSD.EQ.' '.AND.NOMLU.NE.' ') THEN
           CALL RSADPA(RESULT,'E',1,'CARAELEM',NUORD,0,JPARA,K8B)
           ZK8(JPARA)=CARELE

C      WRITE(6,*)'rslesd06'

        ENDIF
      ENDIF
C            
C==========================================================
C
C     T R A I T E M E N T   D U   M A T E R I A U
C
C========================================================== 

C      WRITE(6,*)'rslesd04'
C
C---  RECUPERATION DU NOM DU CHAMP MATERIAU
C
      CALL GETVID(' ','CHAM_MATER',0,1,1,NOMLU,N3)
C
      CALL RSADPA(RESULT,'L',1,'CHAMPMAT',NUORD,0,JPARA,K8B)
      NOMSD=ZK8(JPARA)
C 
C--- VERIFICATIONS ET AFFECTATIONS
C
      IF (NOMSD.NE.' ')THEN
        IF (N3.EQ.0) THEN
           MATERI = NOMSD 
        ELSEIF (NOMSD.EQ.NOMLU) THEN
           MATERI = NOMLU
        ELSE
           CALL UTMESS('A',NOMPRO,
     &           ' LE MATERIAU FOURNI PAR L''UTILISATEUR'
     &         //' EST DIFFERENT DE CELUI PRESENT DANS LA SD RESULTAT,'
     &         //' ON POURSUIT LES CALCULS AVEC LE MATERIAU FOURNI PAR'
     &         //' L''UTILISATEUR.')        
          MATERI = NOMLU
        ENDIF
      ELSE
        IF (N3.NE.0) THEN
           MATERI = NOMLU 
        ELSE
           MATERI = BLAN8
        ENDIF
      ENDIF
C
C--- SI LE MATERIAU EST ABSENT DE LA SD RESULTAT ET S'IL EST FOURNI PAR
C    L'UTILISATEUR , ON LE STOCKE DANS LA SD RESULTAT
C
      IF(NOMSD.EQ.' '.AND.NOMLU.NE.' ') THEN
         CALL RSADPA(RESULT,'E',1,'CHAMPMAT',NUORD,0,JPARA,K8B)
         ZK8(JPARA)=MATERI
      ENDIF

C      WRITE(6,*)'rslesd05'

C            
C==========================================================
C
C     T R A I T E M E N T   D E S    C H A R G E M E N T S
C
C========================================================== 
C      
C--- RECUPERATION DES CHARGEMENTS 'EXCIT'
C
C--- LECTURE DES INFORMATIONS UTILISATEUR
C
      IF(NOMCMD.NE.'POST_ELEM') THEN

C      WRITE(6,*)'rslesd06'

         CALL GETFAC('EXCIT',NCHALU)
C
        IF ( NCHALU .NE. 0 ) THEN 
          CALL WKVECT(KCHA,'V V K8',NCHALU,LCHALU)
          CALL WKVECT(KFON,'V V K8',NCHALU,FCHALU)
C      WRITE(6,*)'rslesd07'
          DO 10 IEX = 1, NCHALU
            CALL GETVID('EXCIT','CHARGE',IEX,1,1,
     &                   ZK8(LCHALU+IEX-1),N1)
C      WRITE(6,*)'rslesd08'
            IF(NOMCMD.NE.'CALC_ERREUR') THEN 
              CALL GETVID('EXCIT','FONC_MULT',IEX,1,1,FONCLU,N2)
      
C      WRITE(6,*)'FONCLU=',FONCLU
C      WRITE(6,*)'rslesd09'

              IF (N2.NE.0) THEN
                ZK8(FCHALU+IEX-1) = FONCLU
              ENDIF
            ENDIF  
  10      CONTINUE
        ENDIF
      ELSE
C      WRITE(6,*)'rslesd-01'
        CALL GETVID(' ','CHARGE'    ,0,1,0,K8B   ,N4)
        NCHA = -N4
        NCHALU = MAX(1,NCHA)
        CALL WKVECT( KCHA ,'V V K8',NCHALU,LCHALU)
        CALL GETVID(' ','CHARGE',0,1,NCHA,ZK8(LCHALU),N4)
      ENDIF
C
C--- LECTURE DES INFORMATIONS CONTENUES DANS LA SD RESULTAT
C
C      WRITE(6,*)'rslesd-02'
      CALL RSADPA(RESULT,'L',1,'EXCIT',NUORD,0,JPARA,K8B)
      EXCISD=ZK24(JPARA)
C 
C--- VERIFICATIONS ET AFFECTATIONS
C
C     IEXCIT = 0 SD RESULTAT
C            = 1 UTILISATEUR

      IF(NOMCMD.EQ.'POST_ELEM') THEN
        IF (N4.EQ.0)  THEN
          IEXCIT = 0
          NCHALU = 0
        ELSE
          IEXCIT = 1
        ENDIF
      ELSE
        IF (NCHALU.NE.0)  IEXCIT = 1
      ENDIF

      IF (NCHALU.EQ.0.AND.EXCISD(1:1).EQ.' ') IEXCIT = 1
                    
      IF (EXCISD.NE.' ') THEN 
        EXCIT = EXCISD
        CALL JEVEUO(EXCIT(1:19)//'.LCHA','L',JLCHA)
        CALL JEVEUO(EXCIT(1:19)//'.INFC','L',JINFC)
        CALL JEVEUO(EXCIT(1:19)//'.FCHA','L',JFCHA)          
        NCHASD = ZI(JINFC)
      ENDIF
C
C--- VERIFICATIONS DES CHARGEMENTS
C
      IF( (NCHALU.NE.0) .AND. (EXCISD.NE.' ')) THEN
C
C--- VERIFICATION DE LA COHERENCE DU NOMBRE DE CHARGES ENTRE 
C    CELLES PRESENTES DANS LA SD RESULTAT ET CELLES FOURNIES 
C    PAR L'UTILISATEUR

      IF(NCHALU.NE.NCHASD) THEN
         CALL UTDEBM('A',NOMPRO,
     &    ' LE NOMBRE DE CHARGES (MOT CLE: CHARGE) FOURNI PAR'
     &  //' L''UTILISATEUR EST DIFFERENT DU NOMBRE DE CHARGES'
     &  //' PRESENT DANS LA SD RESULTAT, ON POURSUIT LES CALCULS'
     &  //' AVEC LE CHARGEMENT FOURNI PAR L''UTILISATEUR.')
              CALL UTIMPI('L',' - NOMBRE DE CHARGE FOURNI PAR'
     &  //                    ' L''UTILISATEUR:   ', 1, NCHALU)
              CALL UTIMPI('L',' - NOMBRE DE CHARGE PRESENT DANS'
     &  //                    ' LA SD RESULTAT:   ', 1, NCHASD)
              CALL UTFINM()
      ENDIF
C
C--- VERIFICATIONS DU NOM DES CHARGEMENTS
C
        DO 40 ILU = 1,NCHALU
          DO 20 ISD = 1,NCHASD
            IF(ZK8(LCHALU-1+ILU).EQ.ZK24(JLCHA-1+ISD)(1:8)) GOTO 30
 20       CONTINUE
          CALL UTMESS('A',NOMPRO,
     &        ' LE CHARGEMENT (MOT CLE: CHARGE) FOURNI PAR'
     &      //' L''UTILISATEUR EST DIFFERENT DE CELUI PRESENT DANS'
     &      //' LA SD RESULTAT,ON POURSUIT LES CALCULS AVEC LE'
     &      //' CHARGEMENT FOURNI PAR L''UTILISATEUR.')
 30       CONTINUE
 40     CONTINUE
C
C--- VERIFICATIONS DU NOM DES FONCTION MULTIPLICATRICES
C
        IF(NOMCMD.NE.'POST_ELEM') THEN
          DO 70 ILU = 1,NCHALU
            DO 50 ISD = 1,NCHASD
              FONCSD = ZK24(JFCHA-1+ISD)(1:8)
              IF(FONCSD(1:2).EQ.'&&') FONCSD = BLAN8
              IF(ZK8(FCHALU-1+ILU).EQ.FONCSD) GOTO 60
 50         CONTINUE
              CALL UTMESS('A',NOMPRO,
     &          ' LES FONCTIONS MULTIPLICATRICES DU CHARGEMENT'
     &        //' (MOT CLE: FONC_MULT) FOURNI PAR L''UTILISATEUR '
     &        //' SONT DIFFERENTES DE CELLES PRESENTES DANS LA SD '
     &        //' RESULTAT, ON POURSUIT LES CALCULS AVEC LES FONCTIONS' 
     &        //' MULTIPLICATRICES FOURNIES PAR L''UTILISATEUR.')
 60         CONTINUE
 70       CONTINUE
         ENDIF
C
C--- VERIFICATIONS DES COUPLES NOM DE CHARGE ET FONCTION MULTIPLICATRICE
C    FOURNI PAR L'UTILISATEUR AVEC CEUX PRESENTS DANS LA SD RESULTAT
C
        IF(NOMCMD.NE.'POST_ELEM') THEN
        DO 80 ILU = 1,NCHALU
          DO 90 ISD = 1,NCHASD
           IF(ZK8(LCHALU-1+ILU).EQ.ZK24(JLCHA-1+ISD)(1:8)) THEN
              FONCSD = ZK24(JFCHA-1+ISD)(1:8)
              IF(FONCSD(1:2).EQ.'&&') FONCSD = BLAN8
              IF(ZK8(FCHALU-1+ILU).EQ.FONCSD) GOTO 95
              CALL UTDEBM('A','RSLESD','LE COUPLE (CHARGE-FONCTION) '
     &    //              'FOURNI PAR L''UTILISATEUR  N''EST PAS '
     &    //              'PRESENT DANS LA SD RESULTAT, ON POURSUIT '
     &    //              'LES CALCULS AVEC LE CHARGEMENT '
     &    //              'FOURNI PAR L''UTILISATEUR.')
              CALL UTIMPK('L',' - CHARGE (UTILISATEUR):   ',
     &                     1, ZK8(LCHALU-1+ILU))
              CALL UTIMPK('L',' - FONCTION (UTILISATEUR): ',
     &                     1, ZK8(FCHALU-1+ILU))
              CALL UTIMPK('L',' - CHARGE (SD RESULTAT):   ',
     &                     1, ZK24(JLCHA-1+ISD)(1:8))
              CALL UTIMPK('L',' - FONCTION (SD RESULTAT): ',
     &                     1, FONCSD)
              CALL UTFINM()
           ENDIF
 90       CONTINUE 
 95     CONTINUE
 80     CONTINUE
        ENDIF
       ENDIF
C
C SI LE TYPE DE RESULTAT EST EVOL_THER ET CALC_NO ON NE PREND PAS 
C LA CHARGE CONTENUE DANS LA SD RESULTAT
C
       IF(TYPE(1:9).EQ.'EVOL_THER'.AND.NOMCMD.EQ.'CALC_NO') IEXCIT = 1
C 
C--- MENAGE
C
      CALL JEDETR(KCHA)
      CALL JEDETR(KFON)
C
      CALL JEDEMA()
      END
