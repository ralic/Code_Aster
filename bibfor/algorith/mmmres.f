      SUBROUTINE MMMRES(DEFICO,DEPDEL,NUMEDD,NOMA,CNSINR)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/07/2005   AUTEUR LAMARCHE S.LAMARCHE 
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
      IMPLICIT NONE
      CHARACTER*8 NOMA
      CHARACTER*19 CNSINR
      CHARACTER*24 DEFICO,DEPDEL,NUMEDD

C   BUT : PREPARER LE CHAM_NO_S POUR L ARCHIVAGE DU CONTACT PAR NMARCH
C         POUR LA METHODE CONTINUE ( ROUTINE JUMELLE DE CFRESU POUR LES
C         AUTRES TYPES DE CONTACT )

C   IN       DEFICO : SD DU CONTACT
C   IN       DEPDEL : DEPLACEMENT AU PAS DE TEMPS COURANT
C   IN       NUMEDD : NUME_DDL POUR LA CREATION DES CHAM_NO
C   IN       NOMA   : NOM DU MAILLAGE
C   IN/OUT   CNSINR : CHAM_NO_S POUR L ARCHIVAGE DU CONTACT        


C ======================================================================
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
      CHARACTER*32 JEXNUM,JEXATR
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


      CHARACTER*8 LICMPR(19),LICMP4(4),LICMP6(6),LICNT2(2),LICNT3(3)
      CHARACTER*8 ALIAS
      INTEGER IRET,JNOESC,NBNO,INO,JTABF,POSNO,JDIM,NUNOE,NUNORE,ILIST
      INTEGER JCNSVR,JCNSLR,JCONT,JFROT,JNOCO,JZONE,ISUCO,NSUCO
      INTEGER JRECO,JJEU,ICONEX,IATYMA,NUMAMA,NUTYP,NNO,JNOE,NUNO
      INTEGER JDEPDE,I,DIM,IZONE,JCMCF,ILONG,JDIME,JTRAV,POSMA,NUMAES
      INTEGER NTPC,IMA,NTMA,JMAESC,NBNOT,JMACO,JEUTEM,JGLIE,JGLIM
      INTEGER JCONTA,JPREMI,JZOCO,NBZONE,INTEGR,JFROL,JCONL,JDEPDL
      REAL*8 LICOEF,RN,RNX,RNY,RNZ,GLI1,GLI2,GLI,RTAX,RTAY,RTAZ,RTGX
      REAL*8 RTGY,RTGZ,RX,RY,RZ,R,X(2),FF(9)
      REAL*8 DEPLP(3),CONT,LAGSF,VECT1(3),VECT2(3)
      REAL*8 CONFRO(3),R8PREM,GLIOLD
      CHARACTER*19 FCONT,FFROT,FCONTS,FFROTS,DEPDES,DDEPLS,DEPCN
      CHARACTER*19 FCTCN, FFROCN
      CHARACTER*24 JEU,GLIE,GLIM,CONTAC,PREMIE
      
      CALL JEMARQ()
      FCONT='&&MMMRES.CONT'
      FCONTS='&&MMMRES.CONT_S'
      FCTCN='&&MMMRES.FCTCN'
      FFROT='&&MMMRES.FROT'
      FFROTS='&&MMMRES.FROT_S'
      FFROCN='&&MMMRES.FROTCN'
      DEPDES='&&MMMRES.DEPDES'
      DEPCN='&&MMMRES.DEPCN'
      CNSINR='&&MMMRES.CNSRINR'
      JEU='&&MMMRES.JEU'
      GLIE='&&MMMRES.GLIE'
      GLIM='&&MMMRES.GLIM'
      CONTAC='&&MMMRES.CONTAC'
      PREMIE='&&MMMRES.PREMIE'

      CALL JEEXIN('&&CFMVEL.LISTE_RESU',IRET)
        
C-----EST CE QU ON EST EN CONTACT CONTINUE ?      
      IF (IRET.GT.0) THEN
      
C ------ RECUPERATION DES DIVERSES DONNEES RELATIVES AU CONTACT

         CALL JEVEUO(DEFICO(1:16)//'.TABFIN','L',JTABF)
         CALL JEVEUO(DEFICO(1:16)//'.NDIMCO','L',JDIM)
         CALL JEVEUO(DEFICO(1:16)//'.NOESCL','L',JNOESC)
         CALL JEVEUO(DEFICO(1:16)//'.NOEUCO','L',JNOCO)
         CALL JEVEUO(DEFICO(1:16)//'.JEUCON','L',JJEU)
         CALL JEVEUO(DEFICO(1:16)//'.CARACF','L',JCMCF)
         CALL JEVEUO(DEFICO(1:16)//'.MAESCL','L',JMAESC)
         CALL JEVEUO(NOMA(1:8)//'.TYPMAIL','L',IATYMA)
         CALL JEVEUO(DEFICO(1:16)//'.MAILCO','L',JMACO)
         CALL JEVEUO(DEFICO(1:16)//'.NOZOCO','L',JZOCO)

C---------RECHERCHE DE LA METHODE D INTEGRATION

         NBZONE=INT(ZR(JCMCF))
         DO 1 I=1,NBZONE
            IF (ZR(JCMCF+6*(I-1)+1).EQ.2.D0) THEN
               CALL UTMESS('A','MMMRES','LA  METHODE D INTEGRATION'//
     &         ' EST GAUSS, LE CHAMP VALE_CONT N EST PAS CREE')
               GOTO 9999
            ENDIF
  1      CONTINUE

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



C ------ TRANSFORMATION DU CHAM_NO DES DDL EN UN CHAM_NO_S POUR 
C ------ UNE LECTURE FACILE

         DIM=ZI(JDIM)
         CALL CNOCNS(DEPDEL,'V',DEPDES)

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


         IF (DIM.EQ.3) THEN
         CALL CNSRED(DEPDES,0,0,6,LICMP6,'V',DEPCN)
         ELSE IF(DIM.EQ.2) THEN
         CALL CNSRED(DEPDES,0,0,4,LICMP4,'V',DEPCN)
         ENDIF

         CALL JEVEUO(DEPCN//'.CNSV','L',JDEPDE)
         CALL JEVEUO(DEPCN//'.CNSL','L',JDEPDL)
         

         
C ------ CREATION DU CHAM_NO_S POUR L ARCHIVAGE DU CONTACT

         CALL CNSCRE(NOMA,'INFC_R',19,LICMPR,'V',CNSINR)
         CALL JEVEUO(CNSINR//'.CNSV','E',JCNSVR)
         CALL JEVEUO(CNSINR//'.CNSL','E',JCNSLR)
         LICOEF=1.D0
         

C ------ ASSEMBLAGE DES SECONDS MEMBRES DU AUX CONTACTS
C ------ POUR RECUPERER LES FORCES NODALES DE CONTACT 

         LICNT3(1) = 'DX'
         LICNT3(2) = 'DY'
         LICNT3(3) = 'DZ'

         LICNT2(1) = 'DX'
         LICNT2(2) = 'DY'

         CALL ASSVEC('V',FCONT,1,'&&MMCCON',LICOEF,
     &    NUMEDD,' ','ZERO',1)

         CALL CNOCNS(FCONT,'V',FCONTS)
         IF (DIM.EQ.3) THEN
         CALL CNSRED(FCONTS,0,0,3,LICNT3,'V',FCTCN)
         ELSE IF(DIM.EQ.2) THEN
         CALL CNSRED(FCONTS,0,0,2,LICNT2,'V',FCTCN)
         ENDIF         
         CALL JEVEUO(FCTCN//'.CNSV','L',JCONT)
         CALL JEVEUO(FCTCN//'.CNSL','L',JCONL)


         CALL ASSVEC('V',FFROT,1,'&&MMCFROT',LICOEF,
     &    NUMEDD,' ','ZERO',1)

         CALL CNOCNS(FFROT,'V',FFROTS)
         IF (DIM.EQ.3) THEN
         CALL CNSRED(FFROTS,0,0,3,LICNT3,'V',FFROCN)
         ELSE IF(DIM.EQ.2) THEN
         CALL CNSRED(FFROTS,0,0,2,LICNT2,'V',FFROCN)
         ENDIF         
         CALL JEVEUO(FFROCN//'.CNSV','L',JFROT)
         CALL JEVEUO(FFROCN//'.CNSL','L',JFROL)



         CALL JEVEUO (JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',ILONG)
         CALL JEVEUO (NOMA(1:8)//'.CONNEX','L',ICONEX)
         CALL JEVEUO (NOMA(1:8)//'.DIME','L',JDIME)
         NBNO=ZI(JDIME)


C--------CREATION D OBJETS DE TRAVAIL
C--------JEU DES POINTS DE CONTACT 
         CALL WKVECT(JEU,'V V R',NBNO,JEUTEM)
C--------VECTEUR DE GLISSEMENT DU NOEUD ESCLAVE
         CALL WKVECT(GLIE,'V V R',2*NBNO,JGLIE)
C--------VECTEUR DE GLISSEMENT DU NOEUD MAITRE
         CALL WKVECT(GLIM,'V V R',2*NBNO,JGLIM)
C--------INDICATEUR DE CONTACT
         CALL WKVECT(CONTAC,'V V R',NBNO,JCONTA)
C--------VECTEUR LOGIQUE INDIQUANT UN PREMIER PASSAGE SUR UN NOEUD
         CALL WKVECT(PREMIE,'V V L',NBNO,JPREMI)

         NBNOT=ZI(JDIM+4)
         
         IF (DIM.EQ.3) THEN

C------CALCUL DU JEU AUX NOEUDS EN PRENANT LE MIN DES JEUX AUX NOEUDS,
C------DU GLISSEMENT EN PRENANT LE MAX DU GLISSEMENT AUX NOEUDS
C------DE L INDICATEUR DE CONTACT EN CONSIDERANT QU UN NOEUD EST EN
C------CONTACT A PARTIR DU MOMENT OU IL L EST SUR AU MOINS UNE 
C------DES MAILLES.
         NTPC=0
         NTMA = ZI(JMAESC)
         DO 10 IMA = 1,NTMA
             POSMA = ZI(JMAESC+3*(IMA-1)+1)
             NUMAES = ZI(JMACO-1+POSMA)
             NBNO  = ZI(JMAESC+3* (IMA-1)+3)
             IZONE  = ZI(JMAESC+3* (IMA-1)+2)
             INTEGR=INT(ZR(JCMCF)+6*(IZONE-1)+1)

        IF (INTEGR.EQ.3 .OR. INTEGR.EQ.4 .OR. INTEGR.EQ.5) THEN
        NBNO=NBNO/2
        ENDIF
        

             DO 20 INO = 1,NBNO
                NUNOE=ZI(ICONEX+ZI(ILONG-1+NUMAES)+INO-2)

C ------ VECTEURS DIRECTEURS DU PLAN DE CONTACT 

                VECT1(1)=ZR(JTABF+16*(NTPC+INO-1)+6)
                VECT1(2)=ZR(JTABF+16*(NTPC+INO-1)+7)
                VECT1(3)=ZR(JTABF+16*(NTPC+INO-1)+8)
                VECT2(1)=ZR(JTABF+16*(NTPC+INO-1)+9)
                VECT2(2)=ZR(JTABF+16*(NTPC+INO-1)+10)
                VECT2(3)=ZR(JTABF+16*(NTPC+INO-1)+11)

                NUMAMA=ZR(JTABF+16*(NTPC+INO-1)+2)
                NUTYP=ZI(IATYMA-1+NUMAMA)

                IF (NUTYP.EQ.2) THEN
                    ALIAS = 'SG2'
                    NNO =2
                    X(1)=ZR(JTABF+16*(NTPC+INO-1)+4)
                ELSE IF (NUTYP.EQ.4) THEN
                    ALIAS = 'SG3'
                    NNO =3
                    X(1)=ZR(JTABF+16*(NTPC+INO-1)+4)
                ELSE IF (NUTYP.EQ.7) THEN
                    ALIAS = 'TR3'
                    NNO =3
                    X(1)=ZR(JTABF+16*(NTPC+INO-1)+4)
                    X(2)=ZR(JTABF+16*(NTPC+INO-1)+12)
                ELSE IF (NUTYP.EQ.9) THEN
                    ALIAS = 'TR6'
                    NNO =6
                    X(1)=ZR(JTABF+16*(NTPC+INO-1)+4)
                    X(2)=ZR(JTABF+16*(NTPC+INO-1)+12)
                ELSE IF (NUTYP.EQ.12) THEN
                    ALIAS = 'QU4'
                    NNO =4
                    X(1)=ZR(JTABF+16*(NTPC+INO-1)+4)
                    X(2)=ZR(JTABF+16*(NTPC+INO-1)+12)
                ELSE IF (NUTYP.EQ.14) THEN
                    ALIAS = 'QU8'
                    NNO =8
                    X(1)=ZR(JTABF+16*(NTPC+INO-1)+4)
                    X(2)=ZR(JTABF+16*(NTPC+INO-1)+12)
                ELSE IF (NUTYP.EQ.16) THEN
                    ALIAS = 'QU9'
                    NNO =9
                    X(1)=ZR(JTABF+16*(NTPC+INO-1)+4)
                    X(2)=ZR(JTABF+16*(NTPC+INO-1)+12)
                ELSE
                    CALL UTMESS('F','MMMRES','TYPE DE MAILLE INCONNU')
                ENDIF
                
                CALL CALFFX(ALIAS,X(1),X(2),FF)

                DEPLP(1)=0.D0
                DEPLP(2)=0.D0
                DEPLP(3)=0.D0

C----DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C----POUR LE CALCUL DU GLISSEMENT

                DO 30 I=1,NNO
                   NUNO=ZI(ICONEX+ZI(ILONG-1+NUMAMA)+I-2)
        
                   CALL ASSERT(ZL(JDEPDL-1+6*(NUNO-1)+1))
                   CALL ASSERT(ZL(JDEPDL-1+6*(NUNO-1)+2))
                   CALL ASSERT(ZL(JDEPDL-1+6*(NUNO-1)+3))

                   DEPLP(1)=DEPLP(1)+ZR(JDEPDE-1+6*(NUNO-1)+1)*FF(I)
                   DEPLP(2)=DEPLP(2)+ZR(JDEPDE-1+6*(NUNO-1)+2)*FF(I)
                   DEPLP(3)=DEPLP(3)+ZR(JDEPDE-1+6*(NUNO-1)+3)*FF(I)

   30           CONTINUE

C----ECRITURE SUR LES VECTEURS DE TRAVAIL DES JEUX, DU GLISSEMENT ET DE
C----L INDICATEUR DE CONTACT      

                IF (.NOT.ZL(JPREMI-1+NUNOE)) THEN
                
                   CALL ASSERT(ZL(JDEPDL-1+6*(NUNOE-1)+1))
                   CALL ASSERT(ZL(JDEPDL-1+6*(NUNOE-1)+2))
                   CALL ASSERT(ZL(JDEPDL-1+6*(NUNOE-1)+3))

                   ZR(JEUTEM+NUNOE-1)=ZR(JJEU-1+NTPC+INO)
                   ZL(JPREMI-1+NUNOE)=.TRUE.
                   ZR(JCONTA+NUNOE-1)=ZR(JTABF+16*(NTPC+INO-1)+13)

                   ZR(JGLIE+2*(NUNOE-1))=
     &             ZR(JDEPDE-1+6*(NUNOE-1)+1)*VECT1(1)
     &             +ZR(JDEPDE-1+6*(NUNOE-1)+2)*VECT1(2)
     &             +ZR(JDEPDE-1+6*(NUNOE-1)+3)*VECT1(3)

                   ZR(JGLIE+2*(NUNOE-1)+1)=
     &             ZR(JDEPDE-1+6*(NUNOE-1)+1)*VECT2(1)
     &             +ZR(JDEPDE-1+6*(NUNOE-1)+2)*VECT2(2)
     &             +ZR(JDEPDE-1+6*(NUNOE-1)+3)*VECT2(3)

                   ZR(JGLIM+2*(NUNOE-1))=DEPLP(1)*VECT1(1)
     &             +DEPLP(2)*VECT1(2)+DEPLP(3)*VECT1(3)
     
                   ZR(JGLIM+2*(NUNOE-1)+1)=DEPLP(1)*VECT2(1)
     &             +DEPLP(2)*VECT2(2)+DEPLP(3)*VECT2(3)
     
                ELSE
                   ZR(JEUTEM+NUNOE-1)=MIN(ZR(JEUTEM+NUNOE-1),
     &                             ZR(JJEU-1+NTPC+INO))
     
                   ZR(JCONTA+NUNOE-1)=MAX(ZR(JCONTA+NUNOE-1),
     &                             ZR(JTABF+16*(NTPC+INO-1)+13))
     
                   GLI1=ZR(JDEPDE-1+6*(NUNOE-1)+1)*VECT1(1)
     &           +ZR(JDEPDE-1+6*(NUNOE-1)+2)*VECT1(2)
     &           +ZR(JDEPDE-1+6*(NUNOE-1)+3)*VECT1(3)
     &           -DEPLP(1)*VECT1(1)-DEPLP(2)*VECT1(2)
     &                -DEPLP(3)*VECT1(3)

                   GLI2=ZR(JDEPDE-1+6*(NUNOE-1)+1)*VECT2(1)
     &           +ZR(JDEPDE-1+6*(NUNOE-1)+2)*VECT2(2)
     &           +ZR(JDEPDE-1+6*(NUNOE-1)+3)*VECT2(3)
     &           -DEPLP(1)*VECT2(1)-DEPLP(2)*VECT2(2)
     &                -DEPLP(3)*VECT2(3)

                   GLI=SQRT(GLI1**2+GLI2**2)
                   
                   GLIOLD=SQRT((ZR(JGLIE+2*(NUNOE-1))-
     &             ZR(JGLIM+2*(NUNOE-1)))**2
     &           +(ZR(JGLIE+2*(NUNOE-1)+1)-ZR(JGLIM+2*(NUNOE-1)+1))**2)
                   
                   IF ( GLI.GT.GLIOLD ) THEN

                      ZR(JGLIE+2*(NUNOE-1))=
     &                ZR(JDEPDE-1+6*(NUNOE-1)+1)*VECT1(1)
     &                +ZR(JDEPDE-1+6*(NUNOE-1)+2)*VECT1(2)
     &                +ZR(JDEPDE-1+6*(NUNOE-1)+3)*VECT1(3)

                      ZR(JGLIE+2*(NUNOE-1)+1)=
     &                ZR(JDEPDE-1+6*(NUNOE-1)+1)*VECT2(1)
     &                +ZR(JDEPDE-1+6*(NUNOE-1)+2)*VECT2(2)
     &                +ZR(JDEPDE-1+6*(NUNOE-1)+3)*VECT2(3)

                      ZR(JGLIM+2*(NUNOE-1))=DEPLP(1)*VECT1(1)
     &                +DEPLP(2)*VECT1(2)+DEPLP(3)*VECT1(3)
     
                      ZR(JGLIM+2*(NUNOE-1)+1)=DEPLP(1)*VECT2(1)
     &                +DEPLP(2)*VECT2(2)+DEPLP(3)*VECT2(3)
                   ENDIF 
                ENDIF

 20          CONTINUE
             NTPC=NTPC+NBNO
 10      CONTINUE

C ------ BOUCLE SUR TOUS LES NOEUDS DE CONTACT
         DO 40 INO=1,NBNOT
         
C ------ TRAITEMENT DES NOEUDS ESCLAVES SEULEMENT
            IF (ZR(JNOESC+10*(INO-1)+1) .EQ. -1.D0) THEN

C ------ NUMERO ABSOLU DU NOEUD TRAITE

              NUNOE=ZI(JNOCO+INO-1)
              ZR(JCNSVR-1+19*(NUNOE-1)+2)=ZR(JEUTEM+NUNOE-1)
              CONT=ZR(JCONTA+NUNOE-1)

              GLI1=ZR(JGLIE+2*(NUNOE-1))-ZR(JGLIM+2*(NUNOE-1))
              GLI2=ZR(JGLIE+2*(NUNOE-1)+1)-ZR(JGLIM+2*(NUNOE-1)+1)
              GLI=SQRT(GLI1**2+GLI2**2)

              GLI1=0.D0
              GLI2=0.D0
              GLI=0.D0
              RTAX=0.D0
              RTAY=0.D0
              RTAZ=0.D0
              RTGX=0.D0
              RTGY=0.D0
              RTGZ=0.D0
              RN=0.D0
              RNX=0.D0
              RNY=0.D0
              RNZ=0.D0

              IF (CONT.EQ.1.D0) THEN
              
C ------ RECUPERATION DES FORCES NODALES DE CONTACT
                CALL ASSERT(ZL(JCONL-1+3*(NUNOE-1)+1))
                CALL ASSERT(ZL(JCONL-1+3*(NUNOE-1)+2))
                CALL ASSERT(ZL(JCONL-1+3*(NUNOE-1)+3))

                 RNX=ZR(JCONT-1+3*(NUNOE-1)+1)
                 RNY=ZR(JCONT-1+3*(NUNOE-1)+2)
                 RNZ=ZR(JCONT-1+3*(NUNOE-1)+3)
                 RN=SQRT(RNX**2+RNY**2+RNZ**2)
                 GLI1=ZR(JGLIE+2*(NUNOE-1))-ZR(JGLIM+2*(NUNOE-1))
                 GLI2=ZR(JGLIE+2*(NUNOE-1)+1)-ZR(JGLIM+2*(NUNOE-1)+1)
                 GLI=SQRT(GLI1**2+GLI2**2)


C ----- NORME DU MULTIPLICATEUR DE LAGRANGE DU FROTTEMENT 

                 LAGSF=SQRT((ZR(JDEPDE-1+6*(NUNOE-1)+5))**2
     &                   +(ZR(JDEPDE-1+6*(NUNOE-1)+6))**2)
                
                 
C ----- Y-A-T-IL DU FROTTEMENT ?

                 IZONE=ZI(JZOCO-1+INO)
                 
                 IF(ZR(JCMCF+6*(IZONE-1)+5).EQ.3.D0) THEN

                CALL ASSERT(ZL(JFROL-1+3*(NUNOE-1)+1))
                CALL ASSERT(ZL(JFROL-1+3*(NUNOE-1)+2))
                CALL ASSERT(ZL(JFROL-1+3*(NUNOE-1)+3))

                    IF (LAGSF.GE.0.999D0) THEN
C ------ LE NOEUD EST GLISSANT

                       RTGX=ZR(JFROT-1+3*(NUNOE-1)+1)
                       RTGY=ZR(JFROT-1+3*(NUNOE-1)+2)
                       RTGZ=ZR(JFROT-1+3*(NUNOE-1)+3)
                       CONT=2.D0
                    ELSE

C ------ LE NOEUD EST ADHERENT

                       RTAX=ZR(JFROT-1+3*(NUNOE-1)+1)
                       RTAY=ZR(JFROT-1+3*(NUNOE-1)+2)
                       RTAZ=ZR(JFROT-1+3*(NUNOE-1)+3)
                    ENDIF
                 ENDIF
              ENDIF
                            

C ------ ARCHIVAGE DES RESULTATS DANS LE CHAM_NO_S CREE

              ZR(JCNSVR-1+19*(NUNOE-1)+1)=CONT
              ZR(JCNSVR-1+19*(NUNOE-1)+3)=RN
              ZR(JCNSVR-1+19*(NUNOE-1)+4)=RNX
              ZR(JCNSVR-1+19*(NUNOE-1)+5)=RNY
              ZR(JCNSVR-1+19*(NUNOE-1)+6)=RNZ
              ZR(JCNSVR-1+19*(NUNOE-1)+7)=GLI1
              ZR(JCNSVR-1+19*(NUNOE-1)+8)=GLI2
              ZR(JCNSVR-1+19*(NUNOE-1)+9)=GLI
              ZR(JCNSVR-1+19*(NUNOE-1)+10)=RTAX
              ZR(JCNSVR-1+19*(NUNOE-1)+11)=RTAY
              ZR(JCNSVR-1+19*(NUNOE-1)+12)=RTAZ
              ZR(JCNSVR-1+19*(NUNOE-1)+13)=RTGX
              ZR(JCNSVR-1+19*(NUNOE-1)+14)=RTGY
              ZR(JCNSVR-1+19*(NUNOE-1)+15)=RTGZ
              ZR(JCNSVR-1+19*(NUNOE-1)+16)=RNX+RTAX+RTGX
              ZR(JCNSVR-1+19*(NUNOE-1)+17)=RNY+RTAY+RTGY
              ZR(JCNSVR-1+19*(NUNOE-1)+18)=RNZ+RTAZ+RTGZ
              ZR(JCNSVR-1+19*(NUNOE-1)+19)=SQRT((RNX+RTAX+RTGX)**2+
     &        (RNY+RTAY+RTGY)**2+(RNZ+RTAZ+RTGZ)**2)

              ZL(JCNSLR-1+ (NUNOE-1)*19+1 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+2 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+3 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+4 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+5 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+6 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+7 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+8 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+9 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+10) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+11) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+12) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+13) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+14) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+15) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+16) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+17) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+18) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+19) = .TRUE.
            ENDIF
           
 40      CONTINUE
      
         ELSE IF (DIM.EQ.2) THEN

C------CALCUL DU JEU AUX NOEUDS EN PRENANT LE MIN DES JEUX AUX NOEUDS,
C------DU GLISSEMENT EN PRENANT LE MAX DU GLISSEMENT AUX NOEUDS
C------DE L INDICATEUR DE CONTACT EN CONSIDERANT QU UN NOEUD EST 
C------EN CONTACT A PARTIR DU MOMENT OU IL L EST SUR AU MOINS 
C------UNE DES MAILLES.

         NTPC=0
         NTMA = ZI(JMAESC)
         DO 50 IMA = 1,NTMA
             POSMA = ZI(JMAESC+3*(IMA-1)+1)
             NUMAES = ZI(JMACO-1+POSMA)
             IZONE  = ZI(JMAESC+3* (IMA-1)+2)
             INTEGR=INT(ZR(JCMCF)+6*(IZONE-1)+1)
             NBNO  = ZI(JMAESC+3* (IMA-1)+3)

        IF (INTEGR.EQ.3 .OR. INTEGR.EQ.4 .OR. INTEGR.EQ.5) THEN
        NBNO=NBNO/2
        ENDIF
        

             DO 60 INO = 1,NBNO
                NUNOE=ZI(ICONEX+ZI(ILONG-1+NUMAES)+INO-2)

C -----VECTEURS DIRECTEURS DU PLAN DE CONTACT 

                VECT1(1)=ZR(JTABF+16*(NTPC+INO-1)+6)
                VECT1(2)=ZR(JTABF+16*(NTPC+INO-1)+7)
                NUMAMA=ZR(JTABF+16*(NTPC+INO-1)+2)

                NUTYP=ZI(IATYMA-1+NUMAMA)
                IF (NUTYP.EQ.2) THEN
                    ALIAS = 'SG2'
                    NNO =2
                    X(1)=ZR(JTABF+16*(NTPC+INO-1)+4)
                ELSE IF (NUTYP.EQ.4) THEN
                    ALIAS = 'SG3'
                    NNO =3
                    X(1)=ZR(JTABF+16*(NTPC+INO-1)+4)
                ELSE
                    CALL UTMESS('F','MMMRES','TYPE DE MAILLE INCONNU')
                ENDIF
                
                CALL CALFFX(ALIAS,X(1),X(2),FF)

                DEPLP(1)=0.D0
                DEPLP(2)=0.D0

C----DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C----POUR LE CALCUL DU GLISSEMENT

                DO 70 I=1,NNO
                   NUNO=ZI(ICONEX+ZI(ILONG-1+NUMAMA)+I-2)
                   CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+1))
                   CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+2))
                   DEPLP(1)=DEPLP(1)+ZR(JDEPDE-1+4*(NUNO-1)+1)*FF(I)
                   DEPLP(2)=DEPLP(2)+ZR(JDEPDE-1+4*(NUNO-1)+2)*FF(I)
   70           CONTINUE
      
C----ECRITURE SUR LES VECTEURS DE TRAVAIL DES JEUX, DU GLISSEMENT ET DE
C----L INDICATEUR DE CONTACT      

                IF (.NOT.ZL(JPREMI-1+NUNOE)) THEN
                   ZL(JPREMI-1+NUNOE)=.TRUE.
                   ZR(JEUTEM+NUNOE-1)=ZR(JJEU-1+NTPC+INO)
                   ZR(JCONTA+NUNOE-1)=ZR(JTABF+16*(NTPC+INO-1)+13)

                   CALL ASSERT(ZL(JDEPDL-1+4*(NUNOE-1)+1))
                   CALL ASSERT(ZL(JDEPDL-1+4*(NUNOE-1)+2))

                   ZR(JGLIE+NUNOE-1)=
     &             ZR(JDEPDE-1+4*(NUNOE-1)+1)*VECT1(1)
     &             +ZR(JDEPDE-1+4*(NUNOE-1)+2)*VECT1(2)

                   ZR(JGLIM+NUNOE-1)=DEPLP(1)*VECT1(1)
     &             +DEPLP(2)*VECT1(2)
     
                ELSE
                   ZR(JEUTEM+NUNOE-1)=MIN(ZR(JEUTEM+NUNOE-1),
     &                             ZR(JJEU-1+NTPC+INO))
     

                   ZR(JCONTA+NUNOE-1)=MAX(ZR(JCONTA+NUNOE-1),
     &                             ZR(JTABF+16*(NTPC+INO-1)+13))
     
                   GLI1=ZR(JDEPDE-1+4*(NUNOE-1)+1)*VECT1(1)
     &           +ZR(JDEPDE-1+4*(NUNOE-1)+2)*VECT1(2)
     &           -DEPLP(1)*VECT1(1)-DEPLP(2)*VECT1(2)

                   GLI=SQRT(GLI1**2)

                   GLIOLD=SQRT((ZR(JGLIE+NUNOE-1)-
     &                         ZR(JGLIM+NUNOE-1))**2)

                   IF ( GLI.GT.GLIOLD ) THEN

                      ZR(JGLIE+NUNOE-1)=
     &                ZR(JDEPDE-1+4*(NUNOE-1)+1)*VECT1(1)
     &                +ZR(JDEPDE-1+4*(NUNOE-1)+2)*VECT1(2)

                      ZR(JGLIM+NUNOE-1)=DEPLP(1)*VECT1(1)
     &                +DEPLP(2)*VECT1(2)
                   ENDIF    
                ENDIF
 60          CONTINUE
             NTPC=NTPC+NBNO
 50      CONTINUE
C ------ BOUCLE SUR TOUS LES NOEUDS DE CONTACT

         DO 80 INO=1,NBNOT
         

C ------ TRAITEMENT DES NOEUDS ESCLAVES SEULEMENT

            IF (ZR(JNOESC+10*(INO-1)+1) .EQ. -1.D0) THEN

C ------ NUMERO ABSOLU DU NOEUD TRAITE

              NUNOE=ZI(JNOCO+INO-1)

              ZR(JCNSVR-1+19*(NUNOE-1)+2)=ZR(JEUTEM+NUNOE-1)
              CONT=ZR(JCONTA+NUNOE-1)

              GLI1=0.D0
              GLI=0.D0
              RTAX=0.D0
              RTAY=0.D0
              RTGX=0.D0
              RTGY=0.D0
              RNX=0.D0
              RNY=0.D0
              RN=0.D0

              IF (CONT.EQ.1.D0) THEN
              
C ------ RECUPERATION DES FORCES NODALES DE CONTACT
                 GLI1=ZR(JGLIE+NUNOE-1)-ZR(JGLIM+NUNOE-1)
                 GLI=SQRT(GLI1**2)
                 RNX=ZR(JCONT-1+2*(NUNOE-1)+1)
                 RNY=ZR(JCONT-1+2*(NUNOE-1)+2)
                 RN=SQRT(RNX**2+RNY**2)

C ----- NORME DU MULTIPLICATEUR DE LAGRANGE DU FROTTEMENT 
                 LAGSF =ABS(ZR(JDEPDE-1+4*(NUNOE-1)+4))
C ----- Y-A-T-IL DU FROTTEMENT ?

                 IZONE=ZI(JZOCO-1+INO)
                 
                 IF(ZR(JCMCF+6*(IZONE-1)+5).EQ.3.D0) THEN
                    IF (LAGSF.GE.0.999D0) THEN
C ------ LE NOEUD EST GLISSANT
                       RTGX=ZR(JFROT-1+2*(NUNOE-1)+1)
                       RTGY=ZR(JFROT-1+2*(NUNOE-1)+2)
                       CONT=2.D0
                    ELSE
C ------ LE NOEUD EST ADHERENT
                       RTAX=ZR(JFROT-1+2*(NUNOE-1)+1)
                       RTAY=ZR(JFROT-1+2*(NUNOE-1)+2)
                    ENDIF
                 ENDIF
              ENDIF
                            
C ------ ARCHIVAGE DES RESULTATS DANS LE CHAM_NO_S CREE
              ZR(JCNSVR-1+19*(NUNOE-1)+1)=CONT
              ZR(JCNSVR-1+19*(NUNOE-1)+3)=RN
              ZR(JCNSVR-1+19*(NUNOE-1)+4)=RNX
              ZR(JCNSVR-1+19*(NUNOE-1)+5)=RNY
              ZR(JCNSVR-1+19*(NUNOE-1)+7)=GLI1
              ZR(JCNSVR-1+19*(NUNOE-1)+9)=GLI
              ZR(JCNSVR-1+19*(NUNOE-1)+10)=RTAX
              ZR(JCNSVR-1+19*(NUNOE-1)+11)=RTAY
              ZR(JCNSVR-1+19*(NUNOE-1)+13)=RTGX
              ZR(JCNSVR-1+19*(NUNOE-1)+14)=RTGY
              ZR(JCNSVR-1+19*(NUNOE-1)+16)=RNX+RTAX+RTGX
              ZR(JCNSVR-1+19*(NUNOE-1)+17)=RNY+RTAY+RTGY
              ZR(JCNSVR-1+19*(NUNOE-1)+19)=SQRT((RNX+RTAX+RTGX)**2+
     &        (RNY+RTAY+RTGY)**2)

              ZL(JCNSLR-1+ (NUNOE-1)*19+1 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+2 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+3 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+4 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+5 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+7 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+9 ) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+10) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+11) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+13) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+14) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+16) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+17) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*19+19) = .TRUE.
            ENDIF
           
 80      CONTINUE
      
         ELSE
         CALL UTMESS('F','MMMRES','DIMENSION DU PROBLEME INCONNU')
         ENDIF
      ELSE
         CALL UTMESS('F','MMMRES','ERREUR DANS LA PROGRAMMATION, '//
     &        'CETTE ROUTINE NE DOIT ETRE APPELE QUE DANS LE CAS '//
     &        'DE LA METHODE CONTINUE DU CONTACT') 
      
      ENDIF
      
      CALL JEDETR(FCONT)
      CALL DETRSD('CHAMP',FCONTS)
      CALL DETRSD('CHAMP',FCTCN)
      CALL JEDETR(FFROT)
      CALL DETRSD('CHAMP',FFROTS)
      CALL DETRSD('CHAMP',FFROCN)
      CALL DETRSD('CHAMP',DEPDES)
      CALL DETRSD('CHAMP',DEPCN)
      CALL JEDETR(JEU)
      CALL JEDETR(GLIE)
      CALL JEDETR(GLIM)
      CALL JEDETR(CONTAC)
      CALL JEDETR(PREMIE)
      
 9999 CONTINUE
      CALL JEDEMA()
      END
