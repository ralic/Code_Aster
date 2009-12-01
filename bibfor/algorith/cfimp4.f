      SUBROUTINE CFIMP4(DEFICO,RESOCO,NOMA,IFM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/01/2009   AUTEUR DESOZA T.DESOZA 
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
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT     NONE
      CHARACTER*24 DEFICO
      CHARACTER*24 RESOCO
      INTEGER      IFM
      CHARACTER*8  NOMA
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
C
C IMPRESSION DES INFOS DETAILLES DE L'APPARIEMENT
C
C ----------------------------------------------------------------------
C
C IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      CFMMVD,ZAPPA,ZAPME
      CHARACTER*24 APPARI,APJEU,APMEMO
      INTEGER      JAPPAR,JAPJEU,JAPMEM
      CHARACTER*24 APPOIN,NORMCO,TANGCO,APCOEF
      INTEGER      JAPPTR,JNORMO,JTANGO,JAPCOE
      CHARACTER*24 APCOFR,PNOMA,NOMACO,COMAFO,FROTE,PENAL,TANINI
      INTEGER      JAPCOF,JPONO,JNOMA,JCOMA,JFRO,JPENA,JTGINI
      CHARACTER*24 NOZOCO,APDDL,PDDLCO
      INTEGER      JZOCO,JAPDDL,JPDDL
      INTEGER      IBID,TYPALF,FROT3D,CFDISI
      INTEGER      NZOCO,NESMAX,NESCL,NNOCO,NDIM
      INTEGER      K,IZONE,INOEUD,IESCL,IDDLE,IMAIT,IDDLM
      INTEGER      POSNOC,POSNOE,POSAPM,POSMAI,POSMA2
      CHARACTER*8  NOMNOC,NOMNOE,NOMMAI,NOMMAM,NOMNOM,NOMMA2
      INTEGER      JDECE,JDECM
      INTEGER      CODRET
      INTEGER      NDDLT,NDDLE,NDDLM,NBNOM
      CHARACTER*20 TYPNOC
      REAL*8       COEFRO,COEFPN,COEFPT,COEFTE,R8BID
      REAL*8       TAU1(3),TAU2(3),NORM(3)
      LOGICAL      CFCALD,LNODAL
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INFOS SUR LA CHARGE DE CONTACT
C
      CALL CFDISC(DEFICO,' ',IBID,TYPALF,FROT3D,IBID)
C
      APPARI = RESOCO(1:14)//'.APPARI'
      APDDL  = RESOCO(1:14)//'.APDDL'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      PDDLCO = RESOCO(1:14)//'.PDDLCO'
      APJEU  = RESOCO(1:14)//'.APJEU'
      APMEMO = RESOCO(1:14)//'.APMEMO'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      TANINI = DEFICO(1:16)//'.TANINI'
      NORMCO = RESOCO(1:14)//'.NORMCO'
      TANGCO = RESOCO(1:14)//'.TANGCO'
      NOMACO = DEFICO(1:16)//'.NOMACO'
      PNOMA  = DEFICO(1:16)//'.PNOMACO'
      PENAL  = DEFICO(1:16)//'.PENAL' 
      COMAFO = DEFICO(1:16)//'.COMAFO'
      FROTE  = DEFICO(1:16)//'.FROTE'           
      NOZOCO = DEFICO(1:16)//'.NOZOCO'    
C
      CALL JEVEUO(NOZOCO,'L',JZOCO ) 
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(APJEU, 'L',JAPJEU)
      CALL JEVEUO(APMEMO,'L',JAPMEM)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(TANINI,'L',JTGINI)
      CALL JEVEUO(PDDLCO,'L',JPDDL )
      CALL JEVEUO(APDDL ,'L',JAPDDL)
      CALL JEVEUO(NORMCO,'L',JNORMO)
      CALL JEVEUO(TANGCO,'L',JTANGO)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(PNOMA ,'L',JPONO )
      CALL JEVEUO(NOMACO,'L',JNOMA )
      CALL JEVEUO(COMAFO,'L',JCOMA )
      CALL JEVEUO(FROTE ,'L',JFRO  ) 
      CALL JEVEUO(PENAL ,'L',JPENA )           
      IF (TYPALF.NE.0) THEN
        APCOFR = RESOCO(1:14)//'.APCOFR'
        CALL JEVEUO(APCOFR,'L',JAPCOF)
      ENDIF
C
      ZAPME  = CFMMVD('ZAPME')
      ZAPPA  = CFMMVD('ZAPPA')
C
C --- INITIALISATIONS
C
      IZONE  = 0
      NDIM   = CFDISI(DEFICO,'NDIM',IZONE)                
      NZOCO  = CFDISI(DEFICO,'NZOCO',IZONE) 
      NESMAX = CFDISI(DEFICO,'NESMAX',IZONE)
      NESCL  = ZI(JAPPAR)
      NNOCO  = CFDISI(DEFICO,'NNOCO',IZONE)
C
      WRITE(IFM,*) '<CONTACT_DVLP> *** APPARIEMENT *** '
C
C ----------------------------------------------------------------------
C --- INFOS SUR LES ZONES DE CONTACT
C ----------------------------------------------------------------------
C
      WRITE(IFM,*) '<CONTACT_DVLP> ------ ZONES ------ '

      WRITE(IFM,1000) NZOCO
      WRITE(IFM,1001) NESMAX
      WRITE(IFM,1002) NESCL
C
C ----------------------------------------------------------------------
C --- INFOS SUR TOUS LES NOEUDS
C ----------------------------------------------------------------------
C
      WRITE(IFM,*) '<CONTACT_DVLP> ------ NOEUDS DE CONTACT ------ '

      DO 30 INOEUD = 1,NNOCO
        POSNOC = INOEUD
        IZONE  = ZI(JZOCO+POSNOC-1)       
        CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNOC,NOMNOC,
     &              CODRET)
     
        IF (ZI(JAPMEM+ZAPME*(INOEUD-1)).EQ.1) THEN
           TYPNOC = 'ESCLAVE             '
        ELSE IF (ZI(JAPMEM+ZAPME*(INOEUD-1)).EQ.0) THEN
           TYPNOC = 'MAITRE              '
        ELSE IF (ZI(JAPMEM+ZAPME*(INOEUD-1)).EQ.-1) THEN
           TYPNOC = 'EXCLU (SANS_NOEUD)  '
        ELSE IF (ZI(JAPMEM+ZAPME*(INOEUD-1)).EQ.-2) THEN
           TYPNOC = 'EXCLU (PIVOT NUL)   '
        ELSE IF (ZI(JAPMEM+ZAPME*(INOEUD-1)).EQ.-3) THEN
           TYPNOC = 'EXCLU (HORS ZONE)   '           
        ELSE
           TYPNOC = 'ETAT INCONNU        '  
        ENDIF
        
        IF (CODRET.LT.0) THEN
          NOMNOC = 'ERREUR'
        ENDIF

        WRITE(IFM,3000) INOEUD,NOMNOC,TYPNOC
        

        TAU1(1) = ZR(JTGINI+6*(POSNOC-1)+1-1)   
        TAU1(2) = ZR(JTGINI+6*(POSNOC-1)+2-1) 
        TAU1(3) = ZR(JTGINI+6*(POSNOC-1)+3-1) 
        TAU2(1) = ZR(JTGINI+6*(POSNOC-1)+4-1)
        TAU2(2) = ZR(JTGINI+6*(POSNOC-1)+5-1)
        TAU2(3) = ZR(JTGINI+6*(POSNOC-1)+6-1)
  
        IF (TYPNOC.EQ.'MAITRE') THEN
          IF (CFCALD(DEFICO,IZONE ,'MAIT')) THEN
            WRITE(IFM,3003) (TAU1(K),K=1,3)
            IF (NDIM.EQ.3) THEN
              WRITE(IFM,3005) (TAU2(K),K=1,3)
            ENDIF  
            CALL MMNORM(NDIM,TAU1,TAU2,NORM,R8BID)
            WRITE(IFM,3006) (NORM(K),K=1,3)         
          ELSE
            WRITE(IFM,3007)              
          ENDIF  
        ELSE
          IF (CFCALD(DEFICO,IZONE ,'ESCL')) THEN
            WRITE(IFM,3003) (TAU1(K),K=1,3)
            IF (NDIM.EQ.3) THEN
              WRITE(IFM,3005) (TAU2(K),K=1,3)
            ENDIF            
            CALL MMNORM(NDIM,TAU1,TAU2,NORM,R8BID)  
            WRITE(IFM,3006) (NORM(K),K=1,3)
          ELSE
            WRITE(IFM,3007)    
          ENDIF                  
        ENDIF
        
        
3000  FORMAT (' <CONTACT_DVLP> NOEUD NUMERO ',I6,' (',A8,') -> NOEUD ',
     &           A20)
3003  FORMAT (' <CONTACT_DVLP>  * TANGENTE 1  : ',3(1PE15.8,2X))
3005  FORMAT (' <CONTACT_DVLP>  * TANGENTE 2  : ',3(1PE15.8,2X))
3006  FORMAT (' <CONTACT_DVLP>  * NORMALE     : ',3(1PE15.8,2X))
3007  FORMAT (' <CONTACT_DVLP>  * TANGENTE ET NORMALE NON CALCULEES')


  30  CONTINUE
C
C ----------------------------------------------------------------------
C --- INFOS SUR LES NOEUDS ESCLAVES
C ----------------------------------------------------------------------
C
      WRITE(IFM,*) '<CONTACT_DVLP> ----- NOEUDS ESCLAVES ----- '

      DO 40 IESCL = 1,NESCL
C
C --- INFO SUR NOEUD ESCLAVE
C      
        POSNOE = ZI(JAPPAR+ZAPPA*(IESCL-1)+1)
        CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNOE,NOMNOE,
     &              CODRET)
        
        IF (CODRET.LT.0) THEN
          GOTO 40
        ELSE 
          WRITE(IFM,4000) IESCL,NOMNOE
        ENDIF  
C
C --- TYPE D'APPARIEMENT
C        
        POSAPM = ZI(JAPMEM+ZAPME*(POSNOE-1)+2) 
        IF (POSAPM.EQ.0) THEN
          WRITE(IFM,4009)
          GOTO 40
        ELSE
C
C --- ENTITE APPARIEE
C
          POSMAI = ZI(JAPPAR+ZAPPA*(IESCL-1)+2) 
          
          IF (POSMAI.LT.0) THEN
            CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSMAI,NOMMAI,
     &                  CODRET)
            LNODAL = .TRUE.
          ELSEIF (POSMAI.GT.0) THEN
            CALL CFNOMM(NOMA  ,DEFICO,'MAIL',POSMAI,NOMMAI,
     &                  CODRET)
            LNODAL = .FALSE.
          ELSE
            LNODAL = .FALSE.
            CODRET = -1
          ENDIF
          
          
          IF (CODRET.LT.0) THEN
            GOTO 40
          ENDIF

          IF (LNODAL) THEN
            WRITE(IFM,4001) NOMMAI
          ELSE
            WRITE(IFM,4002) NOMMAI  
          ENDIF  
        ENDIF    
        JDECE = ZI(JAPPTR+IESCL-1)
C
C --- NOMBRE DE DDL TOTAL: NDDLT
C
        NDDLT = ZI(JAPPTR+IESCL) - ZI(JAPPTR+IESCL-1)
C
C --- NOMBRE DE DDL POUR NOEUD ESCLAVE
C
        NDDLE = ZI(JPDDL+POSNOC) - ZI(JPDDL+POSNOC-1)
C
C --- AFFICHAGES
C
        WRITE(IFM,4004) NDDLT,NDDLE
        WRITE(IFM,4006) ZR(JAPJEU+IESCL-1)
        WRITE(IFM,4007) (ZR(JNORMO+3*(IESCL-1)+K-1),K=1,3)
        WRITE(IFM,4008) (ZR(JTANGO+6*(IESCL-1)+K-1),K=1,3)
        IF (NDIM.EQ.3) THEN
          WRITE(IFM,5008) (ZR(JTANGO+6*(IESCL-1)+K-1+3),K=1,3)
        ENDIF
C ----------------------------------------------------------------------
C --- PARAMETRES PENALISATION
C ----------------------------------------------------------------------

        COEFRO = ZR(JFRO-1+IESCL)     
        COEFPN = ZR(JPENA-1+2*IESCL-1) 
        COEFPT = ZR(JPENA-1+2*IESCL)  
        COEFTE = ZR(JCOMA-1+IESCL)         
 
 
        WRITE(IFM,7000) COEFPN
        WRITE(IFM,7001) COEFPT
        WRITE(IFM,7002) COEFTE
        WRITE(IFM,7003) COEFRO
        
C
C ----------------------------------------------------------------------
C --- DDL ET COEF POUR NOEUD ESCLAVE
C ----------------------------------------------------------------------
C
C
C --- COEFFICIENTS POUR CONTACT
C
        IF (NDIM.EQ.3) THEN
          WRITE (IFM,4015) NOMNOE,
     &      (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NDDLE),
     &      (ZR(JAPCOE+JDECE+IDDLE-1),IDDLE=1,NDDLE)
        ELSE
          WRITE (IFM,4018) NOMNOE,
     &      (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NDDLE),
     &      (ZR(JAPCOE+JDECE+IDDLE-1),IDDLE=1,NDDLE)        
        ENDIF
C
C --- COEFFICIENTS POUR FROTTEMENT 
C
        IF (TYPALF.NE.0) THEN
          IF (NDIM.EQ.3) THEN
            WRITE (IFM,4016) NOMNOE,
     &        (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NDDLE),
     &        (ZR(JAPCOF+JDECE+IDDLE-1),IDDLE=1,NDDLE)
            WRITE (IFM,4017) NOMNOE,
     &        (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NDDLE),
     &        (ZR(JAPCOF+30*NESMAX+JDECE+IDDLE-1),IDDLE=1,NDDLE)     
          ELSE
            WRITE (IFM,4019) NOMNOE,
     &        (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NDDLE),
     &        (ZR(JAPCOF+JDECE+IDDLE-1),IDDLE=1,NDDLE)        
          ENDIF
        ENDIF
C
C ----------------------------------------------------------------------
C --- DDL ET COEF POUR NOEUDS MAITRES
C ----------------------------------------------------------------------
C
        JDECM  = JDECE + NDDLE
C 
C --- APPARIEMENT NODAL
C
        IF (LNODAL) THEN  
          NOMNOM = NOMMAI
          NBNOM  = 1
          IMAIT  = 1
          NDDLM  = NDDLT - NDDLE
C
C --- COEFFICIENTS POUR CONTACT
C          
          IF (NDIM.EQ.3) THEN
            WRITE (IFM,5011) NOMNOM,
     &        (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &        (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM) 
          ELSE
            WRITE (IFM,6011) NOMNOM,
     &        (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &        (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
          ENDIF
C
C --- COEFFICIENTS POUR FROTTEMENT 
C
          IF (TYPALF.NE.0) THEN
            IF (NDIM.EQ.3) THEN
              WRITE (IFM,5012) NOMNOM,
     &         (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &         (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
              WRITE (IFM,5013) NOMNOM,
     &          (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),
     &                        IDDLM=1,NDDLM),
     &          (ZR(JAPCOF+30*NESMAX+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),
     &                        IDDLM=1,NDDLM)
            ELSE
              WRITE (IFM,6012) NOMNOM,
     &         (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &         (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
            ENDIF
          ENDIF
C 
C --- APPARIEMENT MAITRE/ESCLAVE
C
        ELSE
          NOMMAM = NOMMAI
          NBNOM  = ZI(JPONO+POSMAI) - ZI(JPONO+POSMAI-1)
          DO 50 IMAIT = 1,NBNOM    
            POSMA2 = ZI(JNOMA+ZI(JPONO+POSMAI-1)+IMAIT-1)
            NDDLM  = ZI(JPDDL+POSMA2) - ZI(JPDDL+POSMA2-1)
            CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSMA2,NOMMA2,
     &                  CODRET)
            IF (CODRET.LT.0) THEN
              GOTO 40
            ENDIF
             
C
C --- COEFFICIENTS POUR CONTACT
C             
            IF (NDIM.EQ.3) THEN
              WRITE (IFM,5001) NOMMAM,NOMMA2,
     &        (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &        (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
            ELSE
              WRITE (IFM,6001) NOMMAM,NOMMA2,
     &        (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &        (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
            ENDIF
C
C --- COEFFICIENTS POUR FROTTEMENT 
C
            IF (TYPALF.NE.0) THEN
              IF (NDIM.EQ.3) THEN            
                WRITE (IFM,5002) NOMMAM,NOMMA2,
     &         (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &         (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
                WRITE (IFM,5003) NOMMAM,NOMMA2,
     &            (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),
     &                        IDDLM=1,NDDLM),
     &            (ZR(JAPCOF+30*NESMAX+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),
     &                        IDDLM=1,NDDLM)
              ELSE
                WRITE (IFM,6002) NOMMAM,NOMMA2,
     &         (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &         (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
              ENDIF
            ENDIF
   50     CONTINUE

        ENDIF
     
  40  CONTINUE
C
1000  FORMAT (' <CONTACT_DVLP> NOMBRE DE ZONES DE CONTACT: ',I6)
1001  FORMAT (' <CONTACT_DVLP> NOMBRE MAXIMAL DE NOEUDS ESCLAVES: ',I6)
1002  FORMAT (' <CONTACT_DVLP> NOMBRE EFFECTIF DE NOEUDS ESCLAVES: ',I6)

4000  FORMAT (' <CONTACT_DVLP> NOEUD ESCLAVE NUMERO ',I6,' (',
     &        A8,')') 

4001  FORMAT (' <CONTACT_DVLP>  * APPARIEMENT AVEC NOEUD  ',A8)
4002  FORMAT (' <CONTACT_DVLP>  * APPARIEMENT AVEC MAILLE ',A8)
4004  FORMAT (' <CONTACT_DVLP>  * NOMBRE DE DDLS : ',I6,' DONT ',I6,
     &                        ' POUR NOEUD ESCLAVE',I6)

4006  FORMAT (' <CONTACT_DVLP>  * JEU: ',1PE15.8)
4007  FORMAT (' <CONTACT_DVLP>  * NORMALE LISSEE/MOYENNEE: ',
     &         3(1PE15.8,2X))
4008  FORMAT (' <CONTACT_DVLP>  * TANGENTE DIRECTION 1   : ',
     &         3(1PE15.8,2X))
5008  FORMAT (' <CONTACT_DVLP>  * TANGENTE DIRECTION 2   : ',
     &         3(1PE15.8,2X))

4009  FORMAT (' <CONTACT_DVLP>  * PREMIER APPARIEMENT ')

C
C --- 3D - ESCLAVE
C
4015  FORMAT ((' <CONTACT_DVLP>  * DDL ESCL. CONTACT ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))

4016  FORMAT ((' <CONTACT_DVLP>  * DDL ESCL. FROT1   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
4017  FORMAT ((' <CONTACT_DVLP>  * DDL ESCL. FROT2   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
     
C
C --- 2D - ESCLAVE
C     
4018  FORMAT ((' <CONTACT_DVLP>  * DDL ESCL. CONTACT ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))

4019  FORMAT ((' <CONTACT_DVLP>  * DDL ESCL. FROT1   ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
   
C
C --- 3D MAITRE/ESCL - MAITRE
C
5001  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. CONTACT ( ',A8,
     &           '/',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5002  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT1   ( ',A8,
     &           '/',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5003  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT2   ( ',A8,
     &           '/',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
C
C --- 3D NODAL - MAITRE
C    
5011  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. CONTACT ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5012  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT1   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5013  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT2   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))  
C
C --- 2D MAITRE/ESCL - MAITRE
C     
6001  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. CONTACT ( ',A8,
     &           '/',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
6002  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT1   ( ',A8,
     &           '/',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
C
C --- 2D NODAL - MAITRE
C
6011  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. CONTACT ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
6012  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT1   ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
C
C --- PENALISATION
C
7000  FORMAT (' <CONTACT_DVLP>  * E_N              :',1PE15.8)
7001  FORMAT (' <CONTACT_DVLP>  * E_T              :',1PE15.8)
7002  FORMAT (' <CONTACT_DVLP>  * COEF_MATR_FROT   :',1PE15.8)
7003  FORMAT (' <CONTACT_DVLP>  * COULOMB          :',1PE15.8)
C
      
C
      CALL JEDEMA()
C
      END
