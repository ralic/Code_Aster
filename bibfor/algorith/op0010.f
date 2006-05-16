      SUBROUTINE OP0010(IER)
      IMPLICIT NONE
      INTEGER           IER
   
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/05/2006   AUTEUR MASSIN P.MASSIN 
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
C RESPONSABLE MASSIN P.MASSIN
C     ------------------------------------------------------------------
C                       OPERATEUR PROPA_XFEM :
C     CALCUL DE LA FISSURE APRES PROPAGATION AU PAS DE TEMPS SUIVANT
C     ------------------------------------------------------------------
C     OUT : IER = NOMBRE D'ERREURS RENCONTREES
C     ------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32    JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      
      INTEGER        I,IFM,NIV,IBID,IRET,NDIM,ADDIM,JFISS,JMAIL,JCARAF,
     &               JCMCF,NMAEN1,NMAEN2,NMAEN3
      REAL*8         LCMIN,CFLPRO,DELTAT,EPS,RAYON,PFI(3),VOR(3),
     &               ORI(3),NORME
      CHARACTER*8    K8B,MODEL,NOMA,FISS,FISPRE,ALGOLA
      CHARACTER*16   K16B
      CHARACTER*19   CNSVT,CNSVN,CNSLC,
     &               GRLT,GRLN,CNSLT,CNSLN,CNSEN,CNSBAS,CNSENR
      CHARACTER*24   OBJMA,CHFOND
      COMPLEX*16     CBID
      
      INTEGER       CLSM,JMA,IMA,ITYPMA,AR(12,2),NBAR,IA,NA,NB,NUNOA,
     &              NUNOB,JLNSV,JLTSV,JCONX1,JCONX2,NMAABS,NBMA
      CHARACTER*19  MAI
      CHARACTER*8   TYPMA
      REAL*8        D,LSNA,LSNB,CRILSN,LSTA,LSTB,CRILST
      PARAMETER     (CRILSN=1.D-2, CRILST=1.D-3)
      REAL*8        R8PREM

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
      
      CALL GETRES(FISS,K16B,K16B)

C  RECUPERATION DU MODELE
      CALL GETVID(' ','MODELE',1,1,1,MODEL,IBID)

C  RECUPERATION DU MAILLAGE ET SES CARACTERISTIQUES
      OBJMA = MODEL//'.MODELE    .NOMA'
      CALL JEVEUO(OBJMA,'L',JMAIL)
      NOMA = ZK8(JMAIL)
      
      CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
      NDIM=ZI(ADDIM-1+6)

      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)

C  RECUPERATION DE LA FISSURE PRECEDENTE ET DE SES CARACTERISTIQUES
      CALL JEVEUO(MODEL//'.FISS','L',JFISS)
      FISPRE = ZK8(JFISS)
      
      CNSLT = '&&OP0010.CNSLT'
      CNSLN = 'OP0010.CNSLN'
      GRLT = 'OP0010.GRLT'
      GRLN = 'OP0010.GRLN'
      CALL CNOCNS(FISPRE//'.LTNO','V',CNSLT)
      CALL CNOCNS(FISPRE//'.LNNO','V',CNSLN)
      CALL CNOCNS(FISPRE//'.GRLTNO','V',GRLT)
      CALL CNOCNS(FISPRE//'.GRLNNO','V',GRLN)

      CALL JEDUPO(FISPRE//'.CARAFOND','G',FISS//'.CARAFOND',.FALSE.)
      CALL JEVEUO(FISS//'.CARAFOND','L',JCARAF)
      RAYON = ZR(JCARAF)
      IF (NDIM.EQ.3) THEN
         DO 100 I=1,3
            PFI(I) = ZR(JCARAF+I)
            VOR(I) = ZR(JCARAF+3+I)
            ORI(I) = ZR(JCARAF+6+I)
 100     CONTINUE
      ENDIF

C  RECUPERATION DES DONNEES DE CONTACT
      CALL JEDUPO(FISPRE//'.CONTACT.CARACF','G',FISS//'.CONTACT.CARACF',
     &            .FALSE.)
      CALL JEDUPO(FISPRE//'.CONTACT.ECPDON','G',FISS//'.CONTACT.ECPDON',
     &            .FALSE.)
      CALL JEDUPO(FISPRE//'.CONTACT.METHCO','G',FISS//'.CONTACT.METHCO',
     &            .FALSE.)
      CALL JEDUPO(FISPRE//'.CONTACT.XFEM','G',FISS//'.CONTACT.XFEM',
     &            .FALSE.)
      CALL JEVEUO(FISS//'.CONTACT.CARACF','L',JCMCF)

      IF ((ZR(JCMCF-1+9)).EQ.(0.D0)) THEN
         ALGOLA = 'NON'
      ELSEIF ((ZR(JCMCF-1+9)).EQ.(1.D0)) THEN
         ALGOLA = 'VERSION1'
      ELSEIF ((ZR(JCMCF-1+9)).EQ.(2.D0)) THEN
         ALGOLA = 'VERSION2'
      ENDIF

C-----------------------------------------------------------------------
C     CALCUL DES CHAM_NO_S DES VITESSES DE PROPAGATION
C-----------------------------------------------------------------------
      IF (NIV.GT.1)
     & WRITE(IFM,*)'OP0010-1) EXTENSION DU CHAMP DE VITESSE AUX NOEUDS'

      CNSVT='&&OP0010.CNSVT'
      CNSVN='&&OP0010.CNSVN'

      CALL XPRVIT(NOMA,FISPRE,CNSVT,CNSVN)
      
C-----------------------------------------------------------------------
C     CALCUL DES LONGUEURS CARACTERISTIQUES ET DES CONDITIONS CFL
C       AVEC L'OPTION DE CALCUL 'CFL_XFEM'
C-----------------------------------------------------------------------
      IF (NIV.GT.1)  WRITE(IFM,*)'OP0010-2) CALCUL DES CONDITIONS CFL '
     &            //'ET LONGUEURS CARACTERISTIQUES'

      CNSLC='&&OP0010.CNSLC'
      
      CALL XPRCFL(MODEL,CNSVT,CFLPRO,LCMIN,CNSLC)
      
C-----------------------------------------------------------------------
C     AJUSTEMENT DE VT POUR EVITER LA MODIFICATION DE LA FISSURE
C      EXISTANTE
C-----------------------------------------------------------------------
      IF (NIV.GT.1)
     & WRITE(IFM,*)'OP0010-3) AJUSTEMENT DU CHAMP DES VITESSES VN'

      DELTAT=CFLPRO
      EPS=1.0D-2
      CALL XPRAJU(NOMA,CNSLT,CNSVT,CNSVN,DELTAT,EPS)

C-----------------------------------------------------------------------
C     PROPAGATION DES LEVEL SETS
C-----------------------------------------------------------------------
      IF (NIV.GT.1)  WRITE(IFM,*)'OP0010-4) PROPAGATION DES LEVEL SETS'

      CALL XPRLS(MODEL,NOMA,CNSLN,CNSLT,GRLN,GRLT,CNSVT,CNSVN,DELTAT)

      CALL JEDETR(CNSVT)
      CALL JEDETR(CNSVN)
     
C-----------------------------------------------------------------------
C     REINITIALISATION DE LA LEVEL SET NORMALE
C-----------------------------------------------------------------------
C     POUR L'INSTANT, CETTE PARTIE NE FONCTIONNE QU'AVEC DU K1 PUR,
C   ON LA DESACTIVE CAR ELLE EST SENSEE CONVERGER DES LA 1ERE ITERATION
C-----------------------------------------------------------------------
      IF (NIV.GT.1)  WRITE(IFM,*)'OP0010-5) REINITIALISATION DE LSN'

      DELTAT=LCMIN*5.0D-2
C      CALL XPRINI(MODEL,NOMA,CNSLN,GRLN,DELTAT,LCMIN,CNSLC)

C-----------------------------------------------------------------------
C     REORTHOGONALISATION DE LA LEVEL SET TANGENTE
C-----------------------------------------------------------------------
C     POUR L'INSTANT, CETTE PARTIE NE FONCTIONNE QU'AVEC DU K1 PUR,
C   ON LA DESACTIVE CAR ELLE EST SENSEE CONVERGER DES LA 1ERE ITERATION
C-----------------------------------------------------------------------
      IF (NIV.GT.1)  WRITE(IFM,*)'OP0010-6) REORTHOGONALISATION DE LST'

C      CALL XPRORT(MODEL,NOMA,CNSLN,CNSLT,GRLN,GRLT,DELTAT,LCMIN,CNSLC)

C-----------------------------------------------------------------------
C     REINITIALISATION DE LA LEVEL SET TANGENTE
C-----------------------------------------------------------------------
C     POUR L'INSTANT, CETTE PARTIE NE FONCTIONNE QU'AVEC DU K1 PUR,
C   ON LA DESACTIVE CAR ELLE EST SENSEE CONVERGER DES LA 1ERE ITERATION
C-----------------------------------------------------------------------
      IF (NIV.GT.1)  WRITE(IFM,*)'OP0010-7) REINITIALISATION DE LST'

C      CALL XPRINI(MODEL,NOMA,CNSLT,GRLT,DELTAT,LCMIN,CNSLC)

      CALL JEDETR(CNSLC)
      
C-----------------------------------------------------------------------
C     REAJUSTEMENT DES LEVEL SETS TROP PROCHES DE 0
C-----------------------------------------------------------------------
     
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8B,IRET)
      
      CALL JEVEUO(CNSLN//'.CNSV','E',JLNSV)
      CALL JEVEUO(CNSLT//'.CNSV','E',JLTSV)
      
C     COMPTEUR DES LSN ET LST MODIFIÉES
      CLSM=0
      MAI=NOMA//'.TYPMAIL'
      CALL JEVEUO(MAI,'L',JMA)

C     BOUCLE SUR TOUTES LES MAILLES DU MAILLAGE
      DO 200 IMA=1,NBMA
        NMAABS=IMA
        ITYPMA=ZI(JMA-1+IMA)
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)

C       BOUCLE SUR LES ARETES DE LA MAILLE VOLUMIQUE
        CALL CONARE(TYPMA,AR,NBAR)
        IF (NBAR .GT. 0) THEN
         DO 210 IA=1,NBAR
          NA=AR(IA,1)
          NB=AR(IA,2)
          NUNOA=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NA-1)
          NUNOB=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NB-1)

          LSNA=ZR(JLNSV-1+(NUNOA-1)+1)
          LSNB=ZR(JLNSV-1+(NUNOB-1)+1)   

          IF (ABS(LSNA-LSNB).GT.R8PREM()) THEN
            D=LSNA/(LSNA-LSNB)
            IF (ABS(D).LE.CRILSN) THEN
C              RÉAJUSTEMENT DE LSNA
               ZR(JLNSV-1+(NUNOA-1)+1)=0.D0
               CLSM=CLSM+1
            ENDIF
            IF (ABS(D-1.D0).LE.(CRILSN)) THEN
C              RÉAJUSTEMENT DE LSNB
               ZR(JLNSV-1+(NUNOB-1)+1)=0.D0
               CLSM=CLSM+1
            ENDIF
          ENDIF 

          LSTA=ZR(JLTSV-1+(NUNOA-1)+1)
          LSTB=ZR(JLTSV-1+(NUNOB-1)+1)    

          IF (ABS(LSTA-LSTB).GT.R8PREM()) THEN          
            D=LSTA/(LSTA-LSTB)
            IF (ABS(D).LE.CRILST) THEN
C              RÉAJUSTEMENT DE LSTA
               ZR(JLTSV-1+(NUNOA-1)+1)=0.D0
               CLSM=CLSM+1
            ENDIF
            IF (ABS(D-1.D0).LE.(CRILST)) THEN
C              RÉAJUSTEMENT DE LSTB
               ZR(JLTSV-1+(NUNOB-1)+1)=0.D0
               CLSM=CLSM+1
            ENDIF
          ENDIF
 210     CONTINUE
        ENDIF
 200  CONTINUE


C-----------------------------------------------------------------------
C     EXTENSION DES LEVEL SETS AUX NOEUDS MILIEUX
C-----------------------------------------------------------------------

      CALL XPRMIL(NOMA,CNSLT,CNSLN)
      
C-----------------------------------------------------------------------
C    FIN DE LA PARTIE PROPAGATION ;
C    LA SD FISS_XFEM EST ENRICHIE COMME DANS OP0041 : DEFI_FISS_XFEM
C-----------------------------------------------------------------------
      IF (NIV.GT.1)
     & WRITE(IFM,*)'OP0010-8) ENRICHISSEMENT DE LA SD FISS_XFEM'
      
      CALL CNSCNO(CNSLT,' ','NON','G',FISS//'.LTNO')
      CALL CNSCNO(CNSLN,' ','NON','G',FISS//'.LNNO')
      CALL CNSCNO(GRLT,' ','NON','G',FISS//'.GRLTNO' )
      CALL CNSCNO(GRLN,' ','NON','G',FISS//'.GRLNNO' )

C-----------------------------------------------------------------------
C     CALCUL DE L'ENRICHISSEMENT ET DES POINTS DU FOND DE FISSURE
C-----------------------------------------------------------------------

      CNSEN='&&OP0010.CNSEN'
      CNSENR='&&OP0010.CNSENR'
      IF (NDIM .EQ. 3) THEN
        CALL NORMEV(VOR,NORME)
        IF (NORME.LT.1.D-10) CALL UTMESS('F','OP0041','LA NORME '//
     &                              'DU VECTEUR VECT_ORIE EST NULLE')
        CALL XNRCH3(IFM,NIV,NOMA,CNSLT,CNSLN,CNSEN,CNSENR,PFI,VOR,ORI,
     &              RAYON,FISS,NMAEN1,NMAEN2,NMAEN3)
      ELSEIF (NDIM .EQ. 2) THEN
        CALL XNRCH2(IFM,NIV,NOMA,CNSLT,CNSLN,CNSEN,CNSENR,
     &              RAYON,FISS,NMAEN1,NMAEN2,NMAEN3)
      ENDIF
      
      CALL CNSCNO(CNSENR,' ','NON','G',FISS//'.STNOR')
      CALL CNSCNO(CNSEN,' ','NON','G',FISS//'.STNO')

C-----------------------------------------------------------------------
C     CALCUL DE LA BASE LOCALE AU FOND DE FISSURE
C-----------------------------------------------------------------------

      CHFOND = FISS//'.FONDFISS'
      CNSBAS='&&OP0010.CNSBAS'
      IF (NDIM .EQ. 3) THEN
        CALL XBASLO(MODEL,NOMA,CHFOND,GRLT,GRLN,CNSBAS)
      ELSEIF (NDIM .EQ. 2) THEN
        CALL XBASL2(MODEL,NOMA,CHFOND,GRLT,GRLN,CNSBAS)
      ENDIF

      CALL CNSCNO(CNSBAS,' ','NON','G',FISS//'.BASLOC')
      CALL DETRSD('CHAM_NO_S',CNSBAS)

C-----------------------------------------------------------------------
C     STRUCTURE DE DONNEES SUR LE CONTACT
C-----------------------------------------------------------------------

C     SEULEMENT S'IL Y A DES MAILLES DE CONTACT
      IF (NMAEN1+NMAEN2+NMAEN3.GT.0) THEN
C       APPEL À L'ALGORITHME DE RESTRICTION DE L'ESPACE DES
C       MULTIPLICATUERS DE LAGRANGE DE CONTACT
        IF (ALGOLA.NE.'NON') THEN 
          WRITE(IFM,*)'ACTIVATION DE L''ALGORITHME DE RESTRICTION DE '//
     &                'L''ESPACE DES MULTIPLICATEURS DE '//
     &                'PRESSION DE CONTACT'
          CALL XLAGSP(ALGOLA,NDIM,NOMA,CNSLT,CNSLN,GRLN,GRLT,FISS)
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      IF (NIV.GT.1)  WRITE(IFM,*)'OP0010-9) FIN'
    
      CALL JEDEMA()
      END
