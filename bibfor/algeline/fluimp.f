      SUBROUTINE FLUIMP(ITYPFL,NIVPAR,NIVDEF,MELFLU,TYPFLU,NUOR,FREQ,
     &                  FREQI,NBM,VITE,NPV,CARAC,CALCUL,AMOC)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 14/06/2005   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C-----------------------------------------------------------------------
C IMPRESSION DANS LE FICHIER RESULTAT DES PARAMETRES DE COUPLAGE
C FLUIDE-STRUCTURE (FREQ,AMOR) ET/OU DES DEFORMEES MODALES
C APPELANT : FLUST1, FLUST2, FLUST3, FLUST4
C TOLE  CRP_20
C-----------------------------------------------------------------------
C  IN : ITYPLF : INDICE CARACTERISANT LE TYPE DE LA CONFIGURATION
C                ETUDIEE
C  IN : NIVPAR : NIVEAU D'IMPRESSION DES PARAMETRES DU COUPLAGE
C                (FREQ,AMOR)
C  IN : NIVDEF : NIVEAU D'IMPRESSION DES DEFORMEES MODALES
C  IN : MELFLU : NOM UTILISATEUR DU CONCEPT MELASFLU
C  IN : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES SELECTIONNES POUR
C                LE COUPLAGE
C  IN : FREQ   : LISTE DES FREQUENCES ET AMORTISSEMENTS REDUITS MODAUX
C                PERTURBES PAR L'ECOULEMENT
C  IN : FREQI  : LISTE DES FREQUENCES MODALES INITIALES
C  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
C  IN : VITE   : LISTE DES VITESSES D'ECOULEMENT ETUDIEES
C  IN : NPV    : NOMBRE DE VITESSES D'ECOULEMENT
C  IN : CARAC   : DIAMETRE HYDRAULIQUE ET EPAISSEUR
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER        ITYPFL,NIVPAR,NIVDEF,NBM,NPV,NUOR(NBM),JVCN,JVEN
      INTEGER        NIVE,NBVAL,LFSVI,PAS,JCONN
      CHARACTER*19   MELFLU
      CHARACTER*8    TYPFLU
      REAL*8         CARAC(2),FREQ(2*NBM*NPV),FREQI(*),VITE(NPV),AMOC(*)
      REAL*8         VRMIN,VRMAX,VRMIN1,VRMIN2,VRMAX1,VRMAX2 
      REAL*8         VMOY,VMOYTO,REDUIT,RAPPOR
C
      INTEGER        NBR,JTRAV1,JTRAV2,JTRAV3,JTRAV4,JVIT1,JVIT2,JZONE
      INTEGER        JTR1,JTR2,JVRZO,LFSVR,JCSTE
      INTEGER        LPROFV, LNOE, IRET, MODUL, MODUL2
      CHARACTER*1    K1BID
      CHARACTER*3    I3
      CHARACTER*8    K8BID,NOMSYM,NOMCMP(6),FORMAR,NUMZO,NBPZON
      CHARACTER*8    XL1,XL2,XL3
      CHARACTER*13   XCOD,XVRED,XFREQ1,XAMOR,XBMIN,XBMAX
      CHARACTER*13   XVMIN,XVMAX,XVMOY
      CHARACTER*30   CHAM30
      CHARACTER*19   CHAM19
      CHARACTER*24   NOM1,NOM2,FSVI,CRAPPO,CREDUI,CCSTE,CVCN,CTRAV
      CHARACTER*100  CHAV11,CHAV12,CHAV13,CHAV21,CHAV22,CHAV23
      CHARACTER*100  CHAV31,CHAV32,CHAV33,CHAV34
      CHARACTER*100  CHAZP1,CHAZV1,CHAZP2,CHAZV2,CHAZP3,CHAZV3
      CHARACTER*100  CHAZP4,CHAZV4,CHAZP5,CHAZV5,CHAZP6,CHAZV6
      CHARACTER*100  CHAZP7,CHAZV7,CHAV40,CHAZ40
      CHARACTER*255  CTRAV1,CTRAV2,CTRAV3
      LOGICAL        LBID,LCOR,LSUP,LINF,LMIN,LMAX,LRESU,CALCUL(2)
C
      DATA           NOMCMP /'DX      ','DY      ','DZ      ',
     &                     'DRX     ','DRY     ','DRZ     '/
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      IFR = IUNIFI('RESULTAT')
C
      NOM1 = '&&COEFAM.CDR2'
      NOM2 = '&&COEFRA.CKR2'
      CHAM30='******************************'

C      
      CHAM19(1:13) = MELFLU(1:8)//'.C01.'
      NOMSYM = 'DEPL_R  '
      FORMAR = '1PE12.5'
      NIVE   = 3
C
      NPASV = NPV
      NBNO  = 0
C
      LBID  = .FALSE.
      LCOR  = .FALSE.
      LSUP  = .FALSE.
      LINF  = .FALSE.
      LMIN  = .FALSE.
      LMAX  = .FALSE.
      LRESU = .FALSE.
C
C
C     VERIFICATION DE LA COHERENCE DES VITESSES REDUITES 
C     ENTRE LES FICHIERS .70 ET .71 - OPTION FAISCEAU-TRANS
C
      IF(ITYPFL.EQ.1 ) THEN
        CALL JEEXIN(NOM1,IRET)
        IF (IRET.NE.0) THEN
           CALL JEVEUO(NOM1,'L',JVIT1)   
           CALL JEVEUO(NOM2,'L',JVIT2)            
           VRMIN1 = ZR(JVIT1-1+1)
           VRMAX1 = ZR(JVIT1-1+2)
           VRMIN2 = ZR(JVIT2-1+1)
           VRMAX2 = ZR(JVIT2-1+2)        
           IF((ABS(VRMIN1-VRMIN2)) .GT. 1.0D-04 .OR.
     &        (ABS(VRMAX1-VRMAX2)) .GT. 1.0D-04) THEN
               CALL UTMESS('F','FLUIMP', 'LES VITESSES REDUITES' //
     &        ' DES FICHIERS .70 ET .71 NE SONT PAS COHERENTES')
           ENDIF
        ENDIF
C
      ENDIF
C
      IF (NIVPAR.EQ.1) THEN
        IF (CALCUL(1)) THEN
        WRITE (IFR,*)
        WRITE (IFR,*) '==============================================='
        WRITE (IFR,*)
        WRITE (IFR,*) ' RESULTAT MODULE COUPLAGE FLUIDE-STRUCTURE'
        WRITE (IFR,*)
        WRITE (IFR,*) 'EVOLUTION DE LA FREQUENCE ET DE L AMORTISSEMENT'
        WRITE (IFR,*) '   EN FONCTION DE LA VITESSE DE L ECOULEMENT'
        WRITE (IFR,*)
        WRITE (IFR,*) '==============================================='
        WRITE (IFR,*)
C
           CALL JEEXIN('&&FLUST1.TEMP.PROFV',IRET)
           IF (IRET.NE.0) THEN
             CALL JELIRA('&&FLUST1.TEMP.PROFV','LONMAX',LPROFV,K1BID)
             LNOE = (LPROFV-1)/2
             CALL JEVEUO('&&FLUST1.TEMP.PROFV','L',JPROFV)
             VMOYTO = ZR(JPROFV-1+LPROFV)
             WRITE (IFR,1001) VMOYTO
             WRITE (IFR,*)
           ENDIF
        ENDIF
        IF (ITYPFL.EQ.1) THEN
           CALL JEVEUO('&&MDCONF.TEMPO','L',JZONE)
           NZONE = ZI(JZONE-1+1) 
           IF(CALCUL(2)) THEN
              CALL JEVEUO(MELFLU(1:8)//'.VEN','L',JVEN)
              CALL JEVEUO(MELFLU(1:8)//'.VCN','L',JVCN)
              CALL JEVEUO(MELFLU(1:8)//'.MASS','L',JCONN)
              
              CALL JEVEUO(TYPFLU//'           .FSVR','L',LFSVR)
              FSVI = TYPFLU//'           .FSVI'
              CALL JEVEUO(FSVI,'L',LFSVI)
              NBVAL=1
              CTRAV2='*'
              DO 50 I=1,NZONE
                 NBVAL=NBVAL*ZI(LFSVI+1+NZONE+I)
                 CTRAV2((2+(30*(I-1))):(2+(30*I)))=CHAM30
  50          CONTINUE

              DO 60 I=1,90
                 CTRAV2((1+(30*NZONE)+I):(1+(30*NZONE)+I))='*'
  60          CONTINUE
                CTRAV2((1+(30*NZONE)+91):(1+(30*NZONE)+91))='*'
              CALL WKVECT('&&FLUIMP.CSTE','V V R',NZONE,JCSTE)
           ENDIF
        ENDIF

C
        DO 10 IM = 1,NBM
          IMOD = NUOR(IM)
          IF(CALCUL(1)) THEN
             WRITE (IFR,1002) IMOD,NBM,FREQI(IMOD)
             WRITE (IFR,*) ' ------------------------------------------'
             WRITE (IFR,*)
C
C ---     ECRITURE DE L'EN-TETE DU TABLEAU POUR CHAQUE MODE
             CHAV11 = '****************'
             CHAV12 = '                '
             CHAV13 = '               *'
             CHAV21 = '    VITESSE    *'
             CHAV22 = '   FREQUENCE   *'
             CHAV23 = ' AMORTISSEMENT *'
             CHAV31 = '    GAP(M/S)   *'
             CHAV32 = '    REDUITE    *'
             CHAV33 = '     (HZ)      *'
             CHAV34 = '       %       *'
          ENDIF
          IF (ITYPFL.EQ.1) THEN
            IF(CALCUL(1)) THEN
C
               CHAZP1 = '****************************'
               CHAZP2 = '                            '
               CHAZP3 = ' VITESSE MOY : '
               CHAZP4 = '----------------------------'
               CHAZP5 = ' NB POINTS DE LA ZONE :'
               CHAZP6 = '     DONT HORS PLAGE :      '
               CHAZP7 = ' <VREDMIN   TOT    >VREDMAX '
C
               CHAZV1 = '*********************************'
               CHAZV2 = '  ZONE  '
               CHAZV3 = '|    PLAGE DE VITESSE REDUITE   *'
               CHAZV4 = '|-------------------------------*'
               CHAZV5 = '|    VREDMIN    |    VREDMAX    *'
               CHAZV6 = '| '
               CHAZV7 = '|               |               *'
C
               CALL WKVECT('&&FLUIMP.TRAV1','V V K80',NZONE,JTRAV1)
               CALL WKVECT('&&FLUIMP.TRAV2','V V K80',NZONE,JTRAV2)
               CALL WKVECT('&&FLUIMP.TRAV3','V V K80',NZONE,JTRAV3)
               CALL WKVECT('&&FLUIMP.TRAV4','V V K80',NZONE,JTRAV4)
C
               CALL JEVEUO('&&COEFMO.VRZO','L',JVRZO)
C
               DO 15 J = 1,NZONE
                 N1 = ZI(JZONE+2*(J-1)+1)
                 N2 = ZI(JZONE+2*(J-1)+2)
C                CONVERSION EN CHAINES DE CARACTERES
                 CALL CODENT(J,'G',NUMZO)
                 CALL CODENT(N2-N1+1,'G',NBPZON)
C
                 VRMIN = ZR(JVRZO+2*(J-1)+0)
                 CALL CODREE(VRMIN,'E',XVMIN)
                 IF (VRMIN.LT.0.D0) THEN
                   XVMIN = '-'//XVMIN
                 ELSE
                   XVMIN = ' '//XVMIN
                 ENDIF
C
                 VRMAX = ZR(JVRZO+2*(J-1)+1)
                 CALL CODREE(VRMAX,'E',XVMAX)   
                 IF (VRMAX.LT.0.D0) THEN
                   XVMAX = '-'//XVMAX
                 ELSE
                   XVMAX = ' '//XVMAX
                 ENDIF
C
                 IF (IRET.NE.0) THEN
                   VMOY  = ZR(JPROFV+LNOE+N1)
                   CALL CODREE(VMOY,'E',XVMOY)
                   IF (VMOY.LT.0.D0) THEN
                     XVMOY = '-'//XVMOY
                   ELSE
                     XVMOY = ' '//XVMOY
                   ENDIF
                 ELSE
                   XVMOY = '      -      '
                 ENDIF
C
                 ZK80(JTRAV1+J-1) = CHAZV2(1:8)//NUMZO(1:2)//
     &                           '                      *'
                 ZK80(JTRAV2+J-1) = CHAZP3(1:15)//XVMOY//'*'
                 ZK80(JTRAV3+J-1) = CHAZP5(1:23)//NBPZON(1:4)//' *'
                 ZK80(JTRAV4+J-1) = CHAZV6(1:2)//XVMIN//' | '
     &                              //XVMAX//' *'
   15          CONTINUE
C
               WRITE(IFR,3001) ' *',CHAV11,CHAV11,CHAV11,CHAV11,
     &                        (CHAZP1,CHAZV1,J=1,NZONE)
               WRITE(IFR,3001) ' *',CHAV12,CHAV12,CHAV12,CHAV13,
     &                        (CHAZP2,ZK80(JTRAV1+J-1),J=1,NZONE)
               WRITE(IFR,3001) ' *',CHAV11,CHAV11,CHAV11,CHAV11,
     &                        (CHAZP1,CHAZV1,J=1,NZONE)
               WRITE(IFR,3001) ' *',CHAV13,CHAV13,CHAV13,CHAV13,
     &                        (ZK80(JTRAV2+J-1),CHAZV3,J=1,NZONE)
               WRITE(IFR,3001) ' *',CHAV21,CHAV21,CHAV22,CHAV23,
     &                        (CHAZP4,CHAZV4,J=1,NZONE)
               WRITE(IFR,3001) ' *',CHAV31,CHAV32,CHAV33,CHAV34,
     &                        (ZK80(JTRAV3+J-1),CHAZV5,J=1,NZONE)
               WRITE(IFR,3001) ' *',CHAV13,CHAV13,CHAV13,CHAV13,
     &                        (CHAZP6,ZK80(JTRAV4+J-1),J=1,NZONE)
               WRITE(IFR,3001) ' *',CHAV13,CHAV13,CHAV13,CHAV13,
     &                        (CHAZP7,CHAZV7,J=1,NZONE)
               WRITE(IFR,3001) ' *',CHAV11,CHAV11,CHAV11,CHAV11,
     &                        (CHAZP1,CHAZV1,J=1,NZONE)
C
               CALL JEDETR('&&FLUIMP.TRAV1')
               CALL JEDETR('&&FLUIMP.TRAV2')
               CALL JEDETR('&&FLUIMP.TRAV3') 
               CALL JEDETR('&&FLUIMP.TRAV4') 
             ENDIF
          ELSE
            WRITE(IFR,2001) ' *',CHAV11,CHAV11,CHAV11,CHAV11
            WRITE(IFR,2001) ' *',CHAV12,CHAV12,CHAV12,CHAV13
            WRITE(IFR,2001) ' *',CHAV11,CHAV11,CHAV11,CHAV11
            WRITE(IFR,2001) ' *',CHAV13,CHAV13,CHAV13,CHAV13
            WRITE(IFR,2001) ' *',CHAV21,CHAV21,CHAV22,CHAV23
            WRITE(IFR,2001) ' *',CHAV31,CHAV32,CHAV33,CHAV34
            WRITE(IFR,2001) ' *',CHAV13,CHAV13,CHAV13,CHAV13
            WRITE(IFR,2001) ' *',CHAV13,CHAV13,CHAV13,CHAV13
            WRITE(IFR,2001) ' *',CHAV11,CHAV11,CHAV11,CHAV11
          ENDIF
C
          IF (CALCUL(1)) THEN
C ---     ECRITURE DES LIGNES POUR CHAQUE VITESSE GAP
          DO 20 IV = 1,NPV
            IND = 2*NBM*(IV-1) + 2*(IM-1) + 1
            FREQ1 = FREQ(IND)
            AMOR1 = FREQ(IND+1)
            DIF1  = 1.D0-DBLE(ABS(AMOR1))
C
            IF (VITE(IV) .GE. 0) THEN
              CALL CODREE(VITE(IV),'E',XCOD)
              XCOD=' '//XCOD
            ELSE
              CALL CODREE(ABS(VITE(IV)),'E',XCOD)
              XCOD='-'//XCOD
            ENDIF
C
            IF (FREQ1.LT.0.D0) THEN
              CHAV40 = ' * '//XCOD//' *            '//
     &                  'PROBLEME DE CONVERGENCE            *'
              IF (ITYPFL .EQ. 1) THEN
                CHAZ40 = '                            |'//
     &                    '               |               *'
                WRITE(IFR,3002) CHAV40,(CHAZ40,J=1,NZONE)
              ELSE
                WRITE(IFR,2002) CHAV40
              ENDIF
C
            ELSE IF (DIF1.LT.1.D-8) THEN
              CHAV40 = ' * '//XCOD//' *              '//
     &                  'SYSTEME SUR-AMORTI               *'
              IF (ITYPFL .EQ. 1) THEN
                CHAZ40 = '                            |'//
     &                    '               |               *'
                WRITE(IFR,3002) CHAV40,(CHAZ40,J=1,NZONE)
              ELSE
                WRITE(IFR,2002) CHAV40
              ENDIF
C
            ELSE
              VRED = VITE(IV)/(FREQ1*CARAC(1))
              IF(VRED .GE. 0) THEN  
                CALL CODREE(VRED,'E',XVRED)
                XVRED = ' '//XVRED
              ELSE
                CALL CODREE(ABS(VRED),'E',XVRED)
                XVRED = '-'//XVRED
              ENDIF
C
              IF (FREQ1 .GE. 0) THEN  
                CALL CODREE(FREQ1,'E',XFREQ1)
                XFREQ1 = ' '//XFREQ1
              ELSE
                CALL CODREE(ABS(FREQ1),'E',XFREQ1)
                XFREQ1 = '-'//XFREQ1
              ENDIF
C
              IF (AMOR1 .GE. 0) THEN    
                CALL CODREE(AMOR1*1.D+02,'E',XAMOR)
                XAMOR = ' '//XAMOR
              ELSE
                CALL CODREE(ABS(AMOR1*1.D+02),'E',XAMOR)
                XAMOR = '-'//XAMOR
              ENDIF 

              CHAV40 =' * '//XCOD//' * '//XVRED//' * '//
     &                   XFREQ1//' * '//XAMOR//' *' 
C
              IF (ITYPFL .EQ. 1) THEN
                CALL WKVECT('&&FLUIMP.TRAV5','V V K80',10,JTRAV5)
                CALL JEVEUO('&&PACOUC.TRAV1','L',JTR1)
                CALL JEVEUO('&&PACOUC.TRAV2','L',JTR2)  
C              
                DO 25 IK = 1,NZONE
                  L1   = ZI(JTR2 + 3*NZONE*NPV*(IM-1)+
     &                   3*NZONE*(IV-1) + 3*(IK-1))
                  L2   = ZI(JTR2 + 3*NZONE*NPV*(IM-1)+
     &                   3*NZONE*(IV-1) + 3*(IK-1) + 1)
                  L3   = ZI(JTR2 + 3*NZONE*NPV*(IM-1)+
     &                   3*NZONE*(IV-1) + 3*(IK-1) + 2)
                  BMIN = ZR(JTR1 + 2*NZONE*NPV*(IM-1) + 
     &                   2*NZONE*(IV-1) + 2*(IK-1))
                  BMAX = ZR(JTR1 +2*NZONE*NPV*(IM-1) + 
     &                   2*NZONE*(IV-1) + 2*(IK-1) + 1)
                  CALL CODENT(L1,'D',XL1)     
                  CALL CODENT(L2,'D',XL2)     
                  CALL CODENT(L3,'D',XL3)     
                  IF (L1.EQ.0) THEN
                    XBMIN = '     -       '
                  ELSE
                    IF (BMIN.LT.0.D0) THEN
                      CALL CODREE(BMIN,'E',XBMIN)
                      XBMIN = '-'//XBMIN
                    ELSE
                      CALL CODREE(BMIN,'E',XBMIN)
                      XBMIN = ' '//XBMIN
                    ENDIF
                  ENDIF
                  IF (L3.EQ.0) THEN
                    XBMAX = '     -       '
                  ELSE
                    IF (BMAX.LT.0.D0) THEN
                      CALL CODREE(BMAX,'E',XBMAX)
                      XBMAX = '-'//XBMAX
                    ELSE
                      CALL CODREE(BMAX,'E',XBMAX)
                      XBMAX = ' '//XBMAX
                    ENDIF
                   ENDIF
                  ZK80(JTRAV5+IK-1) =' '//XL1(1:8)//' '//XL2(1:8)//
     &               ' '//XL3(1:8)//' | '//XBMIN//' | '//XBMAX//' *'
   25            CONTINUE 
                 WRITE (IFR,3002) CHAV40,(ZK80(JTRAV5+J-1),J=1,NZONE) 
                 CALL JEDETR('&&FLUIMP.TRAV5')         
               ELSE
                 WRITE (IFR,2002) CHAV40
               ENDIF                 
            ENDIF
   20     CONTINUE
C
          IF (ITYPFL .EQ. 1) THEN
            WRITE(IFR,3001) '*',CHAV13,CHAV13,CHAV13,CHAV13,
     &                     (CHAZP2,CHAZV7,J=1,NZONE)
            WRITE(IFR,3001) '*',CHAV11,CHAV11,CHAV11,CHAV11,
     &                     (CHAZP1,CHAZV1,J=1,NZONE)
          ELSE      
            WRITE(IFR,2001) '*',CHAV13,CHAV13,CHAV13,CHAV13
            WRITE(IFR,2001) '*',CHAV11,CHAV11,CHAV11,CHAV11
          ENDIF      
          ENDIF
          IF (CALCUL(2)) THEN

        WRITE (IFR,*) '==============================================='
        WRITE (IFR,*)
        WRITE (IFR,*) 'VALEURS DES VITESSES CRITIQUES ET DES RAPPORTS'
        WRITE (IFR,*) '   D INSTABILITE PAR LA METHODE DE CONNORS'
        WRITE (IFR,*)
        WRITE (IFR,*) '==============================================='
        WRITE (IFR,*)
        CALL CODREE(ZR(JCONN),'E',CTRAV)
        WRITE(IFR,'(A)')'MASSE LINEIQUE DE REFERENCE DU TUBE (kg/m) : '
     &  //CTRAV
        CALL CODREE(ZR(JCONN+1),'E',CTRAV)
        WRITE(IFR,'(A)')'MASSE VOLUMIQUE DE REFERENCE
     & DU FLUIDE SECONDAIRE (kg/m3) : '//CTRAV
             IF (CALCUL(1)) THEN

        WRITE (IFR,5002)('*',J=1,91)
        WRITE (IFR,'(A)')' *   MODE    *      FREQUENCE(Hz)      *
     &    AMORTISSEMENT (%)    *  VITESSE EFFICACE (m/s) *'

        WRITE (IFR,5002)('*',J=1,91)
        WRITE (IFR,5001) IMOD,FREQI(IMOD),(AMOC(IM)*100),
     &                   ZR(JVEN-1+IM)
        WRITE (IFR,5002)('*',J=1,91)
        WRITE (IFR,*)

             ENDIF
             WRITE(IFR,*)
             WRITE(IFR,*) '============================================'
             WRITE(IFR,*)'PLAGE DE VARIATION DES CONSTANTES DE CONNORS'
             WRITE(IFR,*) '============================================'
             WRITE(IFR,*)
             WRITE(IFR,5005) (CHAM30,J=1,NZONE)
             WRITE(IFR,5003) ('ZONE',I,I=1,NZONE)
             WRITE(IFR,5005) (CHAM30,J=1,NZONE)
             WRITE(IFR,5004) (ZR(LFSVR+3+2*(J-1)),
     &                        ZR(LFSVR+3+2*(J-1)+1),J=1,NZONE)
             WRITE(IFR,5005) (CHAM30,J=1,NZONE)
             WRITE(IFR,*)
             CTRAV1='*'
             CTRAV3='*'
             DO 90 I=1,NZONE
                CALL CODENT(I,'D',I3)
                CTRAV1((2+30*(I-1)):(2+(30*I)))='           ZONE '//I3
     &           //'          *'
90           CONTINUE
             CTRAV1((2+(30*NZONE)+90):(2+(30*NZONE)+90))='*'
             CTRAV3(((15*NZONE)-10):((15*NZONE)+11))=
     &             'CONSTANTES DE CONNORS'
             CTRAV3((1+(30*NZONE)):(1+(30*NZONE)))='*'
             CTRAV3((2+(30*NZONE)):(2+(30*NZONE)+90))=
     & '    VITESSE CRITIQUE (m/s)   *    VITESSE REDUITE CRITIQUE    *
     &  RAPPORT D INSTABILITE    *'
             WRITE(IFR,'(A)') CTRAV2(1:(2+(30*NZONE)+90))
             WRITE(IFR,'(A)') CTRAV1(1:(2+(30*NZONE)+90))
             WRITE(IFR,'(A)') CTRAV2(1:(2+(30*NZONE)+90))
             WRITE(IFR,'(A)') CTRAV3(1:(2+(30*NZONE)+90))
             WRITE(IFR,'(A)') CTRAV2(1:(2+(30*NZONE)+90))

             DO 100 I=1,NBVAL
                CTRAV1='* '
                DO 110 J=1,NZONE
                   MODUL=1
                   DO 120 K=(J+1),NZONE
                      MODUL=MODUL*ZI(LFSVI+1+NZONE+K)
120                CONTINUE
                   IF (J.EQ.1) THEN
                      PAS=(I-1)/MODUL
                   ELSE
                      MODUL2=MODUL*ZI(LFSVI+1+NZONE+J)
                      PAS=(MOD((I-1),MODUL2))/MODUL
                   ENDIF    
                   ZR(JCSTE-1+J)=ZR(LFSVR+3+2*(J-1))+PAS*
     &             (ZR(LFSVR+3+2*(J-1)+1)-ZR(LFSVR+3+2*(J-1)))
     &             /(ZI(LFSVI+1+NZONE+J)-1)
                   CALL CODREE(ZR(JCSTE-1+J),'E',CCSTE)
                   CTRAV1((3+(30*(J-1))):(3+(30*J)))='  '//CCSTE//'  *'

110            CONTINUE
             REDUIT=ZR(JVCN-1+(IM-1)*NBVAL+I)/(FREQI(IMOD)*CARAC(1))
             RAPPOR=ZR(JVEN-1+IM)/ZR(JVCN-1+(IM-1)*NBVAL+I)
             CALL CODREE (RAPPOR,'E',CRAPPO)
             CALL CODREE (REDUIT,'E',CREDUI)
             CALL CODREE (ZR(JVCN-1+(IM-1)*NBVAL+I),'E',CVCN)
             CTRAV1((3+(30*NZONE)):(3+(30*NZONE)+90))=
     &       '  '//CVCN//'  *    '//CREDUI//'    *  '//CRAPPO//' *'

             WRITE(IFR,'(A)') CTRAV1(1:(3+(30*NZONE)+90))


 100         CONTINUE
             WRITE(IFR,'(A)') CTRAV2(1:(2+(30*NZONE)+90))
             WRITE(IFR,*) 
     
          ENDIF
C
   10   CONTINUE
C
      ENDIF
C
      IF (NIVDEF.EQ.1) THEN
C
        WRITE (IFR,*)
        WRITE (IFR,*) '==============================================='
        WRITE (IFR,*)
        WRITE (IFR,*) ' RESULTAT MODULE COUPLAGE FLUIDE-STRUCTURE'
        WRITE (IFR,*)
        WRITE (IFR,*) '        EVOLUTION DES DEFORMEES MODALES'
        WRITE (IFR,*) '   EN FONCTION DE LA VITESSE DE L ECOULEMENT'
        WRITE (IFR,*)
        WRITE (IFR,*) '==============================================='
        WRITE (IFR,*)
C
        IF (ITYPFL.NE.3) THEN
          NPASV = 1
          WRITE(IFR,*) 'LES DEFORMEES SOUS ECOULEMENT SONT INCHANGEES'//
     &                 ' PAR RAPPORT A CELLES EN FLUIDE AU REPOS.'
          WRITE(IFR,*)
        ENDIF
C
        DO 30 IM = 1,NBM
          WRITE(IFR,4001) NUOR(IM)
          WRITE(IFR,*)
          WRITE(CHAM19(14:16),'(I3.3)') NUOR(IM)
          DO 40 IV = 1,NPASV
            WRITE(IFR,4002) IV
            WRITE(IFR,*)
            WRITE(CHAM19(17:19),'(I3.3)') IV
            CALL IRDEPL(CHAM19,' ',IFR,'RESULTAT',K8BID,K8BID,NOMSYM,
     &             IBID,IBID,IBID,IBID,IBID,IBID,LBID,LCOR,NBNO,IBID,6,
     &             NOMCMP,LSUP,RBID,LINF,RBID,LMAX,LMIN,LRESU,FORMAR,
     &             NIVE)
            WRITE(IFR,*)
  40      CONTINUE
  30    CONTINUE
C
      ENDIF          
      CALL JEDETR('&&FLUIMP.CSTE')
      CALL JEDEMA()      
C --- FORMATS
C 
 1001 FORMAT (1P,' VITESSE MOYENNE SUR L ENSEMBLE DES ZONES = ',D13.6)
 1002 FORMAT (1P,' MODE : NUMERO D ORDRE:',I3,'/ NUME_MODE:',I3,
     &        '/ FREQ:',D13.6)
 2001 FORMAT (A2,4A16)
 2002 FORMAT (A66)

 3001 FORMAT (A2,4A16,30(A28,A33))
 3002 FORMAT (A66,30A61)

 4001 FORMAT (1X,' MODE N ',I3)
 4002 FORMAT (1X,' VITESSE N ',I3)
 
 5001 FORMAT (1P,1X,'*',3X,I3,5X,'*',4(5X,D13.6,7X,'*'))
 5002 FORMAT (1X,91A1)
 5003 FORMAT (1X,'*',100(10X,A4,1X,I3,11X,'*'))
 5004 FORMAT (1P,1X,100('*',D13.6,1X,'-',D13.6,1X))
 5005 FORMAT (1X,'*',100A30)
C

      END
