      SUBROUTINE  CALCSP ( CASINT, NOMU, TABLE, FREQ, MASG, NBM, NPV,
     &                     NBMR, IMOD1, NUOR, VITE )
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 28/09/98   AUTEUR KXBADNG F.BEAUD 
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
C     CALCUL POUR CHAQUE VITESSES DES INTERSPECTRES DE REPONSES
C-----------------------------------------------------------------------
C IN  : CASINT: BOOLEEN CARACTERISANT L'OPTION DE CALCUL
C       CASINT= TRUE   => ON CALCULE TOUS LES INTERSPECTRES
C       CASINT= FALSE  => ON CALCULE LES AUTOSPECTRES UNIQUEMENT
C IN  : NOMU  : NOM UTILISATEUR DU CONCEPT TABL_INTSP CORRESPONDANT
C               AUX INTERSPECTRES DE REPONSES : A PRODUIRE
C IN  : TABLE : NOM UTILISATEUR DU CONCEPT TABL_INTSP CORRESPONDANT
C               AUX INTERSPECTRES D'EXCITATIONS : DONNEE DU CALCUL
C IN  : FREQ  : TABLEAU DES FREQUENCES ET AMORTISSEMENTS
C IN  : MASG  : TABLEAU DES MASSES GENERALISEES
C IN  : NBM   : NOMBRE DE MODES DE LA BASE DE CONCEPT MELASFLU
C IN  : NPV   : NOMBRE DE VITESSES ETUDIEES
C IN  : NBMR  : NOMBRE DE MODES PRIS EN COMPTE
C IN  : IMOD1 : INDICE DU PREMIER MODE PRIS EN COMPTE DANS LA BASE DE
C               CONCEPT MELASFLU
C IN  : NUOR  : LISTE DES NUMEROS D'ORDRE DES MODES PRIS EN COMPTE
C IN  : VITE  : TABLEAU DES VITESSES DE FLUIDE
C     ----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
      LOGICAL       CASINT
      CHARACTER*8   NOMU, TABLE
      INTEGER       NBM, NPV, NBMR, IMOD1, NUOR(*)
      REAL*8        FREQ(*), MASG(*), VITE(*)
C
      PARAMETER   ( NBPAR = 5 )
      INTEGER       IVAL(3)
      REAL*8        MGI, KSI
      CHARACTER*3   K3IM, K3IV
      CHARACTER*8   K8B
      CHARACTER*19  NOMCOD
      CHARACTER*16  NOPAR(NBPAR)
      CHARACTER*24  NOMFON, VALE, PROL, NOMFO
      COMPLEX*16    C16B
C
      DATA NOPAR  / 'VITE_FLUIDE'  , 'NUME_VITE_FLUI'   , 
     +              'NUME_ORDRE_I' , 'NUME_ORDRE_J' , 'FONCTION' /
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
      PI = R8PI()
      IMODF = IMOD1 + NBMR - 1
C
      IVAL(1) = 1
      IVAL(2) = NUOR(1)
      IVAL(3) = NUOR(1)
C
      CALL TBLIVA ( TABLE, 3, NOPAR(2), IVAL, R8B, C16B, K8B, K8B,
     +             R8B, 'FONCTION', K8B, IBID, R8B, C16B, NOMCOD, IRET )
      IF ( IRET .NE. 0 ) CALL UTMESS('F','CALCSP','Y A UN BUG 1' )
C
      NOMFO = NOMCOD//'.VALE'
      CALL JELIRA ( NOMFO, 'LONUTI', NBPF, K8B )
      NBPF = NBPF/3
C
C --- CREATION DE VECTEURS DE TRAVAIL ---
C
      CALL WKVECT('&&CALCSP.TEMP.HR  ','V V R8',NBMR*NBPF,IHR  )
      CALL WKVECT('&&CALCSP.TEMP.HI  ','V V R8',NBMR*NBPF,IHI  )
C
      DO 20 IV = 1,NPV
C
        IVAL(1) = IV
        IVAL(2) = NUOR(1)
        IVAL(3) = NUOR(1)
C
        CALL TBLIVA ( TABLE, 3, NOPAR(2), IVAL, R8B, C16B, K8B, K8B,
     +             R8B, 'FONCTION', K8B, IBID, R8B, C16B, NOMCOD, IRET )
        IF ( IRET .NE. 0 ) CALL UTMESS('F','CALCSP','Y A UN BUG 2' )
C
        NOMFO = NOMCOD//'.VALE'
        CALL JEVEUO ( NOMFO, 'L', IFO )
C
        DO 25 IM = IMOD1,IMODF
          FRI = FREQ(2*NBM*(IV-1)+2*(IM-1)+1)
          IF ( FRI.LT.0.D0 ) THEN
             WRITE(K3IV,'(I3)') IV
             WRITE(K3IM,'(I3)') NUOR(IM)
             CALL UTMESS('A','CALCSP','LE CALCUL DES PARAMETRES DU '//
     &                   'MODE NO'//K3IM//' PAR L''OPERATEUR '//
     &                   '<CALC_FLUI_STRU> N''A PAS CONVERGE POUR '//
     &                   'LA VITESSE NO'//K3IV//'. ON NE CALCULE '//
     &                   'DONC PAS D''INTERSPECTRES DE REPONSE '//
     &                   'MODALE POUR CETTE VITESSE.')
             GO TO 20
          ENDIF
 25     CONTINUE
C
        DO 30 IM = IMOD1,IMODF
C
          MGI = MASG(IM)*4.D0*PI*PI
          FRI = FREQ(2*NBM*(IV-1)+2*(IM-1)+1)
          KSI = FREQ(2*NBM*(IV-1)+2*(IM-1)+2)
          IF ( KSI .LE. 0.D0 ) KSI = 1.D-06
C
          IMB = IM - IMOD1 + 1
C
          DO 40 IP = 1,NBPF
            FR = ZR(IFO+IP-1)
            IHR1 = IHR+NBPF*(IMB-1)+IP-1
            IHI1 = IHI+NBPF*(IMB-1)+IP-1
            ZR(IHR1) = (MGI*(FRI*FRI - FR*FR))
            ZR(IHI1) = (MGI*KSI*FR*FRI*2.D0)

 40       CONTINUE
 30     CONTINUE
C
        DO 50 IM2 = 1,NBMR
C
          IVAL(3) = NUOR(IM2)
C
          IDEB = IM2
          IF ( CASINT ) IDEB = 1
C
          DO 60 IM1 = IDEB,IM2
C
            IVAL(2) = NUOR(IM1)
C
            CALL TBLIVA ( TABLE, 3, NOPAR(2), IVAL, R8B, C16B, K8B, K8B,
     +             R8B, 'FONCTION', K8B, IBID, R8B, C16B, NOMFON, IRET )
            IF ( IRET .NE. 0 ) CALL UTMESS('F','CALCSP','Y A UN BUG' )
C
            WRITE(NOMCOD,'(A8,A2,3I3.3)') NOMU,'.S',IV,NUOR(IM1),
     +                                    NUOR(IM2)
C
            CALL TBAJLI ( NOMU, NBPAR, NOPAR, 
     +                          IVAL, VITE(IV), C16B, NOMCOD, 0 )
C
            VALE = NOMCOD(1:19)//'.VALE'
            PROL = NOMCOD(1:19)//'.PROL'
            CALL WKVECT(VALE,'G V R ',3*NBPF,LVALE)
            CALL WKVECT(PROL,'G V K8',5     ,LPROL)
C
            ZK8(LPROL)   = 'FONCT_C '
            ZK8(LPROL+1) = 'LIN LIN '
            ZK8(LPROL+2) = 'FREQ    '
            ZK8(LPROL+3) = 'DSP     '
            ZK8(LPROL+4) = 'LL      '
C
            CALL JEVEUO ( NOMFON(1:19)//'.VALE', 'L', IFONC )
C
            DO 70 IL = 1,NBPF
              ZR(LVALE+IL-1) = ZR(IFONC+IL-1)
 70         CONTINUE
C
            DO 80 IL = 1,NBPF
              HIR1 = ZR(IHR+NBPF*(IM1-1)+IL-1)
              HII1 = ZR(IHI+NBPF*(IM1-1)+IL-1)
              HIR2 = ZR(IHR+NBPF*(IM2-1)+IL-1)
              HII2 = ZR(IHI+NBPF*(IM2-1)+IL-1)
              HHR = 1.D0/(HIR1*HIR2 + HII1*HII2)
              HHI = (HIR2*HII1 - HIR1*HII2)
              IF ( ABS(HHI) .LE. 1.D-20 .OR. IM1.EQ.IM2) THEN
                HHI = 0.D0
              ELSE
                HHI = 1.D0/HHI
              ENDIF
              ZR(LVALE+NBPF+2*(IL-1))   = HHR*ZR(IFONC+NBPF+2*(IL-1))
              ZR(LVALE+NBPF+2*(IL-1)+1) = HHI*
     +                                    ZR(IFONC+NBPF+2*(IL-1)+1)
 80         CONTINUE
C
 60       CONTINUE
 50     CONTINUE
 20   CONTINUE
C
      CALL JEDETC('V','&&CALCSP',1)
C
      CALL JEDEMA()
      END
