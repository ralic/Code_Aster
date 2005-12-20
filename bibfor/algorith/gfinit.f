      SUBROUTINE GFINIT ( NUMEDD, NUME, DEPMOI, VITPLU, ACCPLU, DT,
     +                    CHGRFL )
      IMPLICIT   NONE
      INTEGER             NUME
      REAL*8              DT
      CHARACTER*24        NUMEDD, DEPMOI, VITPLU, ACCPLU, CHGRFL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/11/2004   AUTEUR CIBHHLV L.VIVAN 
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
C     FORCE_FLUIDE: INITIALISATION DES COMMONS PEGASE
C
C ----------------------------------------------------------------------
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
      CHARACTER*32       JEXNUM
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
      INTEGER      I2, I3, I6, I7, I8, I9, I10, I14, I15, I16, I18,
     +             I19, II, JFFL, JIFL, JPARA,
     &             NBNO, NEC, NLILI, NDIM, INO, IVAL, IDIM, I, K, IER,
     &             IAPRNO, JDEPL, JVITE, JACC0
      REAL*8       C7, C8, C9, PI, R8PI, UM, UA, UML, UI, UMM1, UAM1,
     +             UMLM1, UIM1, ROC, ROD, ROP, ROML, NUML, G, MA, M, 
     +             DTM, ROTIGE, LTIGE, VARAI, CDML, CDI, ROARAI, 
     +             ROCRAY, LCRAY, LI, LML, ROI, NUI, A, AC, AM, AI,
     +             AML, AT, DHML, DHI, Z, DZ, D2Z, VDIR(3), R8VIDE
      CHARACTER*8  K8B, RESULT
      CHARACTER*16 K16B, CMD
      CHARACTER*24 NOLILI
C ----------------------------------------------------------------------
      CALL JEMARQ()

      CALL GETRES ( RESULT, K16B, CMD )
      PI = R8PI()
C
      CALL JEVEUO ( CHGRFL, 'E', JFFL ) 
      CALL JEVEUO ( '&&GFLECT.INDICE', 'E', JIFL ) 
      II = 5 + ZI(JIFL-1+5)
      I2  = ZI(JIFL-1+II+2 )
      I3  = ZI(JIFL-1+II+3 )
      I6  = ZI(JIFL-1+II+6 )
      I7  = ZI(JIFL-1+II+7 )
      I8  = ZI(JIFL-1+II+8 )
      I9  = ZI(JIFL-1+II+9 )
      I10 = ZI(JIFL-1+II+10)
      I14 = ZI(JIFL-1+II+14)
      I15 = ZI(JIFL-1+II+15)
      I16 = ZI(JIFL-1+II+16)
      I18 = ZI(JIFL-1+II+18)
      I19 = ZI(JIFL-1+II+19)
C
      CALL DISMOI('F','NB_EC','DEPL_R','GRANDEUR',NEC,K8B,IER)
      NDIM = 3
C
C --- RECUPERATION DU NOMBRE DE NOEUDS MODELISANT LA GRAPPE :
C     -----------------------------------------------------
      NBNO = ZI(JIFL-1+5)
C
C --- RECUPERATION DU VECTEUR UNITAIRE ORIENTANT LE CRAYON :
C     ----------------------------------------------------
      VDIR(1) = ZR(JFFL-1+I18+3*(NBNO-1)+1)
      VDIR(2) = ZR(JFFL-1+I18+3*(NBNO-1)+2)
      VDIR(3) = ZR(JFFL-1+I18+3*(NBNO-1)+3)
C
C --- RECUPERATION DE L'ADRESSAGE DES CHAMPS : 
C ---   .PRNO ASSOCIE AU MAILLAGE :
C       -------------------------
      CALL JELIRA(NUMEDD(1:14)//'.NUME.PRNO','NMAXOC',NLILI,K8B)
C
      K = 0
      DO 10 I = 1, NLILI
        CALL JENUNO(JEXNUM(NUMEDD(1:14)//'.NUME.LILI',I),NOLILI)
        IF (NOLILI(1:8).NE.'&MAILLA ') GOTO 10
        K = I
  10  CONTINUE
      IF (K.EQ.0) THEN
        CALL UTMESS('F','GFINIT','ERREUR DANS LA RECUPERATION DU '//
     +                           'NUME.PRNO .')
      ENDIF
      CALL JEVEUO(JEXNUM(NUMEDD(1:14)//'.NUME.PRNO',K),'L',IAPRNO)

      CALL JEVEUO ( DEPMOI(1:19)//'.VALE', 'L', JDEPL)
      CALL JEVEUO ( VITPLU(1:19)//'.VALE', 'L', JVITE)
      CALL JEVEUO ( ACCPLU(1:19)//'.VALE', 'L', JACC0)
C
      Z   = 0.0D0
      DZ  = 0.0D0
      D2Z = 0.0D0
      DO 34  I = 1, NBNO
         INO  = ZI(JIFL-1+5+I)
         IVAL = ZI(IAPRNO+(INO-1)*(NEC+2)+1-1) - 1
         DO 36 IDIM = 1, NDIM
            Z   = Z   + ZR(JDEPL+IVAL+IDIM-1)*VDIR(IDIM)
            DZ  = DZ  + ZR(JVITE+IVAL+IDIM-1)*VDIR(IDIM)
            D2Z = D2Z + ZR(JACC0+IVAL+IDIM-1)*VDIR(IDIM)
 36      CONTINUE
 34   CONTINUE
      Z   = Z   / NBNO
      DZ  = DZ  / NBNO
      D2Z = D2Z / NBNO
C
      ZI(JIFL-1+3) = NUME
C
      ZR(JFFL-1+I14+1) = Z
      ZR(JFFL-1+I14+2) = DZ
      ZR(JFFL-1+I14+3) = D2Z
      ZR(JFFL-1+I14+4) = D2Z
      ZR(JFFL-1+I14+5) = DT

C --- CALCUL DE LA MASSE APPARENTE :
C     ----------------------------
      ROC = ZR(JFFL-1+I3+1)
      ROD = ZR(JFFL-1+I3+2)
      ROP = ZR(JFFL-1+I3+3)
      AC   = ZR(JFFL-1+I7+8)
      ROTIGE = ZR(JFFL-1+I9+2)
      LTIGE  = ZR(JFFL-1+I9+3)
      VARAI  = ZR(JFFL-1+I9+4)
      ROARAI = ZR(JFFL-1+I9+5)
      ROCRAY = ZR(JFFL-1+I9+6)
      LCRAY  = ZR(JFFL-1+I9+7)
      DTM    = ZR(JFFL-1+I9+8)

      MA = PI/4.0D0*DTM**2*((ROTIGE-ROD)*(LTIGE-Z) + (ROTIGE-ROP)*Z)
      MA = MA + 24*AC*((ROCRAY-ROC)*Z + (ROCRAY-ROP)*(LCRAY-Z))
      MA = MA + VARAI*(ROARAI-ROP)
C
      ZR(JFFL-1+I8+2) = MA
C
      A  = ZR(JFFL-1+I7+4)
      M  = ZR(JFFL-1+I8+1)
      G  = ZR(JFFL-1+I8+3)
C
CCC       X : VITESSE ET PRESSION DU FLUIDE (TUBGUI)
C
      ZR(JFFL-1+I16+1) = AC/A*(DZ+MA/M*G*DT)

      LI   = ZR(JFFL-1+1)
      LML  = ZR(JFFL-1+2)
      ROML = ZR(JFFL-1+7)
      NUML = ZR(JFFL-1+8)
      AML  = ZR(JFFL-1+9)
      AI   = ZR(JFFL-1+10)
      DHML = ZR(JFFL-1+11)
      ROI  = ZR(JFFL-1+13)
      DHI  = ZR(JFFL-1+12)
      NUI  = ZR(JFFL-1+14)
      CDML = ZR(JFFL-1+I2+3)
      CDI  = ZR(JFFL-1+I2+4)
      AT   = ZR(JFFL-1+I6+7)
      AM   = ZR(JFFL-1+I10+10)
C
CCC      COMMON/MECANISME1/um,ua,uml,ui,ug,umm1,uam1,umlm1,uim1,pm,pa,
C                          pml,ps,Cfm,Cfg,Cfi,Cfa
      IF ( NUME .EQ. 0 ) THEN
         UM = AT/AM*DZ
         UA = 0.D0
         C7 = ROML*CDML-ROI*CDI*(AML/AI)**2
         C8 = 96*ROML*NUML*LML/DHML**2 + 96*ROI*NUI*LI/DHI**2*AML/AI
     &                                 + 2*ROI*CDI*AT*AML/AI**2*DZ
         C9 = -ROI*CDI*(AT/AI)**2*DZ**2-96*ROML*NUML*LI/DHI**2*AT/AI*DZ
         IF (C7.NE.0.D0) THEN
            UML = (-C8+SQRT(C8**2-4*C7*C9))/C7/2
         ELSE
            UML = AT*DZ/(AI*ROML*LML/DHML**2/(ROI*LI/DHI**2)+AML)
         ENDIF
         UI = AT/AI*DZ - AML/AI*UML
      ELSE
         CALL RSADPA(RESULT,'L',1,'GFUM',NUME,0,JPARA,K8B)
         UM = ZR(JPARA)
         IF ( UM .EQ. R8VIDE() ) THEN
            UM = AT/AM*DZ
            UA = 0.D0
            C7 = ROML*CDML-ROI*CDI*(AML/AI)**2
            C8 = 96*ROML*NUML*LML/DHML**2 + 96*ROI*NUI*LI/DHI**2*AML/AI
     &                                 + 2*ROI*CDI*AT*AML/AI**2*DZ
         C9 = -ROI*CDI*(AT/AI)**2*DZ**2-96*ROML*NUML*LI/DHI**2*AT/AI*DZ
            IF (C7.NE.0.D0) THEN
               UML = (-C8+SQRT(C8**2-4*C7*C9))/C7/2
            ELSE
               UML = AT*DZ/(AI*ROML*LML/DHML**2/(ROI*LI/DHI**2)+AML)
            ENDIF
            UI = AT/AI*DZ - AML/AI*UML
C
            ZI(JIFL-1+3) = 0
C
         ELSE
            CALL RSADPA(RESULT,'L',1,'GFUA',NUME,0,JPARA,K8B)
            UA = ZR(JPARA)
            CALL RSADPA(RESULT,'L',1,'GFUML',NUME,0,JPARA,K8B)
            UML = ZR(JPARA)
            CALL RSADPA(RESULT,'L',1,'GFUI',NUME,0,JPARA,K8B)
            UI = ZR(JPARA)
C
            CALL RSADPA(RESULT,'L',1,'GFVAG',NUME,0,JPARA,K8B)
            ZR(JFFL-1+I16+1) = ZR(JPARA)
C
            CALL RSADPA(RESULT,'L',1,'ITER_DASHPOT',NUME,0,JPARA,K8B)
            ZI(JIFL-1+4) = ZI(JPARA)
            CALL RSADPA(RESULT,'L',1,'GFVFD',NUME,0,JPARA,K8B)
            ZR(JFFL-1+I19+1) = ZR(JPARA)
            CALL RSADPA(RESULT,'L',1,'GFVAD',NUME,0,JPARA,K8B)
            ZR(JFFL-1+I19+2) = ZR(JPARA)
         ENDIF
C
      ENDIF
      UMM1  = UM
      UAM1  = UA
      UMLM1 = UML
      UIM1  = UI
      ZR(JFFL-1+I15+1) = UM
      ZR(JFFL-1+I15+2) = UA
      ZR(JFFL-1+I15+3) = UML
      ZR(JFFL-1+I15+4) = UI
      ZR(JFFL-1+I15+6) = UMM1
      ZR(JFFL-1+I15+7) = UAM1
      ZR(JFFL-1+I15+8) = UMLM1
      ZR(JFFL-1+I15+9) = UIM1
C
      CALL JEDEMA()
      END
