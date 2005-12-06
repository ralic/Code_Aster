      SUBROUTINE DICRGR(OPTION,NEQ,NC,ICODMA,ULM,
     &                  DUL,SIM,VARIM,PGL,KLGLO,VARIP,FONO,SIP,ITEMP,
     &                  TEMPM,TEMPP,IRRAP)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NEQ,ICODMA,NC,JTAB(7),ITEMP
      REAL*8 ULM(NEQ),DUL(NEQ),SIM(NEQ),SIP(NEQ),VARIM(12),TEMPM,TEMPP
      REAL*8 PGL(3,3),VARIP(12),FONO(NEQ)
      REAL*8 IRRAP,KLGLO(144)
      CHARACTER*16 OPTION
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/09/2005   AUTEUR GODARD V.GODARD 
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
C
C RESPONSABLE GODARD V.GODARD


C  COMPORTEMENT DIS_GRICRA : APPLICATION : LIAISON GRILLE-CRAYON COMBU
C           RELATION DE COMPORTEMENT : ELASTIQUE PARTOUT
C           SAUF SUIVANT Y LOCAL : FROTTEMENT DE COULOMB
C       ELEMENTS MECA_DIS_TR_L ET MECA_DIS_T_L

C IN  : OPTION : RIGI_MECA*, FULL_MECA* OU RAPH_MECA
C       NEQ    : NOMBRE DE DDL DE L'ELEMENT
C       NC     : NOMBRE DE DDL PAR NOEUD = 6
C       ICODMA : ADRESSE DU MATERIAU CODE
C       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
C       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
C       VARIM  : VARIABLE INTERNE A L'INSTANT PRECEDENT
C       PGL    : MATRICE DE PASSAGE REPERE GLOBAL -> LOCAL
C       ITEMP  : INDICATEUR DU CHAMP DE TEMPERATURE
C       TEMPM  : TEMPERATURE A L'INSTANT PRECEDENT
C       TEMPP  : TEMPERATURE A L'INSTANT ACTUEL
C       IRRAP  : IRRADIATION
C
C OUT : KLV    : MATRICE TANGENTE
C       VARIP  : VARIABLE INTERNE REACTUALISEE
C       FONI   : FORCES NODALES
C       SIP    : EFFORTS INTERNES
C------------------------------------------------------------------
C------------------------------------------------------------------

C **************** DEBUT COMMUNS NORMALISES JEVEUX *********************

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


      CHARACTER*2 CODRET(20)
      CHARACTER*8 NOMRES(20),NOMPAR

      INTEGER     LGPG,IRET,NNO,I,NBPAR,IIRRAP,J,K,L,P,Q
      REAL*8      VALRES(20),RBID
      REAL*8      KN,KT,RN,RT,LB,FNO,MU,ECROB,ECROR,GNO
      REAL*8      C0,C1,C2,C3,C4,C5,TB1,TB2,TF,TC
      REAL*8      H1,H2,B1,B2,R1,R2
      REAL*8      DUX,DUY,DUZ,DPH,DTH
      REAL*8      UXM,UYM,UZM,PHM,THM
      REAL*8      X,Y,Z,DX,DY,DZ
      REAL*8      FH1(3),FB1(3),FR1(3)
      REAL*8      MOPX,MOPY,MOPZ
      REAL*8      KXX,KYY,KZZ,KNN,KPP,KTT,KZP,KXT
      REAL*8      FL(12),RMIN
      REAL*8      XPH1,XPB1,XPR1
      REAL*8      ZPH1,ZPB1,ZPR1
      REAL*8      FIRRA,TEMP,FTEMP
      REAL*8      MATAN(6,6),MTH1(3,3),MTB1(3,3),MTR1(3,3)
      REAL*8      MATLOC(12,12),MATAN2(6,6)
      REAL*8      MATGLO(12,12),MUR


      DATA NOMRES/'KN_BOS','KT_BOS','KN_RES','KT_RES',
     &            'DIST_BOS','FORC_SER','COUL_BOS','COUL_RES',
     &            'ECRO_BOS','ECRO_RES',
     &            'F_IRRA_0','F_IRRA_1','F_IRRA_2','F_IRRA_3',
     &            'F_IRRA_4','F_IRRA_5',
     &            'F_TEMP_1','F_TEMP_2','TEMP_1','TEMP_2'/

  
C---procedure pour la liaison grille crayon
C
C on travaille ici dans le repère local
C
C
C  recuperer les deplacements et rotation
C  recuperer les variables internes  (moins)
C  recuperer les parametres de defi_materiau

      CALL R8INIR(NEQ,0.D0,FL,1)
      RMIN=1.D-8
C
C recuperation des donnees materiau pour le discret
      CALL RCVALA(ICODMA,' ','DIS_GRICRA',0,' ',0.D0,20,
     &            NOMRES,VALRES,CODRET,' ')
C
C on construit la fonction d'irradiation
      IF ((CODRET(11).EQ.'OK').AND.(CODRET(12).EQ.'OK').AND.
     &     (CODRET(13).EQ.'OK').AND.(CODRET(14).EQ.'OK').AND.
     &     (CODRET(15).EQ.'OK').AND.(CODRET(16).EQ.'OK')) THEN
           C0=VALRES(11)
           C1=VALRES(12)
           C2=VALRES(13)
           C3=VALRES(14)
           C4=VALRES(15)
           C5=VALRES(16)
           FIRRA=(IRRAP+1.D0)**(C5*LOG(IRRAP+1.D0)**5.D0
     &            +C4*LOG(IRRAP+1.D0)**4.D0+C3*LOG(IRRAP+1.D0)**3.D0
     &            +C2*LOG(IRRAP+1.D0)**2.D0+C1*LOG(IRRAP+1.D0)+C0)
      ELSE
        IF ((CODRET(11).EQ.'OK').OR.(CODRET(12).EQ.'OK').OR.
     &     (CODRET(13).EQ.'OK').OR.(CODRET(14).EQ.'OK').OR.
     &     (CODRET(15).EQ.'OK').OR.(CODRET(16).EQ.'OK')) THEN
          CALL UTMESS('F','DICRGR','LES PARAMETRES IRRA_0,IRRA_1,'//
     &            'IRRA_2,IRRA_3,IRRA_4,IRRA_5'//
     &            'DANS DEFI_MATERIAU DOIVENT TOUS ETRE SPECIFIES'//
     &            'POUR PRENDRE EN COMPTE LA FONCTION D IRRADIATION')
        ELSE
          FIRRA=1.D0
        ENDIF
      ENDIF
C
C on construit la fonction de temperature
      IF (ITEMP.NE.0) THEN
        IF ((CODRET(17).EQ.'OK').AND.(CODRET(18).EQ.'OK').AND.
     &     (CODRET(19).EQ.'OK').AND.(CODRET(20).EQ.'OK')
     &      ) THEN
          TB1=VALRES(17)
          TB2=VALRES(18)
          TF=VALRES(19)
          TC=VALRES(20)
          TEMP = 0.5D0* (TEMPM+TEMPP)
          FTEMP=((TB2-TB1)/(TC-TF))/TB1*(TEMP-TF)+1
        ELSE
          IF ((CODRET(17).EQ.'OK').OR.(CODRET(18).EQ.'OK').OR.
     &       (CODRET(19).EQ.'OK').OR.(CODRET(20).EQ.'OK')
     &        ) THEN
            CALL UTMESS('F','DICRGR','LES PARAMETRES TEMP_0,TEMP_1,'//
     &              'TEMP_2,TEMP_3,TEMP_4'//
     &              'DANS DEFI_MATERIAU DOIVENT TOUS ETRE SPECIFIES'//
     &              'POUR PRENDRE EN COMPTE LA FONCTION DE TEMPERATURE')
          ELSE
            FTEMP=1.D0
          ENDIF
        ENDIF
      ELSE
        FTEMP=1.D0
      ENDIF
C
C
C
      KN=VALRES(1)
      KT=VALRES(2)
      RN=VALRES(3)
      RT=VALRES(4)
      LB=VALRES(5)/2.D0
      FNO=VALRES(6)*FIRRA*FTEMP
      MU=VALRES(7)
      MUR=VALRES(8)
      ECROB=VALRES(9)
      ECROR=VALRES(10)
      GNO=FNO/2.D0
C
C Variables internes de contact au temps moins
      H1=1.D0-VARIM(1)
      B1=1.D0-VARIM(5)
      R1=1.D0-VARIM(9)

      CALL TECACH('OON','PVARIMR',7,JTAB,IRET)
      LGPG = MAX(JTAB(6),1)*JTAB(7)

C--initialisation des variables utiles pour la matrice tangente
      XPH1=0.D0
      XPB1=0.D0
      XPR1=0.D0
      ZPH1=0.D0
      ZPB1=0.D0
      ZPR1=0.D0
C
C
C---calcul de l'evolution des variables internes et des forces
C---pour FULL_MECA et RAPH_MECA

      IF (OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION(1:9).EQ.'RAPH_MECA')
     &        THEN
C
C  extension de l'element
C
C  extension au pas de temps precedent
        UXM = ULM(1+NC) - ULM(1)
        UYM = ULM(2+NC) - ULM(2)
        UZM = ULM(3+NC) - ULM(3)
        PHM = ULM(4+NC) - ULM(4)
        THM = ULM(6+NC) - ULM(6)
C  variation d'extension
        DUX = DUL(1+NC) - DUL(1)
        DUY = DUL(2+NC) - DUL(2)
        DUZ = DUL(3+NC) - DUL(3)
        DPH = DUL(4+NC) - DUL(4)
        DTH = DUL(6+NC) - DUL(6)
C
C
C  Calcul des forces et evolution des variables internes
C   sur chacun des sous éléments du système de liaison
C
C  Bossette H1
        X=UXM+LB*THM
        Y=UYM
        Z=UZM-LB*PHM
        DX=DUX+LB*DTH
        DY=DUY
        DZ=DUZ-LB*DPH
        CALL LCDIGC(X,Y,Z,DX,DY,DZ,VARIM(2),VARIM(3),VARIM(4),KN,KT,MU,
     &             GNO,ECROB,VARIP(2),VARIP(3),VARIP(4),H1,FH1,MTH1)
        VARIP(1)=1.D0-H1
        IF ((ABS(VARIP(2)-VARIM(2)).GT.RMIN).OR.
     &        (ABS(VARIP(3)-VARIM(3)).GT.RMIN)) THEN
         XPH1=KT/(ECROB+KT)
         ZPH1=KT/(ECROB+KT)
        ENDIF
C
C  Bossette B1
        X=UXM-LB*THM
        Y=UYM
        Z=UZM+LB*PHM
        DX=DUX-LB*DTH
        DY=DUY
        DZ=DUZ+LB*DPH
        CALL LCDIGC(X,Y,Z,DX,DY,DZ,VARIM(6),VARIM(7),VARIM(8),KN,KT,MU,
     &             GNO,ECROB,VARIP(6),VARIP(7),VARIP(8),B1,FB1,MTB1)
        VARIP(5)=1.D0-B1
        IF ((ABS(VARIP(6)-VARIM(6)).GT.RMIN).OR.
     &     (ABS(VARIP(7)-VARIM(7)).GT.RMIN)) THEN
         XPB1=KT/(ECROB+KT)
         ZPB1=KT/(ECROB+KT)
        ENDIF
C
C  Ressort R1
C-la direction x du ressort est inversee par rapport aux bossettes
C-on rentre donc -x dans la loi de comportement
        X=-UXM
        Y=UYM
        Z=UZM
        DX=-DUX
        DY=DUY
        DZ=DUZ
        CALL LCDIGC(X,Y,Z,DX,DY,DZ,VARIM(10),VARIM(11),VARIM(12),RN,RT,
     &          MUR,FNO,ECROR,VARIP(10),VARIP(11),VARIP(12),R1,FR1,MTR1)
C
C-la direction x du ressort est inversee par rapport aux bossettes
C-on inverse donc la force et la matrice tangente
        FR1(1)=-FR1(1)
        MTR1(1,2)=-MTR1(1,2)
        MTR1(2,1)=-MTR1(2,1)
        MTR1(3,1)=-MTR1(3,1)
        MTR1(1,3)=-MTR1(1,3)
C
        VARIP(9)=1.D0-R1
        IF ((ABS(VARIP(10)-VARIM(10)).GT.RMIN).OR.
     &     (ABS(VARIP(11)-VARIM(11)).GT.RMIN)) THEN
         XPR1=RT/(ECROR+RT)
         ZPR1=RT/(ECROR+RT)
        ENDIF
C
C
        DO 100 I=1,12
          VARIP(LGPG+I)=VARIP(I)
100   CONTINUE
C
C--- Calcul de forces nodales et des moments dans le repere local
C
        MOPY=0.D0
        MOPX=-LB*(FH1(3)-FB1(3))
        MOPZ=LB*(FH1(1)-FB1(1))
C
        SIP(1)=FH1(1)+FB1(1)+FR1(1)
        SIP(2)=FH1(2)+FB1(2)+FR1(2)
        SIP(3)=FH1(3)+FB1(3)+FR1(3)
        SIP(1+NC)=FH1(1)+FB1(1)+FR1(1)
        SIP(2+NC)=FH1(2)+FB1(2)+FR1(2)
        SIP(3+NC)=FH1(3)+FB1(3)+FR1(3)
        SIP(5)=MOPY
        SIP(4)=MOPX
        SIP(6)=MOPZ
        SIP(5+NC)=MOPY
        SIP(4+NC)=MOPX
        SIP(6+NC)=MOPZ
C
        FL(2)=-SIP(2)
        FL(1)=-SIP(1)
        FL(3)=-SIP(3)
        FL(2+NC)=SIP(2)
        FL(1+NC)=SIP(1)
        FL(3+NC)=SIP(3)
        FL(5)=-MOPY
        FL(4)=-MOPX
        FL(6)=-MOPZ
        FL(5+NC)=MOPY
        FL(4+NC)=MOPX
        FL(6+NC)=MOPZ
C
C
C
C calcul de la partie de la matrice tangente sur un noeud
C
C  dans le repere local
C
        CALL R8INIR(36,0.D0,MATAN,1)
        DO 500 I=1,3
          DO 501 J=1,3
            MATAN(I,J)=MATAN(I,J)+MTH1(I,J)+MTB1(I,J)+MTR1(I,J)
501       CONTINUE
          MATAN(I,4)=MATAN(I,4)-LB*MTH1(I,3)+LB*MTB1(I,3)
          MATAN(I,6)=MATAN(I,6)+LB*MTH1(I,1)-LB*MTB1(I,1)
          MATAN(4,I)=MATAN(4,I)-LB*MTH1(3,I)+LB*MTB1(3,I)
          MATAN(6,I)=MATAN(6,I)+LB*MTH1(1,I)-LB*MTB1(1,I)
500     CONTINUE
          MATAN(4,4)=LB*LB*(MTH1(3,3)+MTB1(3,3))
          MATAN(6,6)=LB*LB*(MTH1(1,1)+MTB1(1,1))
          MATAN(4,6)=-LB*LB*(MTH1(3,1)+MTB1(3,1))
          MATAN(6,4)=-LB*LB*(MTH1(1,3)+MTB1(1,3))
C
C
      ENDIF
C
C
C
C---Matrice tangente pour FULL_MECA(_ELAS) et RIGI_MECA(_ELAS)

      IF (OPTION(1:9).EQ.'FULL_MECA' 
     &           .OR. OPTION(1:9).EQ.'RIGI_MECA') THEN
C
        IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.(OPTION(10:14).EQ.'_ELAS'))
     &               THEN
C
C
          KXX=(H1+B1)*KN+R1*RN
          KYY=(H1*(1.D0-XPH1)+B1*(1.D0-XPB1))*KT+(R1*(1.D0-XPR1))*RT
          KZZ=(H1*(1.D0-ZPH1)+B1*(1.D0-ZPB1))*KT+R1*(1.D0-ZPR1)*RT
          KPP=LB*LB*(H1+B1)*KN
          KTT=LB*LB*(H1*(1.D0-ZPH1)+B1*(1.D0-ZPB1))*KT
          KXT=LB*(B1-H1)*KN
          KZP=LB*(H1*(1.D0-ZPH1)-B1*(1.D0-ZPB1))*KT
C
C  on remplit la matrice tangente dans le repere local
C
          CALL R8INIR(144,0.D0,MATLOC,1)
C
C on remplit le 1er bloc 6x6
          MATLOC(1,1)=KXX
          MATLOC(2,2)=KYY
          MATLOC(3,3)=KZZ
          MATLOC(4,4)=KPP
          MATLOC(6,6)=KTT
C--on ne remplit pas les termes extra-diagonaux pour RIGI_MECA_ELAS
C et FULL_MECA_ELAS
          IF (OPTION(10:14).NE.'_ELAS') THEN
            MATLOC(1,6)=KXT
            MATLOC(6,1)=KXT
            MATLOC(3,4)=KZP
            MATLOC(4,3)=KZP
          ENDIF
C
C on remplit les autres blocs
C
          DO 101 I=1,6
            DO 102 J=1,6
              MATLOC(I+6,J+6)=MATLOC(I,J)
              MATLOC(I,J+6)=-MATLOC(I,J)
              MATLOC(I+6,J)=-MATLOC(I,J)
 102        CONTINUE
 101      CONTINUE
C
        ELSE
C
          CALL R8INIR(144,0.D0,MATLOC,1)
          DO 103 I=1,6
            DO 104 J=1,6
              MATLOC(I,J)=MATAN(I,J)
              MATLOC(I+6,J)=-MATAN(I,J)
              MATLOC(I,J+6)=-MATAN(I,J)
              MATLOC(I+6,J+6)=MATAN(I,J)
104         CONTINUE
103       CONTINUE
C
        ENDIF
C
C
C on exprime la matrice tangente dans le repere global
C
        CALL R8INIR(144,0.D0,MATGLO,1)

        DO 123 I=1,3
          DO 124 L=1,3
            DO 125 P=1,4
            DO 126 Q=1,4 
              DO 127 J=1,3
                DO 128 K=1,3
                  MATGLO(I+3*(P-1),L+3*(Q-1))=
     &                   MATGLO(I+3*(P-1),L+3*(Q-1))
     &                  +PGL(J,I)*MATLOC(J+3*(P-1),K+3*(Q-1))*PGL(K,L)
 128            CONTINUE
 127          CONTINUE
 126        CONTINUE
 125        CONTINUE
 124      CONTINUE
 123    CONTINUE
C
C on range la matrice tangente sous forme d'un vecteur
C (rangement d'une matrice non symétrique)
C
        DO 860 I=1,12
          DO 861 J=1,12
             K=12*(I-1)+J
             KLGLO(K)=MATGLO(I,J)
 861      CONTINUE
 860    CONTINUE
C
      ENDIF
C
C
C---Calcul des forces nodales dans le repère global
C
      IF (OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION(1:9).EQ.'RAPH_MECA')
     &           THEN
        NNO = 2
        CALL UTPVLG(NNO,NC,PGL,FL,FONO)
      ENDIF
C
C
      END
