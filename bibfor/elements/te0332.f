      SUBROUTINE TE0332 (OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     OPTION,NOMTE
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/06/2002   AUTEUR F6BHHBO P.DEBONNIERES 
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
C     -----------------------------------------------------------------
C     FONCTION REALISEE :
C
C         CALCUL DU TAUX DE CROISSANCE DE CAVITES SELON UNE LOI DE
C         RICE ET TRACEY EN COMPORTEMENT NON-LINEAIRE.
C         ELEMENTS ISOPARAMETRIQUES 2D.
C
C         OPTION : 'RICE_TRACEY'
C
C ENTREE  --->  OPTION : NOM DE L'OPTION DE CALCUL
C         --->  NOMTE  : NOM DU TYPE D'ELEMENT
C
C     ------------------------------------------------------------------
C
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) ,ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*24       CARAC,FF
      CHARACTER*8        ELREFE
      CHARACTER*16       OPTCAL(12)
      REAL*8             SIG(6),TRIAX,VOLU,RSR0,NUMEMA,DEPSEQ
      REAL*8             POIDS,R,VOLUME,DVOL,SIGM,SIGEQ
      REAL*8             DFDX(9),DFDY(9)
      REAL*8             CONG(4),VARIGP,VARIGM,SDRSRP,SDRSRM,CROIS
      INTEGER            NNO,KP,NPG,K,IRITRA,ICOMPO,JTAB(7)
      INTEGER            ISSOPT,IMA,NBVARI,IPOPP
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK
      INTEGER            IGEOM,ICONG,IVARPG,ISDRMR,ISDRPR
C     ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO=ZI(ICARAC)
      NPG=ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF   ,'L',IFF   )
      IPOIDS=IFF
      IVF   = IPOIDS+NPG
      IDFDE = IVF   +NPG*NNO
      IDFDK = IDFDE +NPG*NNO
C
C     RECUPERATION DU NUMERO DE LA MAILLE :
C     -------------------------------------
      CALL TECAEL(IADZI,IAZK24)
      IMA =ZI(IADZI)
      NUMEMA= DBLE(IMA)
C
      POIDS=0.D0
      TRIAX=0.D0
      RSR0=0.D0
      VOLU=0.D0
      VOLUME=0.D0
      DVOL=0.D0
      DEPSEQ = 0.D0
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCONTPR','L',ICONG)
      CALL JEVECH('PVARIMR','L',IVARMG)
      CALL JEVECH('PVARIPR','L',IVARPG)
      CALL JEVECH('PSDRMR','L',ISDRMR)
      CALL JEVECH('PSOUSOP','L',ISSOPT)
      CALL TECACH(.TRUE.,.TRUE.,'PVARIPR',7,JTAB)
      NBVARI = MAX(JTAB(6),1)*JTAB(7)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
C     READ (ZK16(ICOMPO+1),'(I16)') NBVARI
C
      IF (     (ZK16(ICOMPO).EQ.'VMIS_ISOT_TRAC')
     &     .OR.(ZK16(ICOMPO).EQ.'VMIS_ISOT_LINE')
     &     .OR.(ZK16(ICOMPO).EQ.'LEMAITRE')
     &     .OR.(ZK16(ICOMPO).EQ.'VMIS_ECMI_TRAC')
     &     .OR.(ZK16(ICOMPO).EQ.'VMIS_ECMI_LINE')
     &     .OR.(ZK16(ICOMPO).EQ.'VISC_CIN1_CHAB')
     &     .OR.(ZK16(ICOMPO).EQ.'VISC_CIN2_CHAB') ) THEN
        IPOPP  = 1
      ELSE IF  (ZK16(ICOMPO).EQ.'CHABOCHE') THEN
        IPOPP  = 9
      ELSE
        CALL UTMESS('F','TE0332','POUR L''OPTION '//
     +          '"RICE_TRACEY", LA RELATION "'//ZK16(ICOMPO)//
     +          '" N''EST PAS ADMISE')
      END IF
C
      CALL JEVECH('PRICTRA','E',IRITRA)
      CALL JEVECH('PSDRPR','E',ISDRPR)
C
      OPTCAL(1) = ZK24(ISSOPT)(1:16)
      OPTCAL(2) = ZK24(ISSOPT)(17:19)
C
      DO 150 II=1,4
         CONG(II)=0.D0
 150  CONTINUE
      VARIGM =0.D0
      VARIGP =0.D0
C
C     --- BOUCLE SUR POINTS DE GAUSS SUIVANT OPTIONS DE CALCUL ---
C
      IF ((OPTCAL(1).EQ.'SIGM_ELMOY').AND.(OPTCAL(2).EQ.'NON')) THEN
         DO 200 KP=1,NPG
            K=(KP-1)*NNO
            R=0.D0
            CALL DFDM2D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                   ZR(IGEOM),DFDX,DFDY,POIDS)
            IF (NOMTE(3:4).EQ.'AX') THEN
               DO 160 II=1,NNO
                  R=R+ZR(IGEOM+2*II-2)*ZR(IVF+K+II-1)
 160           CONTINUE
               POIDS=POIDS*R
            ENDIF
            DVOL=POIDS
            VOLUME=VOLUME+DVOL
            DO 165 II=1,4
               CONG(II)=CONG(II)+DVOL*ZR(ICONG+4*KP+II-5)
 165        CONTINUE
            VARIGM = VARIGM+DVOL*ZR(IVARMG+NBVARI*(KP-1)+IPOPP-1)
            VARIGP = VARIGP+DVOL*ZR(IVARPG+NBVARI*(KP-1)+IPOPP-1)
 200     CONTINUE
C        ------- SIGXX MOYENNEE SUR L'ELEMENT -----------
         SIG(1)  =CONG(1)/VOLUME
C        ------- SIGYY MOYENNEE SUR L'ELEMENT -----------
         SIG(2)  =CONG(2)/VOLUME
C        ------- SIGZZ MOYENNEE SUR L'ELEMENT -----------
         SIG(3)  =CONG(3)/VOLUME
C        ------- SIGXY MOYENNEE SUR L'ELEMENT -----------
         SIG(4)  =CONG(4)/VOLUME
C        ------- EPSPEQ MOYENNEE SUR L'ELEMENT ----------
         VARIGM  =VARIGM/VOLUME
         VARIGP  =VARIGP/VOLUME
C
         SIGM = (SIG(1)+SIG(2)+SIG(3))/3.D0
         SIGEQ = (SIG(1)-SIGM)*(SIG(1)-SIGM)+(SIG(2)-SIGM)*
     &           (SIG(2)-SIGM)+(SIG(3)-SIGM)*(SIG(3)-SIGM)+2.D0*SIG(4)*
     &           SIG(4)
         SIGEQ = SQRT(1.5D0*SIGEQ)
         TRIAX = SIGM/SIGEQ
         VOLU = VOLUME
         DEPSEQ = VARIGP-VARIGM
         DO 167 KQ=1,NPG
            ZR(ISDRPR+KQ-1) = ZR(ISDRMR+KQ-1)
 167     CONTINUE
C
C
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELGA').AND.(OPTCAL(2).EQ.'OUI'))
     &       THEN
         DO 300 KP=1,NPG
            R=0.D0
            SDRSRM = ZR(ISDRMR+KP-1)
            K=(KP-1)*NNO
            CALL DFDM2D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                   ZR(IGEOM),DFDX,DFDY,POIDS)
            IF (NOMTE(3:4).EQ.'AX') THEN
               DO 170 II=1,NNO
                  R=R+ZR(IGEOM+2*II-2)*ZR(IVF+K+II-1)
 170           CONTINUE
               POIDS=POIDS*R
            ENDIF
            VOLUME=POIDS
            DO 180 II=1,4
               CONG(II)=ZR(ICONG+4*KP+II-5)
 180        CONTINUE
            VARIGM =ZR(IVARMG+NBVARI*(KP-1)+IPOPP-1)
            VARIGP =ZR(IVARPG+NBVARI*(KP-1)+IPOPP-1)
C
            SIGM = (CONG(1)+CONG(2)+CONG(3))/3.D0
            SIGEQ = (CONG(1)-SIGM)*(CONG(1)-SIGM)+(CONG(2)-SIGM)*
     &          (CONG(2)-SIGM)+(CONG(3)-SIGM)*(CONG(3)-SIGM)+
     &          2.D0*CONG(4)* CONG(4)
            SIGEQ = SQRT(1.5D0*SIGEQ)
            TRIAX = SIGM/SIGEQ
            VOLU = VOLUME
            DEPSEQ = VARIGP-VARIGM
            SDRSRP=SDRSRM+0.283D0*SIGN(1.D0,TRIAX)*EXP(1.5D0*
     &                    ABS(TRIAX))*DEPSEQ
            ZR(ISDRPR+KP-1) = SDRSRP
            CROIS = EXP(SDRSRP)
            IF (CROIS.GT.RSR0) THEN
               RSR0=CROIS
               VOLU=VOLUME
            ENDIF
C
 300     CONTINUE
C
C
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELMOY').AND.(OPTCAL(2).EQ.'OUI'))
     &       THEN
         DO 400 KP=1,NPG
            R=0.D0
            K=(KP-1)*NNO
            CALL DFDM2D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                   ZR(IGEOM),DFDX,DFDY,POIDS)
            IF (NOMTE(3:4).EQ.'AX') THEN
               DO 210 II=1,NNO
                  R=R+ZR(IGEOM+2*II-2)*ZR(IVF+K+II-1)
 210           CONTINUE
               POIDS=POIDS*R
            ENDIF
            DVOL=POIDS
            VOLUME=VOLUME+DVOL
            DO 220 II=1,4
               CONG(II)=CONG(II)+ZR(ICONG+4*KP+II-5)*DVOL
 220        CONTINUE
            VARIGM=VARIGM+DVOL*ZR(IVARMG+NBVARI*(KP-1)+IPOPP-1)
            VARIGP=VARIGP+DVOL*ZR(IVARPG+NBVARI*(KP-1)+IPOPP-1)
 400     CONTINUE
C        ------- SIGXX MOYENNEE SUR L'ELEMENT -----------
         SIG(1)  =CONG(1)/VOLUME
C        ------- SIGYY MOYENNEE SUR L'ELEMENT -----------
         SIG(2)  =CONG(2)/VOLUME
C        ------- SIGZZ MOYENNEE SUR L'ELEMENT -----------
         SIG(3)  =CONG(3)/VOLUME
C        ------- SIGXY MOYENNEE SUR L'ELEMENT -----------
         SIG(4)  =CONG(4)/VOLUME
C        ------- EPSPEQ MOYENNEE SUR L'ELEMENT ----------
         VARIGM=VARIGM/VOLUME
         VARIGP=VARIGP/VOLUME
C
         SIGM = (SIG(1)+SIG(2)+SIG(3))/3.D0
         SIGEQ = (SIG(1)-SIGM)*(SIG(1)-SIGM)+(SIG(2)-SIGM)*
     &           (SIG(2)-SIGM)+(SIG(3)-SIGM)*(SIG(3)-SIGM)+2.D0*SIG(4)*
     &           SIG(4)
         SIGEQ = SQRT(1.5D0*SIGEQ)
         TRIAX = SIGM/SIGEQ
         VOLU = VOLUME
         DEPSEQ = VARIGP-VARIGM
         SDRSRM = ZR(ISDRMR)
         SDRSRP = SDRSRM+0.283D0*SIGN(1.D0,TRIAX)*EXP(1.5D0*
     &                            ABS(TRIAX))*DEPSEQ
         DO 225 KQ=1,NPG
            ZR(ISDRPR+KQ-1) = SDRSRP
 225     CONTINUE
         RSR0 = EXP(SDRSRP)
C
C
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELGA').AND.(OPTCAL(2).EQ.'NON'))
     &       THEN
         DO 100 KP=1,NPG
            K=(KP-1)*NNO
            R=0.D0
            DO 175 II=1,4
               CONG(II)=ZR(ICONG+(4*KP)-5+II)
 175        CONTINUE
            VARIGM=ZR(IVARMG+NBVARI*(KP-1)+IPOPP-1)
            VARIGP=ZR(IVARPG+NBVARI*(KP-1)+IPOPP-1)
            CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                    ZR(IGEOM),DFDX,DFDY,POIDS )
            IF (NOMTE(3:4).EQ.'AX') THEN
               DO 240 II=1,NNO
                  R=R+ZR(IGEOM+2*II-2)*ZR(IVF+K+II-1)
 240           CONTINUE
               POIDS=POIDS*R
            ENDIF
            DVOL=POIDS
            VOLUME=VOLUME+DVOL
            SIGM = (CONG(1)+CONG(2)+CONG(3))/3.D0
            SIGEQ = (CONG(1)-SIGM)*(CONG(1)-SIGM)+(CONG(2)-SIGM)*
     &          (CONG(2)-SIGM)+(CONG(3)-SIGM)*(CONG(3)-SIGM)+
     &          2.D0*CONG(4)* CONG(4)
            SIGEQ = SQRT(1.5D0*SIGEQ)
            TRIAX = TRIAX+DVOL*SIGM/SIGEQ
            DEPSEQ = DEPSEQ+(VARIGP-VARIGM)*DVOL
C
 100     CONTINUE
C
         TRIAX = TRIAX/VOLUME
         DEPSEQ = DEPSEQ/VOLUME
         VOLU = VOLUME
         DO 177 KQ=1,NPG
            ZR(ISDRPR+KQ-1) = ZR(ISDRMR+KQ-1)
 177     CONTINUE
C
C
      ELSE
      CALL UTMESS('F','TE0332','OPTION DE CALCUL NON VALIDE')
      ENDIF
C
C
      ZR(IRITRA) = TRIAX
      ZR(IRITRA+1) = RSR0
      ZR(IRITRA+2) = VOLU
      ZR(IRITRA+3) = NUMEMA
      ZR(IRITRA+4) = DEPSEQ
C
C     DESTRUCTION DES OBJETS CREES DANS LA BASE
C
      END
