      SUBROUTINE ENELPG(FAMI,IADMAT,INSTAN,IGAU,REPERE,XYZGAU,COMPOR,
     &                  F,SIGMA,NBVARI,VARI,ENELAS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/10/2012   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C.......................................................................
      IMPLICIT NONE      
C -----------------------------------------------------------------

C  OPTION ENEL_ELGA : CALCUL DE L'ENERGIE DE DEFORMATION ELASTIQUE
C  ================   DETERMINEE PAR L'EXPRESSION SUIVANTE :

C  EN HPP
C   ENELAS =  SOMME_VOLUME((SIG_T*(1/D)*SIG).DV)
C
C        OU  .SIG       EST LE TENSEUR DES CONTRAINTES DE CAUCHY
C            .D         EST LE TENSEUR DE HOOKE
C
C  EN GRANDES DEFORMATIONS SIMO MIEHE POUR ELAS OU VMIS_ISOT
C   ENERLAS = ENERGIE ELASTIQUE SPECIFIQUE
C           = K/2(0.5(J^2-1)-lnJ)+0.5mu(tr(J^(-2/3)be)-3)
C           SI PRESENCE DE THERMIQUE, ON AJOUTE UNE CORRECTION
C           SPECIFIQUE PRESENTEE DANS LA DOC R
C  EN GRANDES DEFORMATIONS GDEF_LOG
C   ENERELAS = SOMME_VOLUME((T_T*(1/D)*T).DV)
C        OU  .T       EST LE TENSEUR DES CONTRAINTES DU FORMALISME
C            .D         EST LE TENSEUR DE HOOKE
C -----------------------------------------------------------------
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................

C-----------------------------------------------------------------------
      INTEGER NBSIGM,NBSIG,NBVARI,NSOL,I,IADMAT,IGAU,ICODRE(2),ISIG,JSIG
      REAL*8 C1 ,C2 ,DEUX ,VARI(*),ENELAS,TRT ,UN ,UNDEMI,ZERO
      REAL*8 SOL(3),SIGMA(6),JZERO,UZERO,MZERO,INSTAN,EPSI(6)
      REAL*8 MJAC,UJAC,WBE,BE(6),E,NU,F(3,3),REPERE(7),XYZGAU(3)
      REAL*8 MU,TROISK,JAC,TAU(6),TRTAU,EQTAU,DVTAU(6),TLOG(6)
      REAL*8 TRBE,EPSTHE,KR(6),PDTSCA(6),D1(36),VALRES(2)
      LOGICAL LTEATT
      CHARACTER*4  FAMI
      CHARACTER*8  NOMRES(2)
      CHARACTER*16 COMPOR(*)
      DATA KR/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA PDTSCA/1.D0,1.D0,1.D0,2.D0,2.D0,2.D0/
C-----------------------------------------------------------------------

      ZERO = 0.0D0
      DEUX=2.0D0
      UNDEMI=0.5D0
      UN=1.0D0
      NBSIG = NBSIGM()
      ENELAS=0.D0
      NSOL   = 0
      JZERO  = ZERO
      UZERO  = ZERO
      UJAC   = ZERO
      MZERO  = ZERO
      MJAC   = ZERO
      WBE =ZERO
      DO 10 I =1,3
         SOL(I)=ZERO
 10   CONTINUE


C --- CAS EN GRANDES DEFORMATIONS SIMO_MIEHE
      IF ((COMPOR(3).EQ.'SIMO_MIEHE') .AND.
     & ((COMPOR(1)(1:9).EQ.'VMIS_ISOT').OR.(COMPOR(1).EQ.'ELAS'))) THEN
     
C ---    RECUPERATION DES CARACTERISTIQUES DU MATERIAU :
         NOMRES(1) = 'E'
         NOMRES(2) = 'NU'
         CALL RCVALB(FAMI,IGAU,1,'+',IADMAT,' ','ELAS',0,' ',
     &               0.D0,2,NOMRES,VALRES,ICODRE,2)
         E = VALRES(1)
         NU = VALRES(2)
         MU = E/ (2.D0* (1.D0+NU))
         TROISK = E/ (1.D0-2.D0*NU)

         JAC = F(1,1)* (F(2,2)*F(3,3)-F(2,3)*F(3,2)) -
     &         F(2,1)* (F(1,2)*F(3,3)-F(1,3)*F(3,2)) +
     &         F(3,1)* (F(1,2)*F(2,3)-F(1,3)*F(2,2))

C ---    CALCUL DE TAU TEL QUE TAU=JAC*SIGMA

         TAU(5) = 0.D0
         TAU(6) = 0.D0

         DO 60 I = 1,NBSIG
           TAU(I) = JAC*SIGMA(I)
   60    CONTINUE

C ---    CALCUL DE LA TRACE DE TAU- TAU EQUIVALENT ET TAU DEVIATORIQUE

         TRTAU = TAU(1) + TAU(2) + TAU(3)
         EQTAU = 0.D0
         DO 70 I = 1,6
           DVTAU(I) = TAU(I) - KR(I)*TRTAU/3.D0
           EQTAU = EQTAU + PDTSCA(I)* (DVTAU(I)**2.D0)
   70    CONTINUE
         EQTAU = SQRT(1.5D0*EQTAU)

C ---    CALCUL DE LA TRACE DES DEFORMATIONS ELASTIQUES BE

         CALL DCOPY(6,VARI(2),1,BE,1)
         TRBE=BE(1)+BE(2)+BE(3)
         TRBE=JAC**(-2.D0/3.D0)*(3.D0-2.D0*TRBE)

C  ---   DEFORMATION THERMIQUE AU POINT D'INTEGRATION COURANT :

         CALL VERIFT(FAMI,IGAU,1,'+',IADMAT,'ELAS',1,EPSTHE,ICODRE)


C ---    ATTENTION, EN PRESENCE DE THERMIQUE, CA MET LE BAZARD...
         IF (EPSTHE.NE.0) THEN
            CALL ZEROP3(-3.D0*EPSTHE,-1.D0,-3.D0*EPSTHE,SOL,NSOL)
            JZERO=SOL(1)
            CALL NRSMT1(TROISK/3.D0,JZERO,UZERO)
            CALL NRSMTT(TROISK,JZERO,EPSTHE,MZERO)
            CALL NRSMTT(TROISK,JAC,EPSTHE,MJAC)
         END IF

C ---    CALCUL DES TERMES DE L'ENERGIE            
         CALL NRSMT1(TROISK/3.D0,JAC,UJAC)
         CALL NRSMTB(MU,TRBE,WBE)

         ENELAS = UJAC+WBE+MJAC-UZERO-MZERO


C --- CAS EN GRANDES DEFORMATIONS GDEF_LOG

      ELSEIF ((COMPOR(3)(1:8).EQ.'GDEF_LOG')) THEN

C ---    RECUPERATION DES CARACTERISTIQUES DU MATERIAU :
         NOMRES(1) = 'E'
         NOMRES(2) = 'NU'
         CALL RCVALB(FAMI,IGAU,1,'+',IADMAT,' ','ELAS',0,' ',
     &               0.D0,2,NOMRES,VALRES,ICODRE,2)
         E = VALRES(1)
         NU = VALRES(2)

         C1 = (UN+NU)/E
         C2 = NU/E
         CALL DCOPY(6,VARI(NBVARI-5),1,TLOG,1)
         
C        CAS 3D         
         IF(LTEATT(' ','DIM_TOPO_MAILLE','3')) THEN
         
            TRT=TLOG(1)+TLOG(2)+TLOG(3)
            ENELAS = UNDEMI* (TLOG(1)* (C1*TLOG(1)-C2*TRT)+
     &            TLOG(2)* (C1*TLOG(2)-C2*TRT)+
     &            TLOG(3)* (C1*TLOG(3)-C2*TRT)+
     &            (TLOG(4)*C1*TLOG(4)+TLOG(5)*C1*TLOG(5)+
     &            TLOG(6)*C1*TLOG(6)))


C ---    CAS DES CONTRAINTES PLANES :
         ELSEIF (LTEATT(' ','C_PLAN','OUI')) THEN
            TRT=TLOG(1)+TLOG(2)

            ENELAS = UNDEMI*
     &         (TLOG(1)*(C1*TLOG(1)-C2*TRT)
     &        + TLOG(2)*(C1*TLOG(2)-C2*TRT)
     &        + DEUX*TLOG(4)*C1*TLOG(4))
C ---    CAS AXI ET DEFORMATIONS PLANES :
         ELSE
            TRT=TLOG(1)+TLOG(2)+TLOG(3)

             ENELAS = UNDEMI
     &       *(TLOG(1)*(C1*TLOG(1)-C2*TRT)
     &       +TLOG(2)*(C1*TLOG(2)-C2*TRT)
     &       +TLOG(3)*(C1*TLOG(3)-C2*TRT)
     &       +DEUX*TLOG(4)*C1*TLOG(4))
         ENDIF
C --- EN HPP SI ON CONSIDERE LE MATERIAU ISOTROPE
C --- E_ELAS = 1/2*SIGMA*1/D*SIGMA :

C --- CAS EN GRANDES DEFORMATIONS SIMO_MIEHE
      ELSEIF ((COMPOR(3)(1:5).EQ.'PETIT').OR.
     &        (COMPOR(3).EQ.'GROT_GDEP')) THEN
     
C  --    CALCUL DE L'INVERSE DE LA MATRICE DE HOOKE (LE MATERIAU
C  --    POUVANT ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
C        ---------------------------------------------------------
         CALL D1MAMC(FAMI, IADMAT, INSTAN, '+',IGAU, 1,
     +               REPERE,XYZGAU, NBSIG, D1)
C
C  --    DENSITE D'ENERGIE POTENTIELLE ELASTIQUE AU POINT
C  --    D'INTEGRATION COURANT
C        ---------------------
         CALL R8INIR(6,0.D0,EPSI,1)
         DO 80 ISIG = 1, NBSIG
            DO 90 JSIG = 1, NBSIG
               EPSI(ISIG)=EPSI(ISIG)+D1(NBSIG*(ISIG-1)+JSIG)*SIGMA(JSIG)
  90        CONTINUE
            ENELAS = ENELAS + UNDEMI*SIGMA(ISIG)*EPSI(ISIG)
  80     CONTINUE
         
      ELSE
         CALL U2MESG('F','COMPOR1_77',1,COMPOR(3),0,0,0,0.D0)
      ENDIF

      END
