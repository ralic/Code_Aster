        SUBROUTINE LCDPEQ(VIND, VINF,COMP,NBCOMM,CPMONO,NMAT,NVI,SIG,
     &  DETOT,EPSD,MATERF,PGL)

        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/09/2012   AUTEUR PROIX J-M.PROIX 
C TOLE CRS_1404
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
C     DEFORMATION PLASTIQUE EQUIVALENTE CUMULEE MACROSCOPIQUE
C     POUR LE MONOCRISTAL
C     IN  VIND   :  VARIABLES INTERNES A T
C     IN  VINF   :  VARIABLES INTERNES A T+DT
C          COMP   :  NOM MODELE DE COMPORTEMENT
C          NBCOMM :  INDICES DES COEF MATERIAU
C          CPMONO :  NOMS DES LOIS MATERIAU PAR FAMILLE
C          NMAT   :  DIMENSION MATER
C          VIND   :  VARIABLES INTERNES A T
C          SIG    :  CONTRAINTES A T
C          DETOT  :  INCREMENT DE  DEFORMATION TOTALE OU DF
C          EPSD   :  DEFORMATION TOTALE A T OU F A T
C     VAR  NVI    :  NOMBRE DE VARIABLES INTERNES
C          VINF   :  VARIABLES INTERNES A T+DT
C          MATERF :  COEF MATERIAU
C     ----------------------------------------------------------------
      INTEGER NVI,NMAT,NBCOMM(NMAT,3),NBPHAS,I,IPHAS,INDFV,NUVI,IFA
      INTEGER IFL,IS,NBFSYS,NBSYS,NSFV,INDPHA,INDCP,NUMIRR,NS,INDTAU
      INTEGER IEI,IS3,IV3,IV,IRR2
      REAL*8  VIND(NVI),VINF(NVI),DVIN(NVI),SIG(6),GRANB(6)
      REAL*8  EPSEQ,E,NU,FV,SIGG(6),MUS(6),NG(3),LG(3),PGL(3,3)
      REAL*8  ID(3,3),F(3,3),FPM(3,3),FP(3,3),FE(3,3),DETP,LCNRTE
      REAL*8  DETOT(*),EPSD(*),PK2(6),DEVI(6),ENDOC,DP,XI,QM(3,3)
      REAL*8  MATERF(NMAT,2),RHOIRR(12),TAU(60)
      REAL*8  RHOSAT,PHISAT,DZ,ROLOOP(12),FIVOID(12),SDP,DPS(12)
      CHARACTER*16 LOI,CPMONO(5*NMAT+1),LOCA,COMP(*),NECOUL,NOMFAM
      INTEGER IRR,DECIRR,NBSYST,DECAL
      COMMON/POLYCR/IRR,DECIRR,NBSYST,DECAL
      DATA    ID/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/    

      LOI  = COMP(1)
      IF (LOI(1:8).EQ.'MONOCRIS')  THEN
         NVI = NVI +3
         IF (COMP(3)(1:4).EQ.'SIMO') THEN
            NVI=NVI+9
         ENDIF
      ENDIF

      IF (LOI(1:8).EQ.'MONOCRIS') THEN      
  
         NBFSYS=NBCOMM(NMAT,2)
C        NSFV : debut de la famille IFA dans les variables internes
         NSFV=6
         DO 6 IFA=1,NBFSYS
            IFL=NBCOMM(IFA,1)
C            NUECOU=NINT(MATERF(IFL,2))
            NOMFAM=CPMONO(5*(IFA-1)+1)
            NECOUL=CPMONO(5*(IFA-1)+3)
            CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MUS,NG,LG,0,QM)
            IF (NECOUL.EQ.'MONO_DD_CC_IRRA') THEN
               CALL DCOPY(12, VIND(NSFV+3*NBSYS+1),1,RHOIRR,1)
               XI=MATERF(IFL+23,2)
               IRR=1
               IRR2=1
            ELSEIF (NECOUL.EQ.'MONO_DD_CFC_IRRA') THEN
               CALL DCOPY(12, VIND(NSFV+3*NBSYS+1),1,ROLOOP,1)
               CALL DCOPY(12, VIND(NSFV+3*NBSYS+13),1,FIVOID,1)
               IRR=1
               IRR2=2
               IEI   =NBCOMM(IFA,3)
               RHOSAT=MATERF(IEI+8,2)
               PHISAT=MATERF(IEI+9,2)
               XI   = MATERF(IEI+10,2)
               DZ   = MATERF(IEI+11,2)
            ELSE
               IRR=0
               IRR2=0
            ENDIF
            
            IF(IRR2.EQ.1) THEN
               DO 7 IS=1,12
C                 VARIABLES INTERNES PAR SYSTEME DE GLISSEMENT
                  NUVI=NSFV+3*(IS-1)+3
                  DP=VINF(NUVI)
                  RHOIRR(IS)=RHOIRR(IS)*EXP(-XI*DP)
  7            CONTINUE
               CALL DCOPY(12, RHOIRR,1,VINF(NSFV+3*NBSYS+1),1)
               
            ENDIF
           
            IF(IRR2.EQ.2) THEN
               DO 8 IS=1,12
C                 SOMME SUR COPLA(S)
                  SDP=0.D0
                  DO 9 IV=1,12
                     IS3=(IS-1)/3
                     IV3=(IV-1)/3
C                    VARIABLES INTERNES PAR SYSTEME DE GLISSEMENT
                     NUVI=NSFV+3*(IV-1)+3
                     DP=VINF(NUVI)
                     IF (IS3.EQ.IV3) THEN
                        SDP=SDP+DP
                     ENDIF
  9               CONTINUE
                  ROLOOP(IS)=RHOSAT+(ROLOOP(IS)-RHOSAT)*EXP(-XI*SDP)
                  FIVOID(IS)=PHISAT+(FIVOID(IS)-PHISAT)*EXP(-DZ*SDP)
  8            CONTINUE
               CALL DCOPY(12, ROLOOP,1,VINF(NSFV+3*NBSYS+1),1)
               CALL DCOPY(12, FIVOID,1,VINF(NSFV+3*NBSYS+13),1)
            ENDIF
         
            NSFV=NSFV+NBSYS*3
  6      CONTINUE
  
  
         INDTAU=NSFV
         IF (IRR2.EQ.1) INDTAU=INDTAU+12
         IF (IRR2.EQ.2) INDTAU=INDTAU+24
C        CISSIONS TAU_S  
         NS=0
         DO 61 IFA=1,NBFSYS
            IFL=NBCOMM(IFA,1)
            NOMFAM=CPMONO(5*(IFA-1)+1)
            CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MUS,NG,LG,0,QM)
            DO 71 IS=1,NBSYS
C              CALCUL DE LA SCISSION REDUITE =
C              PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
C              TAU      : SCISSION REDUITE TAU=SIG:MUS
               CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MUS,NG,LG,0,QM)
               TAU(NS+IS)=0.D0
               DO 102 I=1,6
                  TAU(NS+IS)=TAU(NS+IS)+SIG(I)*MUS(I)
 102           CONTINUE
  71        CONTINUE
            NS=NS+NBSYS
  61     CONTINUE
         CALL DCOPY(NS,TAU,1,VINF(INDTAU+1),1)
            
  
         IF (COMP(3)(1:5).NE.'PETIT') THEN
C           ICI CONTRAIREMENT A LCMMON, NVI EST LE NOMBRE TOTAL DE V.I
            CALL DCOPY(9,VINF(NVI-3-18+1 ),1,FP,1)
            CALL MATINV('S',3,FP,FPM,DETP)
            CALL PMAT(3,DETOT,EPSD,F)
            CALL PMAT(3,F,FPM,FE)
C           CALCUL DES CONTRAINTES DE KIRCHOFF
            CALL DCOPY(6,SIG,1,PK2,1)
            CALL DSCAL(3,SQRT(2.D0),PK2(4),1)
            CALL PK2SIG(3,FE,1.D0,PK2,SIG,1)
C           LES RACINE(2) ATTENDUES PAR NMCOMP :-)       
            CALL DSCAL(3,SQRT(2.D0),SIG(4),1)
            CALL DAXPY(9,-1.D0,ID,1,FE,1)
            CALL DCOPY(9,FE,1,VINF(NVI-3-18+10),1)
            CALL LCGRLA(FP,DEVI)
            CALL DCOPY(6,DEVI,1,VINF,1)
            CALL DSCAL(3,SQRT(2.D0),DEVI(4),1) 
            CALL DAXPY(9,-1.D0,ID,1,FP,1)
            CALL DCOPY(9,FP,1,VINF(NVI-3-18+1 ),1)
            EPSEQ = LCNRTE(DEVI)
         ELSE
C           V.I. 1 A 6 REPRÈSENTE LA DEFORMATION VISCOPLASTIQUE MACRO
            EPSEQ=0
            DO 10 I=1,6
                DVIN(I)=VINF(I)-VIND(I)
                EPSEQ=EPSEQ+DVIN(I)*DVIN(I)
10          CONTINUE
            EPSEQ = SQRT ( 2.0D0/3.0D0* EPSEQ )
         ENDIF
         VINF (NVI-1) = VIND (NVI-1) + EPSEQ
         
      ELSEIF (LOI(1:8).EQ.'POLYCRIS') THEN

C        V.I. 1 A 6 REPRÈSENTE LA DEFORMATION VISCOPLASTIQUE MACRO
         EPSEQ=0
         DO 20 I=1,6
             DVIN(I)=VINF(I)-VIND(I)
             EPSEQ=EPSEQ+DVIN(I)*DVIN(I)
20       CONTINUE
         EPSEQ = SQRT ( 2.0D0/3.0D0* EPSEQ )
         VINF (7) = VIND (7) + EPSEQ
C        LOCALISATION
C        RECUPERATION DU NOMBRE DE PHASES
         NBPHAS=NBCOMM(1,1)
         LOCA=CPMONO(1)
C        CALCUL DE  B
         DO 53 I=1,6
            GRANB(I)=0.D0
53       CONTINUE
         DO 54 I=1,6
         DO 54 IPHAS=1,NBPHAS
            INDFV=NBCOMM(1+IPHAS,3)
            FV=MATERF(INDFV,2)
            GRANB(I)=GRANB(I)+FV*VINF(7+6*(IPHAS-1)+I)
54       CONTINUE
         NUVI=NVI-6*NBPHAS-1
         DO 1 IPHAS=1,NBPHAS
          INDFV=NBCOMM(1+IPHAS,3)
C         RECUPERER L'ORIENTATION DE LA PHASE ET LA PROPORTION
          FV=MATERF(INDFV,2)
          E =MATERF(1,1)
          NU=MATERF(2,1)
          CALL LCLOCA(MATERF(1,2),E,NU,NMAT,NBCOMM,NBPHAS,SIG,VINF,
     &            IPHAS,GRANB,LOCA,SIGG)
            DO 2 I=1,6
               VINF(NUVI+6*(IPHAS-1)+I)=SIGG(I)
   2        CONTINUE
   1     CONTINUE

C        IRRADIATION   
         NSFV=7+6*NBPHAS
         NUMIRR=0
         DO 33 IPHAS=1,NBPHAS
            INDPHA=NBCOMM(1+IPHAS,1)
            NBFSYS=NBCOMM(INDPHA,1)
            INDCP=NBCOMM(1+IPHAS,2)
            DO 32 IFA=1,NBFSYS
               NECOUL=CPMONO(INDCP+5*(IFA-1)+3)

               IF (NECOUL.EQ.'MONO_DD_CC_IRRA') THEN
                  NBSYS=12
                  CALL DCOPY(12, VIND(DECIRR+NUMIRR+1),1,RHOIRR,1)
                  IFL=NBCOMM(INDPHA+IFA,1)
                  XI=MATERF(IFL+23,2)
                  DO 31 IS=1,NBSYS
C                    VARIABLES INTERNES PAR SYSTEME DE GLISSEMENT
                     NUVI=NSFV+3*(IS-1)+3
                     DP=VINF(NUVI)
                     IF(IRR.EQ.1) THEN
                        RHOIRR(IS)=RHOIRR(IS)*EXP(-XI*DP)
                     ENDIF
  31              CONTINUE
                  CALL DCOPY(12,RHOIRR,1,VINF(DECIRR+NUMIRR+1),1)
                  NUMIRR=NUMIRR+NBSYS
               ENDIF

               IF (NECOUL.EQ.'MONO_DD_CFC_IRRA') THEN         
                  NBSYS=12
                  CALL DCOPY(12, VIND(DECIRR+NUMIRR+1),1,ROLOOP,1)
                  CALL DCOPY(12, VIND(DECIRR+NUMIRR+13),1,FIVOID,1)
                  IEI   =NBCOMM(INDPHA+IFA,3)
                  RHOSAT=MATERF(IEI+8,2)
                  PHISAT=MATERF(IEI+9,2)
                  XI   = MATERF(IEI+10,2)
                  DZ   = MATERF(IEI+11,2)
                  DO 81 IS=1,NBSYS
C                    SOMME SUR COPLA(S)
                     SDP=0.D0
                     DO 91 IV=1,12
C                       VARIABLES INTERNES PAR SYSTEME DE GLISSEMENT
                        NUVI=NSFV+3*(IV-1)+3
                        DP=VINF(NUVI)
                        IS3=(IS-1)/3
                        IV3=(IV-1)/3
C                       PARTIE POSITIVE DE ALPHA
                        IF (IS3.EQ.IV3) THEN
                           SDP=SDP+DP
                        ENDIF
  91                 CONTINUE
                     ROLOOP(IS)=RHOSAT+(ROLOOP(IS)-RHOSAT)*EXP(-XI*SDP)
                     FIVOID(IS)=PHISAT+(FIVOID(IS)-PHISAT)*EXP(-DZ*SDP)
  81              CONTINUE
                  CALL DCOPY(12, ROLOOP,1,VINF(DECIRR+NUMIRR+1),1)
                  CALL DCOPY(12, FIVOID,1,VINF(DECIRR+NUMIRR+13),1)
                  NUMIRR=NUMIRR+NBSYS+NBSYS
               ENDIF         

               NSFV=NSFV+NBSYS*3
  32        CONTINUE
  33     CONTINUE
      ENDIF

      IF (EPSEQ.EQ.0.D0) THEN
         VINF (NVI) = 0.D0
      ELSE
         VINF (NVI) = 1.D0
      ENDIF

C --    DEBUT TRAITEMENT DE VENDOCHAB --
C --    CALCUL DES CONTRAINTES SUIVANT QUE LE MATERIAU EST
C --    ENDOMMAGE OU PAS

      IF (LOI(1:9).EQ.'VENDOCHAB') THEN
C --    DEBUT TRAITEMENT DE VENDOCHAB --
C --    CALCUL DE DSDE SUIVANT QUE LE MATERIAU EST ENDOMMAGE OU PAS
        ENDOC=(1.0D0-VINF(9))
        MATERF(1,1)=MATERF(1,1)*ENDOC
      ENDIF
     
      IF (LOI(1:8).EQ.'HAYHURST') THEN
C --    DEBUT TRAITEMENT DE HAYHURST --
C --    CALCUL DE DSDE SUIVANT QUE LE MATERIAU EST
C --    ENDOMMAGE OU PAS
        ENDOC=(1.0D0-VINF(11))
        MATERF(1,1)=MATERF(1,1)*ENDOC
C --    FIN   TRAITEMENT DE HAYHURST --
      ENDIF
      
      END
