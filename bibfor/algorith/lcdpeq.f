        SUBROUTINE LCDPEQ(VIND, VINF,LOI,NBCOMM,CPMONO,NMAT,NVI,SIG,
     &  COTHE,COEFF,HSR)
     
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/12/2010   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       DEFORMATION PLASTIQUE EQUIVALENTE CUMULEE MACROSCOPIQUE
C       POUR LE MONOCRISTAL
C       IN  VIND   :  VARIABLES INTERNES A T
C       IN  VINF   :  VARIABLES INTERNES A T+DT
C       ----------------------------------------------------------------
        INTEGER         NVI,NMAT,NBCOMM(NMAT,3),NBPHAS,I,IPHAS,INDFV
        INTEGER         NUVI,INDPHA,NBFSYS,IFA,IS,NBSYS,IRET
        REAL*8          VIND(NVI),VINF(NVI),DVIN(NVI),SIG(6),GRANB(6)
        REAL*8          LCNRTE, EPSEQ,COTHE(NMAT),COEFF(NMAT),E,NU,FV
        REAL*8          SIGG(6),MAXRP,ZE(12),RP,HSR(5,24,24)
        CHARACTER*16    LOI,CPMONO(5*NMAT+1),LOCA
      CHARACTER*16 NOMFAM,NECRIS,NECOUL
C V.I. 1 a 6 représente la deformation viscoplastique macro
        EPSEQ=0
        DO 10 I=1,6
            DVIN(I)=VINF(I)-VIND(I)
            EPSEQ=EPSEQ+DVIN(I)*DVIN(I)
10      CONTINUE
        EPSEQ = SQRT ( 2.0D0/3.0D0* EPSEQ )
        CALL R8INIR(12, 0.D0, ZE, 1)                        
     
        NBFSYS=NBCOMM(NMAT,2)                                
        MAXRP=-1.D20
         
         IF (LOI(1:8).EQ.'MONOCRIS') THEN
            VINF (NVI-1) = VIND (NVI-1) + EPSEQ
            DO 6 IFA=1,NBFSYS
               NOMFAM=CPMONO(5*(IFA-1)+1)
               NECOUL=CPMONO(5*(IFA-1)+3)
               IF(NECOUL.EQ.'MONO_DD_CFC') THEN
                  NECRIS=CPMONO(5*(IFA-1)+4)
                  CALL LCMMSG(NOMFAM,NBSYS,0,ZE,ZE,ZE,ZE,0,ZE)
                  DO 7 IS=1,NBSYS
                     CALL LCMMFI(COEFF,IFA,NMAT,NBCOMM,NECRIS,
     &              IS,NBSYS,VINF(7),ZE,HSR,1,ZE,RP)
                     MAXRP=MAX(RP,MAXRP)
  7               CONTINUE
                  VINF(NVI-2)=MAXRP
               ENDIF
  6         CONTINUE
         ELSEIF (LOI(1:8).EQ.'POLYCRIS') THEN

            VINF (7) = VIND (7) + EPSEQ
C           LOCALISATION
C           RECUPERATION DU NOMBRE DE PHASES
            NBPHAS=NBCOMM(1,1)
            LOCA=CPMONO(1)
C           CALCUL DE  B
            DO 53 I=1,6
               GRANB(I)=0.D0
53          CONTINUE
            DO 54 I=1,6
            DO 54 IPHAS=1,NBPHAS
               INDFV=NBCOMM(1+IPHAS,3)
               FV=COEFF(INDFV)
               GRANB(I)=GRANB(I)+FV*VINF(7+6*(IPHAS-1)+I)
54          CONTINUE
            NUVI=NVI-6*NBPHAS-1
            DO 1 IPHAS=1,NBPHAS
             INDFV=NBCOMM(1+IPHAS,3)
C            recuperer l'orientation de la phase et la proportion
             FV=COEFF(INDFV)
             E=COTHE(1)
             NU=COTHE(2)
             CALL LCLOCA(COEFF,E,NU,NMAT,NBCOMM,NBPHAS,SIG,VINF,
     &               IPHAS,GRANB,LOCA,SIGG)
               DO 2 I=1,6
                  VINF(NUVI+6*(IPHAS-1)+I)=SIGG(I)
   2           CONTINUE
   1        CONTINUE
         ENDIF

         IF (EPSEQ.EQ.0.D0) THEN
            VINF (NVI) = 0.D0
         ELSE
            VINF (NVI) = 1.D0
         ENDIF

        END
