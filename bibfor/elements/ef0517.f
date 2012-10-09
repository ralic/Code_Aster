      SUBROUTINE EF0517(NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/10/2012   AUTEUR PELLET J.PELLET 
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
C ======================================================================
C     EFGE_ELNO
C ----------------------------------------------------------------------
C
      INTEGER NC
      INTEGER CODRES(2)
      CHARACTER*2 NOMRES(2)
      REAL*8 ZERO

      REAL*8 PGL(3,3),FL(14),D1B3(2,3),KSI1,TMAX(2),TMIN(2),XIY,XIZ
      REAL*8 SIGFIB
      REAL*8 NX,TY,TZ,MX,MY,MZ,DEUX

      INTEGER NBFIB,KP,ADR,NCOMP,I,CARA,NE,JACF,NCARFI
      INTEGER ICOMPO,ICGP,ICONTN,IORIEN,IVECTU
      INTEGER JTAB(7),LX,INO,ISTRXR,ISTRXM,NBSP
C
      INTEGER IDEPLM,IDEPLP,IGEOM,IRET,ISECT,IMATE,K,IFGM
      REAL*8 UTG(14),XUG(6),XD(3),ANG1(3),DDOT,EY,EZ,TEMP
      REAL*8 XL,XL2,TET1,TET2,ALFA1,BETA1,GAMMA1,GAMMA,VALRES(2)
      REAL*8 XLS2,D1B(7,14),CO(3),AA,E,NU,G,ALFAY,ALFAZ,PHIY,PHIZ
      REAL*8 FORREF,MOMREF
      LOGICAL REACTU

      PARAMETER(ZERO=0.D+0,DEUX=2.0D+0)
C ----------------------------------------------------------------------

C     NOMBRE DE COMPOSANTES DES CHAMPS PSTRX? PAR POINTS DE GAUSS
      IF (NOMTE.EQ.'MECA_POU_D_EM') THEN
        NCOMP=15
      ELSEIF (NOMTE.EQ.'MECA_POU_D_TGM') THEN
        NCOMP=18
      ENDIF

      NC=7
C       --- RECUPERATION DES CARACTERISTIQUES DES FIBRES
      CALL JEVECH('PNBSP_I','L',I)
      NBFIB=ZI(I)
      CALL JEVECH('PFIBRES','L',JACF)
      NCARFI=3

C       --- NOMBRE DE POINT DE GAUSS

C       ON PROJETTE AVEC LES FCTS DE FORME
C       SUR LES NOEUDS DEBUT ET FIN DE L'ELEMENT
C       POUR LE POINT 1
      KSI1=-SQRT(5.D0/3.D0)
      D1B3(1,1)=KSI1*(KSI1-1.D0)/2.0D0
      D1B3(1,2)=1.D0-KSI1*KSI1
      D1B3(1,3)=KSI1*(KSI1+1.D0)/2.0D0
C       POUR LE POINT 2
      KSI1=SQRT(5.D0/3.D0)
      D1B3(2,1)=KSI1*(KSI1-1.D0)/2.0D0
      D1B3(2,2)=1.D0-KSI1*KSI1
      D1B3(2,3)=KSI1*(KSI1+1.D0)/2.0D0

C     --------------------------------------
      IF (NOMTE.EQ.'MECA_POU_D_TGM') THEN

        CALL JEVECH('PCONTRR','L',ICGP)
        CALL JEVECH('PSTRXRR','L',ISTRXR)
        CALL JEVECH('PEFFORR','E',ICONTN)


C --- CALCUL DES FORCES INTEGREES
        DO 20 I=1,NC
          FL(I)=ZERO
          FL(I+NC)=ZERO
          DO 10 KP=1,3
            ADR=ISTRXR+NCOMP*(KP-1)+I-1
            FL(I)=FL(I)+ZR(ADR)*D1B3(1,KP)
            FL(I+NC)=FL(I+NC)+ZR(ADR)*D1B3(2,KP)
   10     CONTINUE
   20   CONTINUE

C !!!   A CAUSE DE LA PLASTIFICATION DE LA SECTION LES EFFORTS
C          N,MFY,MFZ DOIVENT ETRE RECALCULES POUR LES NOEUDS 1 ET 2
        FL(1)=ZERO
        FL(5)=ZERO
        FL(6)=ZERO
        FL(1+NC)=ZERO
        FL(5+NC)=ZERO
        FL(6+NC)=ZERO

C       POUR LES NOEUDS 1 ET 2
C          CALCUL DES CONTRAINTES
C          CALCUL DES EFFORTS GENERALISES A PARTIR DES CONTRAINTES
        DO 50 NE=1,2
          DO 40 I=1,NBFIB
            SIGFIB=ZERO
            DO 30 KP=1,3
              ADR=ICGP+NBFIB*(KP-1)+I-1
              SIGFIB=SIGFIB+ZR(ADR)*D1B3(NE,KP)
   30       CONTINUE
            IF (I.EQ.1) THEN
              TMAX(NE)=SIGFIB
              TMIN(NE)=SIGFIB
            ELSE
              IF (SIGFIB.GT.TMAX(NE))TMAX(NE)=SIGFIB
              IF (SIGFIB.LT.TMIN(NE))TMIN(NE)=SIGFIB
            ENDIF
            ADR=NC*(NE-1)
            CARA=JACF+(I-1)*NCARFI
            FL(1+ADR)=FL(1+ADR)+SIGFIB*ZR(CARA+2)
            FL(5+ADR)=FL(5+ADR)+SIGFIB*ZR(CARA+2)*ZR(CARA+1)
            FL(6+ADR)=FL(6+ADR)-SIGFIB*ZR(CARA+2)*ZR(CARA)
   40     CONTINUE
   50   CONTINUE

        DO 60 I=1,NC
          ZR(ICONTN+I-1)=FL(I)
   60   CONTINUE
        ZR(ICONTN+(NC+1)-1)=TMAX(1)
        ZR(ICONTN+(NC+2)-1)=TMIN(1)
        DO 70 I=(NC+1),2*NC
          ZR(ICONTN+2+I-1)=FL(I)
   70   CONTINUE
        ZR(ICONTN+2*(NC+1)+1-1)=TMAX(2)
        ZR(ICONTN+2*(NC+1)+2-1)=TMIN(2)


      ELSEIF (NOMTE.EQ.'MECA_POU_D_EM') THEN
        CALL JEVECH('PCAORIE','L',IORIEN)
        NC=6
        CALL JEVECH('PGEOMER','L',IGEOM)

        CALL TECACH('OON','PCONTRR',7,JTAB,IRET)
        NBSP=JTAB(7)
        IF (NBSP.NE.NBFIB) CALL U2MESS('F','ELEMENTS_4')
        CALL JEVECH('PSTRXRR','L',ISTRXR)
C ---       LONGUEUR DE L'ELEMENT ---
        LX=IGEOM-1
        XL=SQRT((ZR(LX+4)-ZR(LX+1))**2+(ZR(LX+5)-ZR(LX+2))**2+
     &     (ZR(LX+6)-ZR(LX+3))**2)
        XL2=XL/DEUX

        NX=ZR(ISTRXR-1+1)
        TY=ZR(ISTRXR-1+2)
        TZ=ZR(ISTRXR-1+3)
        MX=ZR(ISTRXR-1+4)
        MY=ZR(ISTRXR-1+5)
        MZ=ZR(ISTRXR-1+6)

C ---       ET ENFIN LE VECTEUR NODAL

        FL(7)=NX
        FL(8)=TY
        FL(9)=TZ
        FL(10)=MX
        DO 80 I=1,4
          FL(I)=-FL(I+6)
   80   CONTINUE
        FL(5)=-MY+TZ*XL2
        FL(6)=-MZ-TY*XL2
        FL(11)=MY+TZ*XL2
        FL(12)=MZ-TY*XL2
C
        CALL JEVECH('PEFFORR','E',ICONTN)
        DO 90 I=1,12
          ZR(ICONTN+I-1)=FL(I)
   90   CONTINUE


      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      END
