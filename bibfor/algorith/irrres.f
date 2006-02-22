      SUBROUTINE IRRRES ( FAMI, KPG, KSP, MOD, NMAT, MATERD, MATERF,
     &                    YD ,  YF,   DEPS,   DY,  R )


      IMPLICIT NONE

      CHARACTER*8 MOD
      CHARACTER*(*) FAMI
      INTEGER     NMAT,KPG,KSP
      REAL*8      MATERD(NMAT,2) ,MATERF(NMAT,2)
      REAL*8      YD(*),YF(*),DEPS(6),DY(*),R(*)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/02/2006   AUTEUR CIBHHPD L.SALMONA 
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

      REAL*8      DFDS(6),ID3D(6),SF,A,B
      REAL*8      IRRAD,IRRAF,DPHI,SIGD(6),SIGF(6),DKOOH(6,6)
      REAL*8      FKOOH(6,6),HOOKF(6,6),K,N,P0,AI0,ETAIS,AG,P
      REAL*8      DP,DETAI,DPI,DG,ETAIF,S,EPSEF(6),EPSD(6),PE
      REAL*8      DEPSA(6),DEPSG(6),DEV(6),EPSED(6),LCNRTS
      REAL*8      RS(6),RP,RE,RPI,RG,QF,DSIG(6),R8PREM
      INTEGER     NDT, NDI,IRET
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT , NDI
C     ----------------------------------------------------------------
      
      DATA ID3D /1.D0, 1.D0, 1.D0, 0.D0, 0.D0, 0.D0/
      DATA PE        /2.D-3/

      CALL LCEQVN ( NDT , YF(1), SIGF)
      CALL LCEQVN ( NDT , YD(1), SIGD)
      CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERD(1,1) , DKOOH )
      CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERF(1,1) , FKOOH )
      CALL LCOPLI  ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOKF )
      
      P=YF(NDT+1)
      
C     AFFECTATION DES VALEURS PLASTIQUES
C     ECRETAGE DE LA LOI POUR LES FAIBLES VALEURS DE P
      IF (MATERF(8,2).EQ.0.D0) THEN
        MATERF(1,2)=MATERF(7,2)
        MATERF(2,2)=0.D0
        MATERF(3,2)=0.D0
      ELSE
        IF ((P.GE.PE).AND.((MATERF(7,2)*(P+MATERF(9,2))**MATERF(8,2))
     &      .GT.(MATERF(10,2)*MATERF(11,2)))) THEN
          MATERF(1,2)=MATERF(7,2)
          MATERF(2,2)=MATERF(8,2)
          MATERF(3,2)=MATERF(9,2)
        ELSE
          A=MATERF(8,2)*MATERF(7,2)*
     &      ((PE+MATERF(9,2))**(MATERF(8,2)-1.D0))
          B=(MATERF(7,2)/A)*(PE+MATERF(9,2))**MATERF(8,2)-PE

          IF ((A*(P+B)).LT.(MATERF(10,2)*MATERF(11,2))) THEN
            MATERF(1,2)=MATERF(10,2)*MATERF(11,2)
            MATERF(2,2)=0.D0
            MATERF(3,2)=0.D0
          ELSE
            MATERF(1,2)=A
            MATERF(2,2)=1.D0
            MATERF(3,2)=B
          ENDIF
        ENDIF
      ENDIF
      K=MATERF(1,2)
      N=MATERF(2,2)
      P0=MATERF(3,2)
      AI0=MATERF(4,2)
      ETAIS=MATERF(5,2)
      AG=MATERF(6,2)
      DP = DY(NDT+1)
      DETAI = DY(NDT+2)
      DPI = DY(NDT+3)
      DG = DY(NDT+4)
      ETAIF=YF(NDT+2)
      CALL RCVARC('F','IRRA','-',FAMI,KPG,KSP,IRRAD,IRET)
      CALL RCVARC('F','IRRA','+',FAMI,KPG,KSP,IRRAF,IRET)
      DPHI=IRRAF-IRRAD
      CALL LCDEVI(SIGF,DEV)
      S = LCNRTS (DEV)
      IF ( S.EQ.0.D0) THEN
        CALL LCINVE (0.D0,DFDS)
      ELSE
        CALL LCPRSV ( 1.5D0 / S , DEV , DFDS )
      ENDIF
      CALL LCPRMV(FKOOH,SIGF,EPSEF)
      CALL LCPRMV(DKOOH,SIGD,EPSED)
      
      CALL LCPRSV ( (DP+DPI), DFDS, DEPSA )
      
      CALL LCPRSV ( DG, ID3D, DEPSG )
     
C   RESIDU EN SIGMA
      CALL LCDIVE(EPSEF,EPSED,RS)
      CALL LCSOVE(RS,DEPSA,RS)
      CALL LCSOVE(RS,DEPSG,RS)
      CALL LCDIVE(RS,DEPS,RS)
      CALL LCPRSV(-1.D0,RS,RS)

C  RESIDU EN DEFORMATION PLASTIQUE
      IF ( ((P+P0).EQ.0.D0).AND.(N.EQ.0.D0)) THEN
        SF=K
      ELSE
        SF=K*(P+P0)**N
      ENDIF

      IF (((S.GE.SF).AND.(DP.GE.0.D0)).OR.(DP.GT.R8PREM())) THEN
        RP=-(S-SF)/HOOKF(1,1)
      ELSE
        RP=-DP
      ENDIF

C  RESIDU PAR RAPPORT A ETA
      RE=-(DETAI-S*DPHI)/HOOKF(1,1)

C  RESIDU EN DEFORMATION D IRRADIATION
      IF (ETAIF.GT.ETAIS) THEN
        RPI=-(DPI-AI0*S*DPHI)
      ELSE
        RPI=-DPI
      ENDIF

C  RESIDU PAR RAPPORT AU GONFLEMENT
      RG=-(DG-AG*DPHI)

C  RESIDU PAR RAPPORT A LA DEFORMATION ( C_PLAN )
      IF ( MOD(1:6) .EQ. 'C_PLAN' ) THEN
      QF = (- HOOKF(3,3) *  EPSEF(3)
     &     - HOOKF(3,1) *  EPSEF(1)
     &     - HOOKF(3,2) *  EPSEF(2)
     &     - HOOKF(3,4) *  EPSEF(4))/HOOKF(1,1)
      ENDIF
      
      CALL LCEQVN ( NDT , RS     , R(1) )
      R(NDT+1)=RP
      R(NDT+2)=RE
      R(NDT+3)=RPI
      R(NDT+4)=RG
      IF ( MOD(1:6).EQ.'C_PLAN' ) R(NDT+5) = QF
      END
