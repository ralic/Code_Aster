      SUBROUTINE PASFRE(DISC,FREQ,PASF,DIM,NBM,IV,IMODI,FREQI,FREQF,NB)
      IMPLICIT REAL*8 (A-H,O-Z)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
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
C---------------------------------------------------------------------
C  CALCUL DE LA DISCRETISATION FREQUENTIELLE
C  -----------------------------------------
C  POUR CHAQUE FREQUENCE MODALE FREQ(I) PRISE EN COMPTE  
C  DANS LA BANDE DE FREQUENCES, UNE ZONE CALCULEE
C  EN FONCTION DE FREQ(I-1) ET FREQ(I+1) EST DISCRETISEE
C  EN 1024 POINTS (NB = 1024 , EN PARAMETER DANS OP0146)      
C  CHAQUE ZONE EST DIVISEE EN 4  
C  2 ZONES A PAS FIN ET 2 ZONES A PAS LACHE DETERMINEES PAR :
C   FREQ_DEBUT = (FREQ(I)+FREQ(I-1))/2
C   F2         = FREQ(I) - DF
C   F3         = FREQ(I)
C   F4         = FREQ(I) + DF
C   FREQ_FIN   = (FREQ(I)+FREQ(I+1))/2 
C       AVEC DF = 2*PI*FREQ(I)*AMOR(I)
C-----------------------------------------------------------------------
C     IN  : FREQ  : CARACT. MODALES DE LA BASE DE CONCEPT MELASFLU
C     OUT : PASF  : DISCRETISATION FREQUENTIELLE CALCULEE 
C     IN  : DIM   : NOMBRE DE MODES PRIS EN COMPTE DANS LA BANDE DE FREQ
C     IN  : NBM   : NOMBRE DE MODES DSE LA BASE DE CONCEPT MELASFLU
C     IN  : IV    : INDICATEUR DE LA VITESSE ETUDIEE
C     IN  : IMODI : NUMERO D ORDRE DU PREMIER MODE PRIS EN COMPTE
C     IN  : FREQI : FREQUENCE INITIALE DE LA BANDE DE FREQ.
C     IN  : FREQF : FREQUENCE FINALE   DE LA BANDE DE FREQ.
C     IN  : NB    : NOMBRE DE POINTS PAR MODE POUR LA DISCR. FREQ.
C     IN  : DISC  : TABLEAU DE TRAVAIL
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      DIM
      REAL*8       FREQ(2,NBM,*), PASF(DIM*NB),DISC(2,*)
C-----------------------------------------------------------------------
CC
      NB4 = NB/4
      PI = R8PI()
      NBPF = NB*DIM
      NBZ =  4   *DIM
C
      DISC(1,1)   = FREQI
      DISC(2,NBZ) = FREQF
C
      DO 10 IM=1,DIM
        NUMO = IMODI + (IM-1)
C
        IF (FREQ(2,NUMO,IV) .LT . 0.D0) THEN
          DF = 20.D0*FREQ(1,NUMO,IV) * 1.D-06
        ELSE
          DF = 2.D0*PI*FREQ(1,NUMO,IV)*FREQ(2,NUMO,IV)
        ENDIF
C
        IF (IM .GT. 1) THEN
          DISC(1,(IM-1)*4+1) =  DISC(2,(IM-1)*4)
        ENDIF
C
        IF (IM .LT. DIM) THEN
          DISC(2,(IM-1)*4+4) = (FREQ(1,NUMO,IV)+FREQ(1,NUMO+1,IV))/2.D0
        ENDIF
C
        DFF = FREQ(1,NUMO,IV)-DISC(1,(IM-1)*4+1)
        IF (DF .GE. DFF) THEN
          DISC(2,(IM-1)*4+1) = FREQ(1,NUMO,IV) - (DFF/2.D0)
          DISC(1,(IM-1)*4+2) = FREQ(1,NUMO,IV) - (DFF/2.D0)
        ELSE
          DISC(2,(IM-1)*4+1) = FREQ(1,NUMO,IV) - DF
          DISC(1,(IM-1)*4+2) = FREQ(1,NUMO,IV) - DF
        ENDIF
C
        DISC(2,(IM-1)*4+2) = FREQ(1,NUMO,IV)
        DISC(1,(IM-1)*4+3) = FREQ(1,NUMO,IV)
C
        DFF = DISC(2,(IM-1)*4+4) - FREQ(1,NUMO,IV)
        IF (DF .GE. DFF) THEN
          DISC(2,(IM-1)*4+3) = FREQ(1,NUMO,IV) + (DFF/2.D0) 
          DISC(1,(IM-1)*4+4) = FREQ(1,NUMO,IV) + (DFF/2.D0)
        ELSE
          DISC(2,(IM-1)*4+3) = FREQ(1,NUMO,IV) + DF 
          DISC(1,(IM-1)*4+4) = FREQ(1,NUMO,IV) + DF
        ENDIF
 10   CONTINUE
C
      IF = 1
      DO 20 IZ=1,NBZ
        IF (IZ .LT. NBZ) THEN
          FMIN = DISC(1,IZ)
          FMAX = DISC(2,IZ)
          PAS  = (FMAX - FMIN) / DBLE(NB4)
          PASF(IF) = FMIN
          DO 30 IP =1,NB4-1
            IF = IF+1
            PASF(IF) = FMIN + PAS*IP
 30       CONTINUE
          IF = IF+1
        ELSE
          FMIN = DISC(1,IZ)
          FMAX = DISC(2,IZ)
          PAS  = (FMAX - FMIN) / DBLE(NB4-1)
          PASF(IF) = FMIN 
          DO 40 IP =1,NB4-2
            IF = IF+1
            PASF(IF) = FMIN + PAS*IP
 40       CONTINUE
        ENDIF
 20   CONTINUE
      PASF(NBPF) = FMAX
      END
