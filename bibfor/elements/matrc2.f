      SUBROUTINE MATRC2(NBPAR,NOMPAR,VALPAR,KCIS,MATC,VECTT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C
      IMPLICIT NONE
C
      INTEGER     NBPAR
      REAL*8      VALPAR(*), KCIS, MATC(5,5),VECTT(3,3)
      CHARACTER*8 NOMPAR(*)
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80 (1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      REAL*8        VALRES(26)
      CHARACTER*2   CODRET(26)
      CHARACTER*8   NOMRES(26)
      CHARACTER*10  PHENOM
      REAL*8 YOUNG, NU,NULT,NUTL,ALPHA,R8DGRD,BETA,DX,DY,DZ,PS,R8PREM
      REAL*8 PASSAG(3,3),PAS2(2,2),NORM,DORTH(3,3),WORK(3,3),D(3,3)
      REAL*8 DCIS(2,2),C,S,D2(2,2),TPG1,T,TINF,TSUP,EL,ET,GLT,GTN,DELTA
      REAL*8 PJDX,PJDY,PJDZ
      INTEGER I,J,JMATE,NBV,ITEMP,IBID,IER,IADZI,IAZK24,JCOQU,K
C
      DO 19 I=1,5
      DO 20 J=1,5
         MATC(I,J)=0.D0
 20   CONTINUE
 19   CONTINUE
C
      CALL JEVECH ('PMATERC' , 'L' , JMATE)
C
      CALL RCCOMA(ZI(JMATE),'ELAS',PHENOM,CODRET)
C
      IF ( PHENOM .EQ. 'ELAS' )  THEN
         NBV=2
         NOMRES(1)='E'
         NOMRES(2)='NU'
      ELSEIF (PHENOM.EQ.'ELAS_ORTH') THEN
          NOMRES(1)='E_L'
          NOMRES(2)='E_T'
          NOMRES(3)='NU_LT'
          NOMRES(4)='G_LT'
          NOMRES(5)='G_TN'
          NBV = 5
      ELSE
         CALL UTMESS('F','MATRC2','COMPORTEMENT MATERIAU NON ADMIS')
      ENDIF
C
      IF ( PHENOM .EQ. 'ELAS' )  THEN
C
      CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,NBV,NOMRES,
     &                                            VALRES,CODRET, 'FM')
C
C     MATERIAU ISOTROPE
C
         YOUNG = VALRES(1)
         NU    = VALRES(2)
C
C     CONSTRUCTION DE LA MATRICE DE COMPORTEMENT MATC : (5,5)
C
      MATC(1,1) = YOUNG / (1.D0 - NU*NU)
      MATC(1,2) = MATC(1,1) * NU
      MATC(2,1) = MATC(1,2)
      MATC(2,2) = MATC(1,1)
      MATC(3,3) = YOUNG / 2.D0 / (1.D0 + NU)
      MATC(4,4) = MATC(3,3) * KCIS
      MATC(5,5) = MATC(4,4)
C
      ELSEIF (PHENOM.EQ.'ELAS_ORTH') THEN
C
C ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
C ----   ET DU TEMPS
C        -----------
          CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,NBV,
     &                                    NOMRES,VALRES,CODRET, 'FM')
C
          EL    = VALRES(1)
          ET    = VALRES(2)
          NULT  = VALRES(3)
          GLT   = VALRES(4)
          GTN   = VALRES(5)
          NUTL  = ET*NULT/EL
          DELTA = 1.D0-NULT*NUTL
C
          DORTH(1,1) = EL/DELTA
          DORTH(1,2) = NULT*ET/DELTA
          DORTH(2,2) = ET/DELTA
          DORTH(2,1) = DORTH(1,2)
          DORTH(3,3) = GLT
          DORTH(1,3) = 0.D0
          DORTH(2,3) = 0.D0
          DORTH(3,1) = 0.D0
          DORTH(3,2) = 0.D0

C --- RECUPERATION DES ANGLES DETERMINANT LE REPERE UTILISATEUR
C --- PAR RAPPORT AU REPERE GLOBAL :
C     ============================
      CALL JEVECH ('PCACOQU', 'L', JCOQU)
C
      ALPHA = ZR(JCOQU+1) * R8DGRD()
      BETA  = ZR(JCOQU+2) * R8DGRD()
C
      DX = COS(BETA)*COS(ALPHA)
      DY = COS(BETA)*SIN(ALPHA)
      DZ = SIN(BETA)
      NORM = SQRT (DX*DX + DY*DY + DZ*DZ)
      DX = DX/NORM
      DY = DY/NORM
      DZ = DZ/NORM

C ---   RECUPERATION DE LA MATRICE DE PASSAGE AU POINT D'INTEGRATION 
C ---   DETERMINATION DE LA PROJECTION DU VECTEUR X DU REPERE
C ---   UTILISATEUR SUR LE FEUILLET TANGENT A LA COQUE AU POINT
C ---   D'INTEGRATION COURANT :
C       ---------------------
        PS = DX*VECTT(3,1) + DY*VECTT(3,2) + DZ*VECTT(3,3)
        PJDX = DX - PS*VECTT(3,1)
        PJDY = DY - PS*VECTT(3,2)
        PJDZ = DZ - PS*VECTT(3,3)
        NORM = SQRT (PJDX*PJDX + PJDY*PJDY + PJDZ*PJDZ)
        IF ( NORM .LE. R8PREM() ) THEN
          CALL UTMESS('F','MATRC','L''AXE DE REFERENCE EST NORMAL A'
     +                //' UN ELEMENT DE PLAQUE - CALCUL OPTION'
     +                //' IMPOSSIBLE - ORIENTER CES MAILLES' )
        ENDIF
C
        PJDX = PJDX/NORM
        PJDY = PJDY/NORM
        PJDZ = PJDZ/NORM
C
        C = PJDX*VECTT(1,1) + PJDY*VECTT(1,2) + PJDZ*VECTT(1,3)
        S = PJDX*VECTT(2,1) + PJDY*VECTT(2,2) + PJDZ*VECTT(2,3)
C
C
C ----   TENSEUR D'ELASTICITE DANS LE REPERE INTRINSEQUE :
C ----   D_GLOB = PASSAG_T * D_ORTH * PASSAG

         DO 22 I = 1, 3
         DO 22 J = 1, 3
            PASSAG(I,J) = 0.D0
 22      CONTINUE
         PASSAG(1,1) = C*C
         PASSAG(2,2) = C*C
         PASSAG(1,2) = S*S
         PASSAG(2,1) = S*S
         PASSAG(1,3) = C*S
         PASSAG(3,1) = -2.D0*C*S
         PASSAG(2,3) = -C*S
         PASSAG(3,2) = 2.D0*C*S
         PASSAG(3,3) = C*C-S*S
              
         CALL UTBTAB('ZERO',3,3,DORTH,PASSAG,WORK,D)

         DO 24 I = 1, 3
         DO 24 J = 1, 3
            MATC(I,J) = D(I,J)
 24      CONTINUE
 
         DCIS(1,1) = GLT
         DCIS(1,2) = 0.D0
         DCIS(2,1) = 0.D0
         DCIS(2,2) = GTN
         PAS2(1,1) = C
         PAS2(2,2) = C
         PAS2(1,2) = S
         PAS2(2,1) = -S
         CALL UTBTAB('ZERO',2,2,DCIS,PAS2,WORK,D2)
         DO 23 I = 1, 2
         DO 23 J = 1, 2
            MATC(3+I,3+J) = D2(I,J)
 23      CONTINUE

      ENDIF
C
      END
