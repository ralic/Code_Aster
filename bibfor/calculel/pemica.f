      SUBROUTINE PEMICA(CHAMP,LONG,VR,NBMAIL,NUMMAI,ORIG,IORIG,ICAGE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CHAMP
      INTEGER                 LONG,   NBMAIL,NUMMAI(*),  IORIG
      REAL*8                       VR(*),           ORIG(3)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C     FAIRE DES OPERATIONS SUR UN CHAM_ELEM (OU D'UN RESUELEM)
C            (NOTION D'INTEGRALE DU CHAMP SUR LE MODELE)
C     ------------------------------------------------------------------
C IN  : CHAMP  : NOM DU CHAM_ELEM
C IN  : LONG   : LONGUEUR DU VECTEUR VR
C OUT : VR     : VECTEUR CONTENANT LES RESULATTS GLOBAUX
C IN  : NBMAIL : = 0  , CALCUL SUR TOUT LE CHAM_ELEM
C                SINON, CALCUL SUR UN NOMBRE DE MAILLES
C IN  : NUMMAI : NUMEROS DES MAILLES
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     ------------------------------------------------------------------
      INTEGER      NBGREL, NBELEM, DIGDEL, LONGT, LONG2, MODE, NPERM
      INTEGER      ITYPE, IORDRE
      REAL*8       MASSE, IXX, IYY, IZZ, IXY, IXZ, IYZ, ANGL(3)
      CHARACTER*8  SCAL, SCALAI, K8B
      CHARACTER*4  DOCU
      CHARACTER*19 CHAMP2, LIGREL
      LOGICAL      FIRST
      REAL*8       AR(6),BR(6),VECPRO(3,3),VALPRO(3), TOL, TOLDYN
      REAL*8       V1(3), V2(3), V3(3), JACAUX(3),IXPR2,IYPR2
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CHAMP2  = CHAMP
      RDDG    = R8RDDG()
      EPSI    = 1.D-12
C     --- ON RETROUVE LE NOM DU LIGREL ---

C     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
      CALL CELVER(CHAMP2,'NBVARI_CST','STOP',IBID)
      CALL CELVER(CHAMP2,'NBSPT_1','STOP',IBID)

      CALL JELIRA (CHAMP2//'.CELD','DOCU',IBID,DOCU)
      IF( DOCU.NE.'CHML')THEN
         CALL UTMESS('F','PEMICA','LE CHAMP DOIT ETRE UN CHAM_ELEM.')
      ENDIF
      CALL JEVEUO (CHAMP2//'.CELK','L',LCELK)
      LIGREL = ZK24(LCELK)(1:19)
C
      CALL JEVEUO (CHAMP2//'.CELD','L',JCELD)
C
C     --- TYPE DE LA GRANDEUR ---
      SCAL = SCALAI(ZI(JCELD))
C
C     -- ON VERIFIE LES LONGUEURS:
      FIRST = .TRUE.
      NBGR  =  NBGREL(LIGREL)
      DO 10, J = 1,NBGR
         MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
         IF (MODE .NE.0 ) THEN
C           --- NOMBRE D'ELEMENTS DANS LE MODE LOCAL ---
            LONG2 = DIGDEL(MODE)
            ICOEF=MAX(1,ZI(JCELD-1+4))
            LONG2=LONG2*ICOEF
            IF (FIRST) THEN
               LONGT=LONG2
            ELSEIF (LONGT.NE.LONG2) THEN
               CALL UTMESS('F','PEMICA','LONGUEURS DES MODES LOCAUX '
     +                                //'INCOMPATIBLES ENTRE EUX.')
            ENDIF
            FIRST = .FALSE.
         ENDIF
 10   CONTINUE
      IF (LONGT.GT.LONG) THEN
         CALL UTMESS ('F','PEMICA','LA LONGUEUR:LONG EST TROP PETITE.')
      ENDIF
C
C     -- ON MET A ZERO LE VECTEUR "VSCAL":
      IF (SCAL(1:1).EQ.'R') THEN
         DO 20, I = 1,LONGT
            VR(I) = 0.D0
 20      CONTINUE
      ELSE
         CALL UTMESS('F','PEMICA','TYPE SCALAIRE INTERDIT :'//SCAL)
      ENDIF
C
      CALL JEVEUO (CHAMP2//'.CELV','L',LVALE)
      IF ( NBMAIL.LE.0 ) THEN
         DO 102, J = 1,NBGR
            MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
            IF (MODE .EQ.0 ) GOTO 102
            NEL  = NBELEM(LIGREL,J)
            IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
            DO 104, K = 1,NEL
C
C              -- MASSE DE LA STRUCTURE ----
               I = 1
               MASSE = ZR(LVALE-1+IDECGR+(K-1)*LONGT+I-1)
               VR(1) = VR(1)+ MASSE
C
C              -- CENTRE DE GRAVITE DE LA STRUCTURE ----
               DO 106, I = 2,4
                  VR(I)=VR(I)+ZR(LVALE-1+IDECGR+(K-1)*LONGT+I-1)*MASSE
 106           CONTINUE
 104        CONTINUE
 102     CONTINUE
      ELSE
         CALL JEVEUO(LIGREL//'.LIEL','L',JLIGR)
         DO 110, IM = 1,NBMAIL
            DO 112, J = 1,NBGR
               MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
               IF (MODE .EQ.0 ) GOTO 112
               CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',J),'L',JGR)
               NEL = NBELEM(LIGREL,J)
               IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
               DO 114, K = 1,NEL
                  IEL = ZI(JGR+K-1)
                  IF (IEL.NE.NUMMAI(IM)) GOTO 114
C
C                 -- MASSE DE LA STRUCTURE ----
                  I = 1
                  MASSE = ZR(LVALE-1+IDECGR+(K-1)*LONGT+I-1)
                  VR(1) = VR(1)+ MASSE
C
C                 -- CENTRE DE GRAVITE DE LA STRUCTURE ----
                  DO 116, I = 2,4
                   VR(I)=VR(I)+ZR(LVALE-1+IDECGR+(K-1)*LONGT+I-1)*MASSE
 116              CONTINUE
                  GOTO 110
 114            CONTINUE
 112        CONTINUE
 110     CONTINUE
      ENDIF
C
C     --- CENTRE DE GRAVITE ---
      IF ( ABS(VR(1)) .GT. 1.D-6 ) THEN
         VR(2) = VR(2) / VR(1)
         VR(3) = VR(3) / VR(1)
         VR(4) = VR(4) / VR(1)
      ENDIF
C
      IF (IORIG.EQ.1) THEN
C
C       --- NOEUD P CHOISI PAR L'UTILISATEUR POUR CALCULER LE ---
C       --- TENSEUR D'INERTIE                                 ---
        VR(17) = ORIG(1)
        VR(18) = ORIG(2)
        VR(19) = ORIG(3)
C
C       --- VECTEUR PG ---
        PGX = VR(2) - ORIG(1)
        PGY = VR(3) - ORIG(2)
        PGZ = VR(4) - ORIG(3)
      ENDIF
C
      IF ( NBMAIL.LE.0 ) THEN
         DO 202, J = 1,NBGR
            MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
            IF (MODE .EQ.0 ) GOTO 202
            NEL  = NBELEM(LIGREL,J)
            IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
            DO 204, K = 1,NEL
C
               MASSE = ZR(LVALE-1+IDECGR+(K-1)*LONGT)
C
               DX = ZR(LVALE-1+IDECGR+(K-1)*LONGT+1) - VR(2)
               DY = ZR(LVALE-1+IDECGR+(K-1)*LONGT+2) - VR(3)
               DZ = ZR(LVALE-1+IDECGR+(K-1)*LONGT+3) - VR(4)
C
C              --- INERTIES DE LA STRUCTURE ---
               IXX = ZR(LVALE-1+IDECGR+(K-1)*LONGT+4)
               IYY = ZR(LVALE-1+IDECGR+(K-1)*LONGT+5)
               IZZ = ZR(LVALE-1+IDECGR+(K-1)*LONGT+6)
               IXY = ZR(LVALE-1+IDECGR+(K-1)*LONGT+7)
               IXZ = ZR(LVALE-1+IDECGR+(K-1)*LONGT+8)
               IYZ = ZR(LVALE-1+IDECGR+(K-1)*LONGT+9)
               VR(5)  = VR(5)  + IXX + MASSE*(DY*DY + DZ*DZ)
               VR(6)  = VR(6)  + IYY + MASSE*(DX*DX + DZ*DZ)
               VR(7)  = VR(7)  + IZZ + MASSE*(DX*DX + DY*DY)
               VR(8)  = VR(8)  + IXY + MASSE*DX*DY
               VR(9)  = VR(9)  + IXZ + MASSE*DX*DZ
               VR(10) = VR(10) + IYZ + MASSE*DY*DZ
               IF (ICAGE.NE.0) THEN
                  IXPR2  = ZR(LVALE-1+IDECGR+(K-1)*LONGT+10)
                  IYPR2  = ZR(LVALE-1+IDECGR+(K-1)*LONGT+11)
                  VR(26) = VR(26) + IXPR2 + DY*(3.0D0*IXX+IYY)
     &                  + MASSE*DY*(DX*DX+DY*DY) + 2.0D0*DX*IXY
                  VR(27) = VR(27) + IYPR2 + DX*(3.0D0*IYY+IXX)
     &                  + MASSE*DX*(DX*DX+DY*DY) + 2.0D0*DY*IXY
               ENDIF
C
 204        CONTINUE
 202     CONTINUE
      ELSE
         CALL JEVEUO(LIGREL//'.LIEL','L',JLIGR)
         DO 210, IM = 1,NBMAIL
            DO 212, J = 1,NBGR
               MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
               IF (MODE .EQ.0 ) GOTO 212
               CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',J),'L',JGR)
               NEL = NBELEM(LIGREL,J)
               IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
               DO 214, K = 1,NEL
                  IEL = ZI(JGR+K-1)
                  IF (IEL.NE.NUMMAI(IM)) GOTO 214
C
                  MASSE = ZR(LVALE-1+IDECGR+(K-1)*LONGT)
C
                  DX = ZR(LVALE-1+IDECGR+(K-1)*LONGT+1) - VR(2)
                  DY = ZR(LVALE-1+IDECGR+(K-1)*LONGT+2) - VR(3)
                  DZ = ZR(LVALE-1+IDECGR+(K-1)*LONGT+3) - VR(4)
C
C                 --- INERTIES DE LA STRUCTURE ---
                  IXX = ZR(LVALE-1+IDECGR+(K-1)*LONGT+4)
                  IYY = ZR(LVALE-1+IDECGR+(K-1)*LONGT+5)
                  IZZ = ZR(LVALE-1+IDECGR+(K-1)*LONGT+6)
                  IXY = ZR(LVALE-1+IDECGR+(K-1)*LONGT+7)
                  IXZ = ZR(LVALE-1+IDECGR+(K-1)*LONGT+8)
                  IYZ = ZR(LVALE-1+IDECGR+(K-1)*LONGT+9)
                  VR(5)  = VR(5)  + IXX + MASSE*(DY*DY + DZ*DZ)
                  VR(6)  = VR(6)  + IYY + MASSE*(DX*DX + DZ*DZ)
                  VR(7)  = VR(7)  + IZZ + MASSE*(DX*DX + DY*DY)
                  VR(8)  = VR(8)  + IXY + MASSE*DX*DY
                  VR(9)  = VR(9)  + IXZ + MASSE*DX*DZ
                  VR(10) = VR(10) + IYZ + MASSE*DY*DZ
                  IF (ICAGE.NE.0) THEN
                     IXPR2  = ZR(LVALE-1+IDECGR+(K-1)*LONGT+10)
                     IYPR2  = ZR(LVALE-1+IDECGR+(K-1)*LONGT+11)
                     VR(26) = VR(26) + IXPR2 + DY*(3.0D0*IXX+IYY)
     &                    + MASSE*DY*(DX*DX+DY*DY) + 2.0D0*DX*IXY
                     VR(27) = VR(27) + IYPR2 + DX*(3.0D0*IYY+IXX)
     &                    + MASSE*DX*(DX*DX+DY*DY) + 2.0D0*DY*IXY
                  ENDIF
                  GOTO 210
 214           CONTINUE
 212        CONTINUE
 210     CONTINUE
      ENDIF
C
      IF (IORIG.EQ.1) THEN
C
C     --- INERTIES DE LA STRUCTURE AU NOEUD UTILISATEUR P  ---
        VR(20)  = VR(5)  + VR(1)*(PGY*PGY + PGZ*PGZ)
        VR(21)  = VR(6)  + VR(1)*(PGX*PGX + PGZ*PGZ)
        VR(22)  = VR(7)  + VR(1)*(PGX*PGX + PGY*PGY)
        VR(23)  = VR(8)  + VR(1)*PGX*PGY
        VR(24)  = VR(9)  + VR(1)*PGX*PGZ
        VR(25)  = VR(10) + VR(1)*PGY*PGZ
      ENDIF
C
      NBVEC = 3
      IF ( ABS(VR(5)).LT.EPSI .AND. ABS(VR(6)).LT.EPSI .AND.
     +     ABS(VR(7)).LT.EPSI .AND. ABS(VR(8)).LT.EPSI .AND.
     +     ABS(VR(9)).LT.EPSI .AND. ABS(VR(10)).LT.EPSI ) THEN
         VR(11) = 0.D0
         VR(12) = 0.D0
         VR(13) = 0.D0
         VR(14) = 0.D0
         VR(15) = 0.D0
         VR(16) = 0.D0
         IF (ICAGE.NE.0) THEN
            VR(26) = 0.D0
            VR(27) = 0.D0
            VR(28) = 0.D0
            VR(29) = 0.D0
         ENDIF
      ELSE
C        LORS DE LA CONSTRUCTION DE LA MATRICE D'INERTIE,
C        ON RAJOUTE DES MOINS SUR LES TERMES EXTRA_DIAGONAUX
         AR(1) = VR(5)
         AR(2) = - VR(8)
         AR(3) = - VR(9)
         AR(4) = VR(6)
         AR(5) = - VR(10)
         AR(6) = VR(7)
         BR(1) = 1.D0
         BR(2) = 0.D0
         BR(3) = 0.D0
         BR(4) = 1.D0
         BR(5) = 0.D0
         BR(6) = 1.D0
         NPERM = 12
         TOL = 1.D-10
         TOLDYN = 1.D-2
         ITYPE = 0
         IORDRE = 0
         CALL JACOBI(NBVEC,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,JACAUX,
     &               NITJAC,ITYPE,IORDRE)
         V1(1) = 0.D0
         V1(2) = 0.D0
         V1(3) = 0.D0
         V2(1) = VECPRO(1,1)
         V2(2) = VECPRO(2,1)
         V2(3) = VECPRO(3,1)
         V3(1) = VECPRO(1,2)
         V3(2) = VECPRO(2,2)
         V3(3) = VECPRO(3,2)
         CALL ORIEN2 ( V1, V2, V3, ANGL )
         VR(11) = VALPRO(1)
         VR(12) = VALPRO(2)
         VR(13) = VALPRO(3)
         VR(14) = ANGL(1) * RDDG
         VR(15) = ANGL(2) * RDDG
         VR(16) = ANGL(3) * RDDG
         IF (ICAGE.NE.0) THEN
            VR(28) = -SIN(ANGL(1))*VR(27) + COS(ANGL(1))*VR(26)
            VR(29) =  COS(ANGL(1))*VR(27) + SIN(ANGL(1))*VR(26)
         ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
