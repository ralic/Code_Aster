      SUBROUTINE MATLOC ( NOMA, NCNCIN, MOTFAC, IOC, INO, NBMA,
     &                    LISTMA, PGL )
      IMPLICIT NONE
      INTEGER             IOC, INO, NBMA, LISTMA(*)
      REAL*8              PGL(3,3)
      CHARACTER*8         NOMA
      CHARACTER*16        MOTFAC
      CHARACTER*24        NCNCIN
C ---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C IN  : MOTFAC : MOT CLE FACTEUR
C IN  : IOC    : NOMERO D'OCCURRENCE
C IN  : INO    : NOMERO DU NOEUD TRAITE
C OUT : PGL    : MATRICE DE PASSAGE GLOBAL VERS LOCAL
C ---------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM, JEXATR
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C
      INTEGER       N2, N3, I, J, NBM, ADRM, IATYMA, NUMA,
     &              ADRVLC, INO1, INO2, ACOORD
      REAL*8        VX(3), VY(3), VZ(3), VECTY(3), VXN, VYN, VYP, DGRD,
     &              R8DGRD, ANGL(3), ALPHA, BETA, GAMMA
      CHARACTER*8   K8B, TYPM
      CHARACTER*24  CONNEX, TYPMAI, COORDO
C ----------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      DGRD = R8DGRD()
C
      CALL JELIRA ( JEXNUM(NCNCIN,INO), 'LONMAX', NBM, K8B)
      CALL JEVEUO ( JEXNUM(NCNCIN,INO), 'L', ADRM )
C
      CONNEX = NOMA//'.CONNEX         '
      TYPMAI = NOMA//'.TYPMAIL        '
      COORDO = NOMA//'.COORDO    .VALE'
      CALL JEVEUO ( TYPMAI, 'L', IATYMA )
      CALL JEVEUO ( COORDO, 'L', ACOORD )
C
C --- SI 1 MAILLE SUR LE NOEUD, OK
C
      IF ( NBM .EQ. 1 ) THEN
         NUMA = ZI(ADRM)
C
C --- SI PLUSIEURS MAILLES SUR LE NOEUD, RECHERCHE DE LA MAILLE
C
      ELSE
         IF ( NBMA .EQ. 0 ) THEN
             CALL U2MESS('F','MODELISA5_32')
         ENDIF
         IF ( NBMA .NE. 1 ) THEN
             CALL U2MESS('F','MODELISA5_33')
         ENDIF
         DO 20 I = 1 , NBMA
            NUMA = LISTMA(I)
            DO 22 J = 1 , NBM
               IF ( ZI(ADRM+J-1) .EQ. NUMA ) GOTO 24
 22         CONTINUE
 20      CONTINUE
         CALL U2MESS('F','MODELISA5_34')
 24      CONTINUE
      ENDIF
C
      CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(IATYMA+NUMA-1)),TYPM)
      IF ( TYPM(1:3) .NE. 'SEG' ) THEN
         CALL U2MESS('F','MODELISA5_35')
      ENDIF
      CALL JEVEUO ( JEXNUM(CONNEX,NUMA), 'L', ADRVLC )
      IF ( INO .EQ. ZI(ADRVLC) ) THEN
         INO1 = ZI(ADRVLC+1)
         INO2 = ZI(ADRVLC)
      ELSE
         INO1 = ZI(ADRVLC)
         INO2 = ZI(ADRVLC+1)
      ENDIF
C
C --- SI VECT_Y
C
      CALL GETVR8 ( MOTFAC, 'VECT_Y', IOC,1,3, VECTY, N2 )
      IF ( N2 .NE. 0 ) THEN
C        -- VECTEUR COLINEAIRE A LA MAILLE
         VX(1) = ZR(ACOORD+3*(INO2-1)  ) - ZR(ACOORD+3*(INO1-1)  )
         VX(2) = ZR(ACOORD+3*(INO2-1)+1) - ZR(ACOORD+3*(INO1-1)+1)
         VX(3) = ZR(ACOORD+3*(INO2-1)+2) - ZR(ACOORD+3*(INO1-1)+2)
         VXN = SQRT( VX(1)**2 + VX(2)**2 + VX(3)**2 )
         VX(1) = VX(1) / VXN
         VX(2) = VX(2) / VXN
         VX(3) = VX(3) / VXN
C        -- VECTEUR VECT_Y FOURNI
         VY(1) = VECTY(1)
         VY(2) = VECTY(2)
         VY(3) = VECTY(3)
C        -- PROJECTION / NORMALISATION
         VYP = VX(1)*VY(1) + VX(2)*VY(2) + VX(3)*VY(3)
         VY(1) = VY(1) - VYP*VX(1)
         VY(2) = VY(2) - VYP*VX(2)
         VY(3) = VY(3) - VYP*VX(3)
         VYN = SQRT(VY(1)**2+VY(2)**2+VY(3)**2)
         VY(1) = VY(1) / VYN
         VY(2) = VY(2) / VYN
         VY(3) = VY(3) / VYN
C        -- VECTEUR TANGENT
         VZ(1) = VX(2)*VY(3) - VY(2)*VX(3)
         VZ(2) = VX(3)*VY(1) - VY(3)*VX(1)
         VZ(3) = VX(1)*VY(2) - VY(1)*VX(2)
         DO 30 I = 1,3
            PGL(1,I) = VX(I)
            PGL(2,I) = VY(I)
            PGL(3,I) = VZ(I)
 30      CONTINUE
         GOTO 9999
      ENDIF
C
C --- SI ANGL_VRIL
C
      CALL GETVR8 ( MOTFAC, 'ANGL_VRIL', IOC,1,1, GAMMA, N3 )
      IF ( N3 .NE. 0 ) THEN
C        -- VECTEUR COLINEAIRE A LA MAILLE
         VX(1) = ZR(ACOORD+3*(INO2-1)  ) - ZR(ACOORD+3*(INO1-1)  )
         VX(2) = ZR(ACOORD+3*(INO2-1)+1) - ZR(ACOORD+3*(INO1-1)+1)
         VX(3) = ZR(ACOORD+3*(INO2-1)+2) - ZR(ACOORD+3*(INO1-1)+2)
         VXN = SQRT( VX(1)**2 + VX(2)**2 + VX(3)**2 )
         VX(1) = VX(1) / VXN
         VX(2) = VX(2) / VXN
         VX(3) = VX(3) / VXN
         CALL ANGVX ( VX, ALPHA, BETA )
         ANGL(1) = ALPHA
         ANGL(2) = BETA
         ANGL(3) = GAMMA * DGRD
         CALL MATROT ( ANGL , PGL )
         GOTO 9999
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA ( )
C
      END
