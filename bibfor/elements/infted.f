      SUBROUTINE INFTED(NOMTE,SYMETR,NBTERM,NBNOEU,NBCOMP,NDIMEN,ITYPE)
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'

      CHARACTER*(*) NOMTE
      INTEGER       SYMETR,NBTERM,NBNOEU,NBCOMP,NDIMEN,ITYPE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FLEJOU J-L.FLEJOU
C --- ------------------------------------------------------------------
C                 INFORMATIONS SUR LES DISCRETS ET POUTRES
C IN
C     NOMTE : ELEMENTS CONCERNES :
C        MECA_DIS_TR_L     : MAILLE A 2 NOEUDS EN 3D
C        MECA_DIS_T_L      : MAILLE A 2 NOEUDS EN 3D
C        MECA_DIS_TR_N     : MAILLE A 1 NOEUD  EN 3D
C        MECA_DIS_T_N      : MAILLE A 1 NOEUD  EN 3D
C        MECA_2D_DIS_TR_L  : MAILLE A 2 NOEUDS EN 2D
C        MECA_2D_DIS_T_L   : MAILLE A 2 NOEUDS EN 2D
C        MECA_2D_DIS_TR_N  : MAILLE A 1 NOEUD  EN 2D
C        MECA_2D_DIS_T_N   : MAILLE A 1 NOEUD  EN 2D
C        MECA_POU_D_T      : MAILLE A 2 NOEUDS EN 3D
C        MECA_POU_D_E      : MAILLE A 2 NOEUDS EN 3D
C        MECA_POU_D_EM     : MAILLE A 2 NOEUDS EN 3D
C        MECA_POU_C_T      : MAILLE A 2 NOEUDS EN 3D
C        MECA_POU_D_TG     : MAILLE A 2 NOEUDS EN 3D
C        MECA_POU_D_TGM    : MAILLE A 2 NOEUDS EN 3D
C     SYMETR : =1 NON-SYMETRIQUE, =2 NON-SYMETRIQUE (DISCRETS SEULEMENT)
C
C OUT
C     NBTERM : NOMBRE DE TERME DANS LA MATRICE
C     NBNOEU : NOMBRE DE NOEUDS DE L'ELEMENT
C     NBCOMP : NOMBRE DE COMPOSANTE PAR NOEUD
C     NDIMEN : DIMENSION DE L'ELEMENT
C     ITYPE  : TYPE DE L'ELEMENT
C
C     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
C
      CHARACTER*20 KMESS(5)
      INTEGER      IADZI,IAZK24,LSECT,IRET
C --- ------------------------------------------------------------------
C
C     ITYPE : DANS PTENPO
C        POUTRE DROITE DE SECTION CONSTANTE OU VARIABLE  : 0 1 2
C        POUTRE COURBE DE SECTION CONSTANTE              : 10
C
C                                                  T  TR
C        DISCRET TYPE NODALE  ..._N              : 20 21
C        DISCRET TYPE NODALE  ..._N_NS           : 22 23
C        DISCRET TYPE LIAISON ..._L              : 40 41
C        DISCRET TYPE LIAISON ..._L_NS           : 42 43
C
      ITYPE = -10
      IF (NOMTE.EQ.'MECA_DIS_TR_L') THEN
         CALL ASSERT( (SYMETR.EQ.1).OR.(SYMETR.EQ.2) )
         IF (SYMETR.EQ.1) THEN
            NBTERM = 78
            NBNOEU = 2
            NBCOMP = 6
            NDIMEN = 3
            ITYPE  = 41
         ELSEIF (SYMETR.EQ.2) THEN
            NBTERM = 144
            NBNOEU = 2
            NBCOMP = 6
            NDIMEN = 3
            ITYPE  = 43
         ENDIF
      ELSE IF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
         CALL ASSERT( (SYMETR.EQ.1).OR.(SYMETR.EQ.2) )
         IF (SYMETR.EQ.1) THEN
            NBTERM = 21
            NBNOEU = 1
            NBCOMP = 6
            NDIMEN = 3
            ITYPE  = 21
         ELSEIF (SYMETR.EQ.2) THEN
            NBTERM = 36
            NBNOEU = 1
            NBCOMP = 6
            NDIMEN = 3
            ITYPE  = 23
         ENDIF
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_L') THEN
         CALL ASSERT( (SYMETR.EQ.1).OR.(SYMETR.EQ.2) )
         IF (SYMETR.EQ.1) THEN
            NBTERM = 21
            NBNOEU = 2
            NBCOMP = 3
            NDIMEN = 3
            ITYPE  = 40
         ELSEIF (SYMETR.EQ.2) THEN
            NBTERM = 36
            NBNOEU = 2
            NBCOMP = 3
            NDIMEN = 3
            ITYPE  = 42
         ENDIF
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_N') THEN
         CALL ASSERT( (SYMETR.EQ.1).OR.(SYMETR.EQ.2) )
         IF (SYMETR.EQ.1) THEN
            NBTERM = 6
            NBNOEU = 1
            NBCOMP = 3
            NDIMEN = 3
            ITYPE  = 20
         ELSEIF (SYMETR.EQ.2) THEN
            NBTERM = 9
            NBNOEU = 1
            NBCOMP = 3
            NDIMEN = 3
            ITYPE  = 22
         ENDIF
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_L') THEN
         CALL ASSERT( (SYMETR.EQ.1).OR.(SYMETR.EQ.2) )
         IF (SYMETR.EQ.1) THEN
            NBTERM = 21
            NBNOEU = 2
            NBCOMP = 3
            NDIMEN = 2
            ITYPE  = 41
         ELSEIF (SYMETR.EQ.2) THEN
            NBTERM = 36
            NBNOEU = 2
            NBCOMP = 3
            NDIMEN = 2
            ITYPE  = 43
         ENDIF
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_TR_N') THEN
         CALL ASSERT( (SYMETR.EQ.1).OR.(SYMETR.EQ.2) )
         IF (SYMETR.EQ.1) THEN
            NBTERM = 6
            NBNOEU = 1
            NBCOMP = 3
            NDIMEN = 2
            ITYPE  = 21
         ELSEIF (SYMETR.EQ.2) THEN
            NBTERM = 9
            NBNOEU = 1
            NBCOMP = 3
            NDIMEN = 2
            ITYPE  = 23
         ENDIF
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_T_L') THEN
         CALL ASSERT( (SYMETR.EQ.1).OR.(SYMETR.EQ.2) )
         IF (SYMETR.EQ.1) THEN
            NBTERM = 10
            NBNOEU = 2
            NBCOMP = 2
            NDIMEN = 2
            ITYPE  = 40
         ELSEIF (SYMETR.EQ.2) THEN
            NBTERM = 16
            NBNOEU = 2
            NBCOMP = 2
            NDIMEN = 2
            ITYPE  = 42
         ENDIF
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_T_N') THEN
         CALL ASSERT( (SYMETR.EQ.1).OR.(SYMETR.EQ.2) )
         IF (SYMETR.EQ.1) THEN
            NBTERM = 3
            NBNOEU = 1
            NBCOMP = 2
            NDIMEN = 2
            ITYPE  = 20
         ELSEIF (SYMETR.EQ.2) THEN
            NBTERM = 4
            NBNOEU = 1
            NBCOMP = 2
            NDIMEN = 2
            ITYPE  = 22
        ENDIF
C
C     LES POUTRES
      ELSEIF (NOMTE.EQ.'MECA_POU_D_T') THEN
         NBTERM = 78
         NBNOEU = 2
         NBCOMP = 6
         NDIMEN = 3
         CALL TECACH('NNN','PCAGNPO','L',1,LSECT,IRET)
         IF ( IRET.EQ.0 ) ITYPE =  NINT(ZR(LSECT-1+23))
      ELSE IF (NOMTE.EQ.'MECA_POU_D_E') THEN
         NBTERM = 78
         NBNOEU = 2
         NBCOMP = 6
         NDIMEN = 3
         CALL TECACH('NNN','PCAGNPO','L',1,LSECT,IRET)
         IF ( IRET.EQ.0 ) ITYPE =  NINT(ZR(LSECT-1+23))
      ELSE IF (NOMTE.EQ.'MECA_POU_D_EM') THEN
         NBTERM = 78
         NBNOEU = 2
         NBCOMP = 6
         NDIMEN = 3
         CALL TECACH('NNN','PCAGNPO','L',1,LSECT,IRET)
         IF ( IRET.EQ.0 ) ITYPE =  NINT(ZR(LSECT-1+23))
      ELSE IF (NOMTE.EQ.'MECA_POU_C_T') THEN
         NBTERM = 78
         NBNOEU = 2
         NBCOMP = 6
         NDIMEN = 3
         CALL TECACH('NNN','PCAGNPO','L',1,LSECT,IRET)
         IF ( IRET.EQ.0 ) ITYPE =  NINT(ZR(LSECT-1+23))
      ELSE IF ( (NOMTE.EQ.'MECA_POU_D_TG') .OR.
     &          (NOMTE.EQ.'MECA_POU_D_TGM') ) THEN
         NBTERM = 105
         NBNOEU = 2
         NBCOMP = 7
         NDIMEN = 3
         CALL TECACH('NNN','PCAGNPO','L',1,LSECT,IRET)
         IF ( IRET.EQ.0 ) ITYPE =  NINT(ZR(LSECT-1+23))
C
C     L'ELEMENT N'EST PAS TRAITE
      ELSE
         KMESS(1) = NOMTE
         KMESS(2) = 'INFTED'
         CALL TECAEL ( IADZI, IAZK24 )
         KMESS(3) = ZK24(IAZK24-1+3)
         CALL U2MESK('F','DISCRETS_13',3,KMESS)
      ENDIF

      END
