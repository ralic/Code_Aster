      SUBROUTINE PJEFTG (GEOM2,NOMA2,MOTFAC,IOCC)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 10/12/2007   AUTEUR REZETTE C.REZETTE 
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
C
C BUT : TRANSFORMER LA GEOMETRIE DES NOEUDS DU MAILLAGE_2 AVANT LA
C       LA PROJECTION (MOT CLE TRANSF_GEOM_2).
C       CELA PERMET PAR EXEMPLE DE PROJETER :
C       - UN MAILLLAGE SUR UN AUTRE MAILLAGE HOMOTHETIQUE
C       - UN MAILLAGE 2D SUR UN MAILLAGE 3D "ECRASE" DANS UN PLAN
C         (2D AXIS -> 3D AXIS)
C
C OUT : GEOM2 (K24) : NOM DE L'OBJET CONTENANT LA GEOMETRIE TRANSFORMEE
C                     DES NOEUDS DE MAILLAGE_2
C ----------------------------------------------------------------------
      IMPLICIT   NONE
C
C 0.1. ==> ARGUMENTS
C
      CHARACTER*8 NOMA2
      CHARACTER*24 GEOM2
      CHARACTER*(*) MOTFAC
      INTEGER IOCC
C
C 0.2. ==> COMMUNS
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      CHARACTER*32 JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C 0.3. ==> VARIABLES LOCALES
C

      CHARACTER*8 LFONC(3),LPARX(3)
      CHARACTER*8 KBID
      INTEGER N1,NBNO2,IFONC
      INTEGER NFONC,JGEOM2,INO2,IER

      REAL*8  VX(3)
C----------------------------------------------------------------------
C DEB ------------------------------------------------------------------
      CALL JEMARQ()


C     PRISE EN COMPTE DU MOT-CLE TRANSF_GEOM_2 : CALCUL DE GEOM2
C     -----------------------------------------------------------
      CALL GETVID(MOTFAC,'TRANSF_GEOM_2',IOCC,1,3,LFONC,NFONC)
      CALL ASSERT(NFONC.GE.0)
      IF (NFONC.GT.0) THEN
         CALL ASSERT(NFONC.EQ.2 .OR. NFONC.EQ.3)
         IF (NFONC.EQ.2) LFONC(3)='&FOZERO'
         GEOM2='&&PJEFTG.GEOM2'
         CALL JEDETR(GEOM2)
         CALL JEDUPO(NOMA2//'.COORDO    .VALE', 'V', GEOM2, .FALSE.)
         CALL JELIRA(GEOM2,'LONMAX',N1,KBID)
         CALL JEVEUO(GEOM2,'E',JGEOM2)
         NBNO2=N1/3
         CALL ASSERT(N1.EQ.NBNO2*3)
         LPARX(1)='X'
         LPARX(2)='Y'
         LPARX(3)='Z'
         DO 1, INO2=1,NBNO2
           DO 2, IFONC=1,3
             CALL FOINTE('F',LFONC(IFONC),3,LPARX,
     &           ZR(JGEOM2-1+3*(INO2-1)+1),VX(IFONC),IER)
             CALL ASSERT(IER.EQ.0)
2          CONTINUE
           ZR(JGEOM2-1+3*(INO2-1)+1)=VX(1)
           ZR(JGEOM2-1+3*(INO2-1)+2)=VX(2)
           ZR(JGEOM2-1+3*(INO2-1)+3)=VX(3)
1        CONTINUE
      ELSE
         GEOM2=' '
      ENDIF

      CALL JEDEMA()
      END
