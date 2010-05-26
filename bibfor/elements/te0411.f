      SUBROUTINE TE0411(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/05/2010   AUTEUR LEBOUVIER F.LEBOUVIER 
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
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C    - TE DEDIE A L'OPTION : PROJ_ELEM_SIGM
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C    - OPTIONS CALCULEES : PROJ_ELEM_SIGN,PROJ_ELEM_SIGT,PROJ_ELEM_SIT1,
C                          PROJ_ELEM_SIT2.
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER ISIG,IARC,NNOP,NDIM,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO,I,IANG
      INTEGER IGEOM,J,IFONC,INO,IADZI,IAZK24,ISIGN,ISIGT,ISIT1,ISIT2
      REAL*8 PREC,XX1(3),ALPHA,BETA,PI,A(3),B(3),C(3)
      REAL*8 SIGG(3,3),MLG(3,3),MTMP(3,3),SIGL(3,3),R8DGRD,D(3),AD(3)
      REAL*8 AB(3),AC(3),NORME,X1(3),X2(3),X3(3),MGL(3,3),DET,PS
      REAL*8 DFF(162),VTAN1(3),VTAN2(3),NORX3,VT1(3),VT2(3),VNO(3)
      REAL*8 NORT1,NORT2,NORNO,SIXT1,SIXT2,SIYT1,SIYT2,SIYNO,SIXNO
      REAL*8 SIZT1,SIZT2,SIZNO,A1,A2,A3,B1,B2,B3,C1,C2,C3,L1,L2,DELTA
      REAL*8 V1(3),V2(3),VCN(3),VCNL(3),VTMP(3),VT(3),VTMP2(3)
      CHARACTER*8 ELREFE,KNUNOE,VALK(2)
      PARAMETER(PREC=1.0D-10)
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL TECAEL(IADZI,IAZK24)
      CALL ELREF1(ELREFE)
      CALL ELREF4(' ','RIGI',NDIM,NNOP,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      CALL JEVECH('PSIG3D' ,'L',ISIG)
      CALL JEVECH('PGEOMER','L',IGEOM)
      IF(OPTION(1:14).EQ.'PROJ_ELEM_SIGN')THEN
         CALL JEVECH('PPJSIGN','E',ISIGN)
      ELSE IF(OPTION(1:14).EQ.'PROJ_ELEM_SIGT')THEN
         CALL JEVECH('PPJSIGT','E',ISIGT)
      ELSE IF(OPTION(1:14).EQ.'PROJ_ELEM_SIT1')THEN
         CALL JEVECH('PPJSIT1','E',ISIT1)
      ELSE IF(OPTION(1:14).EQ.'PROJ_ELEM_SIT2')THEN
         CALL JEVECH('PPJSIT2','E',ISIT2)
      ELSE
         CALL U2MESK('F','ELEMENTS4_11',1,OPTION)
      ENDIF
C
C     CALCUL DES DERIVEES DES FONCTIONS DE FORMES AUX NOEUDS DE L'ELREFE
      CALL DFFNO ( ELREFE, NDIM, NNOP, NNOS, DFF )
C
C --- 1. CALCUL DU REPERE LOCAL : (VT1, VT2, VNO)
C     ===========================================
C     VT1, VT2 = VECTEURS TANGENTS A L'ELEMENT
C     VNO = VECTEUR NORMAL A L'ELEMENT

C    INITIALISATION
      DO 9 I=1,3
         VT1(I) = 0.D0
         VT2(I) = 0.D0
         VNO(I) = 0.D0
         DO 8 J=I,3
            SIGG(I,J)=0.D0
 8       CONTINUE
 9    CONTINUE

C     BOUCLE SUR LES NOEUDS DE L'ELEMENT
C     ----------------------------------
      DO 10 INO=1,NNOP

         DO 12 I=1,3
            VTAN1(I) = 0.D0
            VTAN2(I) = 0.D0
 12      CONTINUE

         DO 20 IFONC=1,NNOP
           VTAN1(1)=VTAN1(1)+
     &          ZR(IGEOM-1+3*(IFONC-1)+1)*DFF((INO-1)*NNOP*2+IFONC)
           VTAN1(2)=VTAN1(2)+
     &          ZR(IGEOM-1+3*(IFONC-1)+2)*DFF((INO-1)*NNOP*2+IFONC)
           VTAN1(3)=VTAN1(3)+
     &          ZR(IGEOM-1+3*(IFONC-1)+3)*DFF((INO-1)*NNOP*2+IFONC)
           VTAN2(1)=VTAN2(1)+
     &          ZR(IGEOM-1+3*(IFONC-1)+1)*DFF((INO-1)*NNOP*2+NNOP+IFONC)
           VTAN2(2)=VTAN2(2)+
     &          ZR(IGEOM-1+3*(IFONC-1)+2)*DFF((INO-1)*NNOP*2+NNOP+IFONC)
           VTAN2(3)=VTAN2(3)+
     &          ZR(IGEOM-1+3*(IFONC-1)+3)*DFF((INO-1)*NNOP*2+NNOP+IFONC)
 20      CONTINUE

         VT1(1)=VT1(1)+VTAN1(1)
         VT1(2)=VT1(2)+VTAN1(2)
         VT1(3)=VT1(3)+VTAN1(3)
         VT2(1)=VT2(1)+VTAN2(1)
         VT2(2)=VT2(2)+VTAN2(2)
         VT2(3)=VT2(3)+VTAN2(3)

         SIGG(1,1)=SIGG(1,1)+ZR(ISIG+6*(INO-1))
         SIGG(2,2)=SIGG(2,2)+ZR(ISIG+6*(INO-1)+1)
         SIGG(3,3)=SIGG(3,3)+ZR(ISIG+6*(INO-1)+2)
         SIGG(1,2)=SIGG(1,2)+ZR(ISIG+6*(INO-1)+3)
         SIGG(1,3)=SIGG(1,3)+ZR(ISIG+6*(INO-1)+4)
         SIGG(2,3)=SIGG(2,3)+ZR(ISIG+6*(INO-1)+5)

 10   CONTINUE

C     VECTEURS TANGENTS PAR MOYENNE DE CHAQUE COMPOSANTE
      VT1(1)=VT1(1)/NNOP
      VT1(2)=VT1(2)/NNOP
      VT1(3)=VT1(3)/NNOP
      VT2(1)=VT2(1)/NNOP
      VT2(2)=VT2(2)/NNOP
      VT2(3)=VT2(3)/NNOP

C     TENSEUR DES CONTRAINTES PAR MOYENNE DE CHAQUE COMPOSANTE
      SIGG(1,1)=SIGG(1,1)/NNOP
      SIGG(2,2)=SIGG(2,2)/NNOP
      SIGG(3,3)=SIGG(3,3)/NNOP
      SIGG(1,2)=SIGG(1,2)/NNOP
      SIGG(1,3)=SIGG(1,3)/NNOP
      SIGG(2,3)=SIGG(2,3)/NNOP
      SIGG(2,1)=  SIGG(1,2)
      SIGG(3,1)=  SIGG(1,3)
      SIGG(3,2)=  SIGG(2,3)

      CALL NORMEV(VT1,NORT1)
      CALL NORMEV(VT2,NORT2)
      CALL PROVEC(VT1,VT2,VNO)
      CALL NORMEV(VNO,NORNO)
      CALL PROVEC(VNO,VT1,VT2)
C
C --- 2. EXPRESSION DES VECTEURS CONTRAINTES DANS LE REPERE LOCAL :
C       ==========================================================
C       (VT1,VT2,VNO) 
C       SIX_L = (SIXT1,SIXT2,SIXNO) DANS (VT1,VT2,VNO)
C       SIY_L = (SIYT1,SIYT2,SIYNO) DANS (VT1,VT2,VNO)
C       SIZ_L = (SIZT1,SIZT2,SIZNO) DANS (VT1,VT2,VNO)

      DET=VT1(1)*VT2(2)*VNO(3)+VT2(1)*VNO(2)*VT1(3)+VNO(1)*VT1(2)*VT2(3)
     &   -VNO(1)*VT2(2)*VT1(3)-VT1(1)*VNO(2)*VT2(3)-VT2(1)*VT1(2)*VNO(3)
      CALL ASSERT(ABS(DET).GT.PREC)

      MGL(1,1) = VT1(1) 
      MGL(2,1) = VT2(1)
      MGL(3,1) = VNO(1)
      MGL(1,2) = VT1(2)  
      MGL(2,2) = VT2(2)
      MGL(3,2) = VNO(2)
      MGL(1,3) = VT1(3) 
      MGL(2,3) = VT2(3)
      MGL(3,3) = VNO(3)   
           
      MLG(1,1) = VT1(1) 
      MLG(2,1) = VT1(2)
      MLG(3,1) = VT1(3)
      MLG(1,2) = VT2(1)  
      MLG(2,2) = VT2(2)
      MLG(3,2) = VT2(3)
      MLG(1,3) = VNO(1) 
      MLG(2,3) = VNO(2)
      MLG(3,3) = VNO(3)   
      
      CALL PMAT(3,SIGG,MLG,MTMP)
      CALL PMAT(3,MGL,MTMP,SIGL)

C
C --- 3. CALCUL DES OPTIONS : PROJ_ELEM_SIT1 & PROJ_ELEM_SIT2
C     =======================================================
C     DETERMINATION DES 2 MODES PROPRES TELS QUE SIXT2=SIYT1=0
      IF(OPTION(1:13).EQ.'PROJ_ELEM_SIT')THEN
         DELTA=(SIGL(1,1)+SIGL(2,2))**2 - 
     &          4.D0*(SIGL(1,1)*SIGL(2,2)-SIGL(1,2)*SIGL(2,1))
         CALL ASSERT(DELTA.GT.0.D0)
         L1=(SIGL(1,1)+SIGL(2,2)-SQRT(DELTA))/2
         L2=(SIGL(1,1)+SIGL(2,2)+SQRT(DELTA))/2
         IF(ABS(SIGL(1,1)-L1).LT.PREC)THEN
            V1(1)=-(SIGL(2,2)-L1)
            V1(2)=SIGL(2,1)
            V1(3)=0.D0
         ELSE 
            V1(1)=-SIGL(1,2)
            V1(2)=SIGL(1,1)-L1
            V1(3)=0.D0
         ENDIF
         IF(ABS(SIGL(1,1)-L2).LT.PREC)THEN
            V2(1)=-(SIGL(2,2)-L2)
            V2(2)=SIGL(2,1)
            V2(3)=0.D0
         ELSE 
            V2(1)=-SIGL(1,2)
            V2(2)=SIGL(1,1)-L2
            V2(3)=0.D0
         ENDIF
         CALL NORMEV(V1,NORT1)
         CALL PSCVEC(3,L1,V1,V1)
         CALL NORMEV(V2,NORT2)
         CALL PSCVEC(3,L2,V2,V2)
C        PASSAGE AU REPERE GLOBAL
         IF(OPTION(1:14).EQ.'PROJ_ELEM_SIT1')THEN
            CALL PMAVEC('ZERO',3,MLG,V1,VTMP)
            ZR(ISIT1)  =VTMP(1)
            ZR(ISIT1+1)=VTMP(2)
            ZR(ISIT1+2)=VTMP(3)
            ZR(ISIT1+3)= L1
         ELSEIF(OPTION(1:14).EQ.'PROJ_ELEM_SIT2')THEN
            CALL PMAVEC('ZERO',3,MLG,V2,VTMP)
            ZR(ISIT2)  =VTMP(1)
            ZR(ISIT2+1)=VTMP(2)
            ZR(ISIT2+2)=VTMP(3)
            ZR(ISIT2+3)= L2
         ENDIF
         GOTO 111
      ENDIF
C
C --- 4. CALCUL DES OPTIONS : PROJ_ELEM_SIGN & PROJ_ELEM_SIGT
C     =======================================================
      IF(OPTION(1:13).EQ.'PROJ_ELEM_SIG')THEN
         IF(OPTION(1:14).EQ.'PROJ_ELEM_SIGN')THEN
            VTMP(1)=0.D0
            VTMP(2)=0.D0
            VTMP(3)=SIGL(3,3)
            CALL PMAVEC('ZERO',3,MLG,VTMP,VTMP2)
            ZR(ISIGN)  = VTMP2(1)
            ZR(ISIGN+1)= VTMP2(2)
            ZR(ISIGN+2)= VTMP2(3)
            ZR(ISIGN+3)= SIGL(3,3)
         ELSEIF(OPTION(1:14).EQ.'PROJ_ELEM_SIGT')THEN
            VTMP(1)=SIGL(1,1)
            VTMP(2)=SIGL(2,2)
            VTMP(3)=0.D0
            CALL PMAVEC('ZERO',3,MLG,VTMP,VTMP2)
            ZR(ISIGT)  = VTMP2(1)
            ZR(ISIGT+1)= VTMP2(2)
            ZR(ISIGT+2)= VTMP2(3)
         ENDIF
      ENDIF
C
 111  CONTINUE
C
      CALL JEDEMA()
C
      END
