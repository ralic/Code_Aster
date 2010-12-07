      SUBROUTINE VFGEFA(MAXDIM,NDIM,NBNOS,XS,T,XG,
     >    SURF,NORM,XGF,D,IRET)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
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
C ======================================================================
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C ======================================================================
C
C  CALCUL DES ELEMENTS GEOMETRIQUES D UNE FACE SUPPOSEE COPLANAIRE
C
C IDIM  : 1,NDIM
C IN
C     MAXDIM
C     NDIM
C     NBNOS NBRE DE SOMMETS = NOMBRE DE ARETE
C     XS(1:MAXDIM,J) COORD SOMMET J 
C     T(1:MAXDIM,J)  COORD DU VECTEUR DE L J EME ARETE
C     XG  COORD D UN POINT QUI PEMET D ORIENTER LA FACE 
C        (XGF-XG).N >0 
C OUT
C     SURF SURFACE DE LA FACE
C     NORM NORMALE
C     XGF BARYCENTRE FACE
C     D = (XGF-XG).N  
      IMPLICIT NONE 
      INTEGER  MAXDIM,NDIM,NBNOS,IRET,IDIM,IS
      REAL*8   XS(1:MAXDIM,NBNOS),T(1:MAXDIM,NBNOS)
      REAL*8   XG(NDIM),SURF,NORM(1:MAXDIM),XGF(1:MAXDIM),D
      REAL*8   XS1(1:3,3),T1(1:3,2)
      REAL*8   SURF1,NORM1(1:3),XGF1(1:3),D1 
      REAL*8   XS2(1:3,3),T2(1:3,2)
      REAL*8   SURF2,NORM2(1:3),XGF2(1:3),D2,N1VN2(1:3)
      REAL*8   XN1N2      
      CALL ASSERT(NDIM.EQ.3)
      CALL ASSERT (MAXDIM.GE.3)
      CALL ASSERT ((NBNOS.GE.4).AND.(NBNOS.GE.3))
C  
      IF ( NBNOS.EQ.3) THEN
         CALL VFGETR(MAXDIM,NDIM,NBNOS,XS,T,XG,
     >    SURF,NORM,XGF,D)
      ELSE
        DO 10 IDIM=1,NDIM
         XGF(IDIM)=0.D0
         DO 11 IS = 1 , NBNOS
          XGF(IDIM)=XGF(IDIM)+XS(IDIM,IS)
   11    CONTINUE
        XGF(IDIM)=XGF(IDIM)/NBNOS
   10  CONTINUE
        DO 20 IDIM = 1 , NDIM
          XS1(IDIM,1)=XS(IDIM,1)
          XS1(IDIM,2)=XS(IDIM,2)
          XS1(IDIM,3)=XS(IDIM,3)
          T1(IDIM,1)=T(IDIM,1)
          T1(IDIM,2)=T(IDIM,2)
   20   CONTINUE
         CALL VFGETR(3,NDIM,3,XS1,T1,XG,
     >    SURF1,NORM1,XGF1,D1)
        DO 30 IDIM = 1 , NDIM
          XS2(IDIM,1)=XS(IDIM,3)
          XS2(IDIM,2)=XS(IDIM,4)
          XS2(IDIM,3)=XS(IDIM,1)
          T2(IDIM,1)=T(IDIM,3)
          T2(IDIM,2)=T(IDIM,4)
   30   CONTINUE
         CALL VFGETR(3,NDIM,3,XS2,T2,XG,
     >    SURF2,NORM2,XGF2,D2)
C
C   ON VERIFIRE QUE NORM1 ET NORM2 SONT PARALLELLES
C         
        CALL PROVEC(NORM1,NORM2,N1VN2)
        XN1N2=SQRT(N1VN2(1)**2+N1VN2(2)**2+N1VN2(3)**2)
        IF ( XN1N2.GT.1.D-6) THEN
         IRET = 1
        ELSE
         IRET = 0
         SURF = SURF1+SURF2
         NORM(1)=NORM2(1)
         NORM(2)=NORM2(2)
         NORM(3)=NORM2(3)
         D =(XGF(1)-XG(1))*NORM(1)+
     >     (XGF(2)-XG(2))*NORM(2)+
     >     (XGF(3)-XG(3))*NORM(3)
         CALL ASSERT(D.GT.0.D0)
        ENDIF
       ENDIF
       END
