      SUBROUTINE CHGREF ( GEOMI , X , Y , BIDIM )
      IMPLICIT   NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/09/2002   AUTEUR GREFFET N.GREFFET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     BUT : CHANGEMENT DE REPERE D'UN MAILLAGE
C
C     IN :
C            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE A TOURNER
C            X      : PREMIER VECTEUR DE LA BASE
C            Y      : DEUXIEME VECTEUR DE LA BASE
C            BIDIM  : BOOLEEN VRAI SI GEOMETRIE 2D
C     OUT:
C            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE ACTUALISE
C                 
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER       N1, I, IADCOO
      LOGICAL       BIDIM
      CHARACTER*8   K8BID
      CHARACTER*19  GEOMI
      CHARACTER*24  COORJV
      REAL*8        X(3), Y(3), Z(3), P(3), PREC, R8NRM2, R8DOT, R1, R2
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      COORJV=GEOMI(1:19)//'.VALE'
      CALL JEVEUO(COORJV,'E',IADCOO)
      CALL JELIRA(COORJV,'LONMAX',N1,K8BID)
      PREC=1.D-14
      N1=N1/3
      IADCOO=IADCOO-1
C     -- ON TRAITE LE CAS 2D SEPAREMENT POUR OPTIMISER :
      IF ( BIDIM ) THEN
         R1=R8NRM2(2,X,1)
         IF ( R1 .GT. PREC ) THEN
            X(1)=X(1)/R1
            X(2)=X(2)/R1
            X(3)=X(3)/R1
            Y(1)=(-1.D0)*X(2)
            Y(2)=X(1)
            DO 10 I=1,N1
               P(1)=ZR(IADCOO+3*(I-1)+1)
               P(2)=ZR(IADCOO+3*(I-1)+2)
               ZR(IADCOO+3*(I-1)+1)=R8DOT(2,X,1,P,1)
               ZR(IADCOO+3*(I-1)+2)=R8DOT(2,Y,1,P,1)
 10         CONTINUE
         ELSE
            CALL UTMESS('F','CHGREP','CE MOT CLE DE MODI_MAILLAGE'
     +      //' ATTEND UN VECTEUR DE NORME NON NULLE.')
         ENDIF
      ELSE
         R1=R8NRM2(3,X,1)
         R2=R8NRM2(3,Y,1)
         IF ( ( R8DOT(3,X,1,Y,1) .LT. PREC ) .AND. 
     +   ( (R1*R2) .GT. 0.D0 ) ) THEN
            X(1)=X(1)/R1
            X(2)=X(2)/R1
            X(3)=X(3)/R1
            Y(1)=Y(1)/R2
            Y(2)=Y(2)/R2
            Y(3)=Y(3)/R2
            CALL PROVEC(X,Y,Z)
            DO 20 I=1,N1
               P(1)=ZR(IADCOO+3*(I-1)+1)
               P(2)=ZR(IADCOO+3*(I-1)+2)
               P(3)=ZR(IADCOO+3*(I-1)+3)
               ZR(IADCOO+3*(I-1)+1)=R8DOT(3,X,1,P,1)
               ZR(IADCOO+3*(I-1)+2)=R8DOT(3,Y,1,P,1)
               ZR(IADCOO+3*(I-1)+3)=R8DOT(3,Z,1,P,1)
 20         CONTINUE
         ELSE
            CALL UTMESS('F','CHGREP','LE MOT CLE REPERE DE'
     +      //' MODI_MAILLAGE ATTEND DEUX VECTEURS' 
     +      //' NON NULS ORTHOGONAUX.')
         ENDIF
      ENDIF
      CALL JEDEMA()
      END
