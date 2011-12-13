      SUBROUTINE C3DREP(NOMTE,EPAIS,ALPHA,BETA,COORD,
     &                  NUMNOE,PGL)
      IMPLICIT NONE
      INTEGER      NUMNOE
      CHARACTER*16 NOMTE
      REAL*8       EPAIS,ALPHA,BETA,COORD(3,9),PGL(3,3)
C     ---------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/10/2011   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE SELLENET N.SELLENET
C     ------------------------------------------------------------------
C
C         CETTE ROUTINE REALISE LA MEME TACHE QUE COQREP MAIS POUR LES
C         COQUES 3D
C         CALCUL DE LA MATRICE DE PASSAGE DU REPERE DE L'ELEMENT A
C         LA VARIETE (LE REPERE DE LA VARIETE EST OBTENU PAR LA MATRICE
C         DE PASSAGE GLOBAL -> LOCAL) AINSI QUE SON INVERSE
C
C     ------------------------------------------------------------------
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
      CHARACTER*16               ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                          ZK80
      COMMON  / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER NB1,NB2,NPGSR,I,J,K,IND,INTSR,LZI,LZR
C
      REAL*8 VECTA(9,2,3),VECTN(9,3),VECTG(2,3),VECTT(3,3)
      REAL*8 ZERO,VECTPT(9,2,3),VECTMP(3,3),PGLTMP(3,3)
      REAL*8 MATEVN(2,2,10),MATEVG(2,2,10),V
C
      CALL JEVEUO('&INEL.'//NOMTE(1:8)//'.DESI','L', LZI )
      CALL JEVEUO('&INEL.'//NOMTE(1:8)//'.DESR','L', LZR )
      NB1  =ZI(LZI-1+1)
      NB2  =ZI(LZI-1+2)
      NPGSR=ZI(LZI-1+3)

C     -- POUR REMPLIR LZR+1090+...  ET CALCULER VECTN :
      CALL VECTAN(NB1,NB2,COORD,ZR(LZR),VECTA,VECTN,VECTPT)

C     -- POUR REMPLIR LZR+2000+... :
C     -- QUELLE VALEUR POUR IND ? FICHE ???
      IND =0
      K = 0
      DO 110 INTSR=1,NPGSR
        CALL VECTGT(IND,NB1,COORD,ZERO,INTSR,ZR(LZR),EPAIS,
     &              VECTN,VECTG,VECTT)
        DO 120 J = 1, 3
          DO 130 I = 1, 3
            K = K + 1
            ZR(LZR+2000+K-1) = VECTT(I,J)
 130      CONTINUE
 120    CONTINUE
 110  CONTINUE
C
      CALL VDREP2(ALPHA,BETA,ZI(LZI),ZR(LZR),MATEVN,MATEVG)
C
      VECTMP(1,1) = MATEVN(1,1,NUMNOE)
      VECTMP(1,2) = MATEVN(1,2,NUMNOE)
      VECTMP(2,1) = MATEVN(2,1,NUMNOE)
      VECTMP(2,2) = MATEVN(2,2,NUMNOE)
      VECTMP(1,3) = 0.D0
      VECTMP(2,3) = 0.D0
      VECTMP(3,3) = 1.D0
      VECTMP(3,1) = 0.D0
      VECTMP(3,2) = 0.D0
C
      K = 0
      DO 20 J = 1, 3
        DO 30 I = 1, 3
          K  = K + 1
          PGLTMP(I,J) = ZR(LZR+1090+(NUMNOE-1)*9+K-1)
  30    CONTINUE
  20  CONTINUE
      DO 1 I = 1,3
        DO 2 J = 1,3
          V = 0.D0
          DO 3 K = 1,3
            V = V + VECTMP(I,K) * PGLTMP(K,J)
 3        CONTINUE
          PGL(I,J) = V
 2      CONTINUE
 1    CONTINUE
C
      END
