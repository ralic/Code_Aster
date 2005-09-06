      SUBROUTINE  XCRVOL(IGEOM,PINTT,CNSET,HEAVT,LONCH,CRIT)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/09/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
       IMPLICIT NONE
C
       INTEGER      IGEOM,CNSET(4*32),HEAVT(36),LONCH(7)
       REAL*8       CRIT,PINTT(3*11)
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER  ZI
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
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C.......................................................................
C
C     BUT:  CALCUL DE CRITERE PABSÉ SUR LE RAPPORT DES VOLUMES
C.......................................................................

C..............................................................
C----------------------------------------------------------------
      CHARACTER*8   ELREFP
      REAL*8        HE,CO(4,3),VOL,VOLM,VOLP,MAT(3,3)
      INTEGER       CONNEC(6,4),NINTER,NPTS,NSE,CNSE(6,4),NSEMAX,NPG
      INTEGER       I,IT,ISE,IN,INO,NIT,CPT,NCMP,IDEB,JCOORS,NNOM,J
      PARAMETER    (NSEMAX=6)

C     RÉCUPÉRATION DE LA SUBDIVISION L'ÉLÉMENT PARENT EN NIT TETRAS 
      NIT=LONCH(1)

      VOLM=0.D0
      VOLP=0.D0

      CPT=0
C     BOUCLE SUR LES NIT TETRAS
      DO 100 IT=1,NIT

C       RÉCUPÉRATION DU DÉCOUPAGE EN NSE SOUS-ÉLÉMENTS 
        NSE=LONCH(1+IT)

C       BOUCLE D'INTÉGRATION SUR LES NSE SOUS-ÉLÉMENTS
        DO 110 ISE=1,NSE

          CPT=CPT+1

C         COORDONNÉES DES SOMMETS DU SOUS-TÉTRA EN QUESTION
          DO 112 IN=1,4
            INO=CNSET(4*(CPT-1)+IN)
            IF (INO.LT.1000) THEN
              CO(IN,1)=ZR(IGEOM-1+3*(INO-1)+1)
              CO(IN,2)=ZR(IGEOM-1+3*(INO-1)+2)
              CO(IN,3)=ZR(IGEOM-1+3*(INO-1)+3)
            ELSE
              CO(IN,1)=PINTT(3*(INO-1000-1)+1)
              CO(IN,2)=PINTT(3*(INO-1000-1)+2)
              CO(IN,3)=PINTT(3*(INO-1000-1)+3)
            ENDIF
 112      CONTINUE

C         FONCTION HEAVYSIDE CSTE SUR LE SS-ÉLT
          HE=HEAVT(NSEMAX*(IT-1)+ISE)

C         CALCUL DU VOLUME DU TETRAEDRE PAR LE DETERMINANT :
C         VOLUME = |DETERMINANT| / 6
          DO 113 I=1,3
            DO 114 J=1,3
               MAT(I,J)=CO(1,J)-CO(I+1,J)
 114        CONTINUE
 113      CONTINUE          

C         DETERMINANT CALCULÉ PAR LA RÈGLE DE SARRUS
          VOL =  ABS( MAT(1,1)*MAT(2,2)*MAT(3,3)
     +              + MAT(2,1)*MAT(3,2)*MAT(1,3)
     +              + MAT(3,1)*MAT(1,2)*MAT(2,3)
     +              - MAT(3,1)*MAT(2,2)*MAT(1,3)
     +              - MAT(2,1)*MAT(1,2)*MAT(3,3)
     +              - MAT(1,1)*MAT(3,2)*MAT(2,3) )/6.D0

          IF (HE.EQ.-1) VOLM=VOLM+VOL
          IF (HE.EQ.1)  VOLP=VOLP+VOL

 110    CONTINUE

 100  CONTINUE

      CRIT=MIN(VOLP,VOLM)/MAX(VOLP,VOLM)

      END
