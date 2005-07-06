      SUBROUTINE  XCRVOL(IGEOM,PINTT,CNSET,HEAVT,LONCH,CRIT)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/07/2005   AUTEUR GENIAUT S.GENIAUT 
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
      REAL*8        HE,CO(4,3),A,B,C,D,E,F,P,Q,R,BDE,ADF,ABC,VOL
      REAL*8        VOLM,VOLP
      INTEGER       CONNEC(6,4),NINTER,NPTS,NSE,CNSE(6,4),NSEMAX,NPG
      INTEGER       I,IT,ISE,IN,INO,NIT,CPT,NCMP,IDEB,JCOORS,NNOM
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

C         CALCUL DU VOL DU SS-TET AVEC LA FORMULE D'EULER
          A=SQRT((CO(2,1)-CO(4,1))**2+(CO(2,2)-CO(4,2))**2+
     &           (CO(2,3)-CO(4,3))**2)
          B=SQRT((CO(2,1)-CO(3,1))**2+(CO(2,2)-CO(3,2))**2+
     &           (CO(2,3)-CO(3,3))**2)
          C=SQRT((CO(4,1)-CO(3,1))**2+(CO(4,2)-CO(3,2))**2+
     &           (CO(4,3)-CO(3,3))**2)
          D=SQRT((CO(2,1)-CO(1,1))**2+(CO(2,2)-CO(1,2))**2+
     &           (CO(2,3)-CO(1,3))**2)
          E=SQRT((CO(1,1)-CO(3,1))**2+(CO(1,2)-CO(3,2))**2+
     &           (CO(1,3)-CO(3,3))**2)
          F=SQRT((CO(1,1)-CO(4,1))**2+(CO(1,2)-CO(4,2))**2+
     &           (CO(1,3)-CO(4,3))**2)

          BDE=B*B+D*D-E*E
          ADF=A*A+D*D-F*F
          ABC=A*A+B*B-C*C

          P=4*A*A*B*B*D*D          
          Q=A*A*BDE*BDE-B*B*ADF*ADF-D*D*ABC*ABC
          R=BDE*ADF*ABC
          
          VOL=SQRT(P-Q+R)/12.D0
          
          IF (HE.EQ.-1) VOLM=VOLM+VOL
          IF (HE.EQ.1)  VOLP=VOLP+VOL

 110    CONTINUE

 100  CONTINUE

      CRIT=MIN(VOLP,VOLM)/MAX(VOLP,VOLM)

      END
