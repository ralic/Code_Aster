      SUBROUTINE XORIFF(NFON,JFON,PFI,O,VOR,DIMIN)
      IMPLICIT NONE 

      INTEGER       NFON,JFON
      REAL*8        PFI(3),O(3),VOR(3),DIMIN


C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/07/2004   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ORIENTATION DES POINTS DU FOND DE FISSURE DANS LE CADRE DE XFEM
C     
C  ENTRESS : 
C     JFON  :   ADRESSE DES POINTS DU FOND DE FISSURE DÉSORDONNÉS
C     NFON  :   NOMBRE DE POINTS DU FOND DE FISSURE DÉSORDONNÉS
C     PFI   :   POINT DU FOND DE FISSURE DEMANDÉ
C     O     :   POINT ORIGINE DE L'ORIENTATION
C     VOR   :   VECTEUR D'ORIENTATION
C
C  SORTIES : 
C     JFON  :  ADRESSE DES POINTS DU FOND DE FISSURE ORDONNÉS
C     DIMIN :  DISTANCE ENTRE PFON_INI DEMANDE ET TROUVE
C
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER         I,PI,J,PD,PP,K
      REAL*8          R8MAEM,M(3),D,PADIST,A(3),OA(3),NOA,AM(3),PS
      REAL*8          PS1,LAMBDA,H(3),OH(3),NOH,COS,THETA,TRIGOM,R3(3)
      REAL*8          R8PI,TAMPON(4),EPS

C ----------------------------------------------------------------------

      CALL JEMARQ()
      
      EPS=-1.0D-10
      
C      DÉTERMINATION DU NUMÉRO DU POINT LE PLUS PROCHE DE PFON_INI : A
       PI=0
       DIMIN=R8MAEM()
       DO 500 I=1,NFON
         M(1)=ZR(JFON-1+4*(I-1)+1)
         M(2)=ZR(JFON-1+4*(I-1)+2)
         M(3)=ZR(JFON-1+4*(I-1)+3)
         D=PADIST(3,PFI,M)
         IF (D.LT.DIMIN) THEN
           DIMIN=D
           PI=I
         ENDIF
 500   CONTINUE
       IF (DIMIN.GT.10) CALL UTMESS('A','XORIFF','PFON_INI LOIN DE '//
     &                                                      'FOND_FISS')

       DO 501 I=1,3
         A(I)=ZR(JFON-1+4*(PI-1)+I)
         OA(I)=A(I)-O(I)
 501   CONTINUE

       NOA=SQRT(OA(1)*OA(1) + OA(2)*OA(2)  +  OA(3)*OA(3))
       IF (NOA.LT.1.D-6) CALL UTMESS('F','XORIFF','PFON_INI = '//
     &                                                   'PT_ORIGINE')

C      BOUCLE SUR LES POINTS M DE FONFIS POUR CALCULER L'ANGLE THETA
       DO 510 I=1,NFON
         DO 511 J=1,3
           M(J)=ZR(JFON-1+4*(I-1)+J)
           AM(J)=M(J)-A(J)
 511     CONTINUE
         CALL PSCAL(3,AM,VOR,PS)
         CALL PSCAL(3,VOR,VOR,PS1)
         LAMBDA=-PS/PS1
         DO 512 J=1,3
           H(J)=M(J)+LAMBDA*VOR(J)
           OH(J)=H(J)-O(J)
 512     CONTINUE
         CALL PSCAL(3,OA,OH,PS)
         NOH=SQRT(OH(1)*OH(1) + OH(2)*OH(2)  +  OH(3)*OH(3))
         IF (NOH.EQ.0) CALL UTMESS('F','XORIFF','PROBLEME DANS L '//
     &                           'ORIENTATION DU FOND DE FISSURE :'//
     &                           ' PT_ORIGIN MAL CHOISI.')
         COS=PS/(NOA*NOH)
         THETA=TRIGOM('ACOS',COS)
C        SIGNE DE THETA (06/01/2004)
         CALL PROVEC(OA,OH,R3)
         CALL PSCAL(3,R3,VOR,PS)

         IF (PS.LT.EPS) THETA = -1 * THETA + 2 * R8PI()
         ZR(JFON-1+4*(I-1)+4)=THETA
 510   CONTINUE

C      TRI SUIVANT THETA CROISSANT
       DO 520 PD=1,NFON-1
         PP=PD
         DO 521 I=PP,NFON
           IF (ZR(JFON-1+4*(I-1)+4).LT.ZR(JFON-1+4*(PP-1)+4)) PP=I
 521     CONTINUE
         DO 522 K=1,4
           TAMPON(K)=ZR(JFON-1+4*(PP-1)+K)
           ZR(JFON-1+4*(PP-1)+K)=ZR(JFON-1+4*(PD-1)+K)
           ZR(JFON-1+4*(PD-1)+K)=TAMPON(K)
 522     CONTINUE
 520   CONTINUE

      CALL JEDEMA()
      END
