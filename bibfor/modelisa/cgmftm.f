      SUBROUTINE CGMFTM(TYMAZ,NOMAZ,LISMA,NBMA,IERR)
      IMPLICIT NONE
      INTEGER NBMA,IERR
      CHARACTER*(*) NOMAZ, TYMAZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/03/2011   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRS_1404
C       OPERATEUR: DEFI_GROUP/CREA_GROUP_MA
C
C       CGMFTM -- TRAITEMENT DU FILTRE DES MAILLES
C                 EN FONCTION DE LEUR TYPE.
C
C -------------------------------------------------------
C  TYMA          - IN    - K8   - : TYPE DE MAILLE RETENU
C                                   ("TOUT","0D","1D","2D","3D")
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISMAZ        - INOUT - K24  - : NOM DE LA LISTE DE MAILLES A FILTRER
C                                   ET FILTREE
C  NBMA          - INOUT -  I   - : LONGUEUR DE CETTE LISTE
C  IERR          - OUT   -  I   - : CODE RETOUR (=0 OU 1)
C                                   
C
C  REMARQUES : 
C     IERR=0 : OK
C        => ON A OBTENU DES MAILLES EN FILTRANT LA LISTE DE MAILLES, 
C           LA LISTE DE MAILLES RETOURNEE EST LA LISTE FILTREE,
C           LE NOMBRE DE MAILLES RETOURNE EST LA LONGUEUR DE LA LISTE
C           FILTREE.
C     IERR=1 : NOOK
C        => AUCUNE MAILLE N'A ETE RETENUE CAR LE TYPE DE MAILLE SOUHAITE
C           PAR L'UTILISATEUR NE CORRESPOND PAS AUX TYPES DES MAILLES
C           DE LA LISTE.
C           LA LISTE DE MAILLE RETOURNEE EST LA LISTE INITIALE,
C           LE NOMBRE DE MAILLES RETOURNE EST LA LONGUEUR DE LA LISTE
C           INITIALE.
C           
C -------------------------------------------------------
C
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

      INTEGER I,DIM,JLIMA,NBTROU,TABMAI(NBMA),TATROU(NBMA)
      CHARACTER*8  NOMA, TYMA
      CHARACTER*24 LISMA

      CALL JEMARQ()

      NOMA=NOMAZ
      TYMA=TYMAZ
   
      CALL JEVEUO(LISMA ,'L',JLIMA)
      DO 10 I=1,NBMA
          TABMAI(I)=ZI(JLIMA+I-1)
 10   CONTINUE

      IF (TYMA.EQ.'0D') THEN
        DIM = 0
      ELSE IF (TYMA.EQ.'1D') THEN
        DIM = 1
      ELSE IF (TYMA.EQ.'2D') THEN
        DIM = 2
      ELSE IF (TYMA.EQ.'3D') THEN
        DIM = 3
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      CALL UTFLM2(NOMA,TABMAI,NBMA,DIM,NBTROU,TATROU)
      
      IF (NBMA.EQ.0) THEN
        IERR = 1
      ELSE
        IERR = 0
        NBMA = NBTROU
        CALL JEVEUO(LISMA ,'E',JLIMA)
        DO 20 I=1,NBMA
          ZI(JLIMA+I-1)=TATROU(I)
 20     CONTINUE
        
      ENDIF

      CALL JEDEMA()

      END
