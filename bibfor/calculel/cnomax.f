      SUBROUTINE CNOMAX(CNOZ,RMAX,NUMNO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 07/10/2004   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT      NONE
      CHARACTER*(*) CNOZ
      REAL*8        RMAX
      INTEGER       NUMNO
C
C ======================================================================
C ROUTINE APPELEE PAR : CVGCNT
C ======================================================================
C
C CALCULER LE MAX DE LA NORME DU DEPL. (DX DY DZ) DE CNO
C
C IN  CNO    : SD CHAM_NO
C OUT RMAX   : MAX DE LA NORME DU DEPL.
C OUT NUMNO  : NUMERO DU NOEUD REALISANT LE MAX DE DEPL.
C
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------
C
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
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      JCNSD,JCNSV,JCNSL
      INTEGER      NBNO,K,INO,NCMP
      CHARACTER*8  LICMP(3)
      CHARACTER*19 CNO,CNS1,CNS
      REAL*8       NORME
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CNO = CNOZ
      RMAX     = 0.D0
      CNS1     = '&&CNOMAX.CNS1'
      CNS      = '&&CNOMAX.CNS'
      NCMP     = 3
      LICMP(1) = 'DX'
      LICMP(2) = 'DY'
      LICMP(3) = 'DZ'
      NUMNO    = 0
      CALL CNOCNS(CNO,'V',CNS1)
      CALL CNSRED(CNS1,0,0,3,LICMP,'V',CNS)
      
      CALL JEVEUO(CNS//'.CNSD','L',JCNSD)
      CALL JEVEUO(CNS//'.CNSV','L',JCNSV)
      CALL JEVEUO(CNS//'.CNSL','L',JCNSL)

      NBNO = ZI(JCNSD-1+1)
     

      DO 10,INO = 1,NBNO
        NORME=0.D0
        DO 30,K = 1,NCMP
          IF (ZL(JCNSL-1+ (INO-1)*NCMP+K)) THEN 
             NORME=NORME+ZR(JCNSV-1+ (INO-1)*NCMP+K)**2
          ENDIF
   30   CONTINUE
        IF (SQRT(NORME).GE.RMAX) THEN
           RMAX  = SQRT(NORME)
           NUMNO = INO
        ENDIF
   10 CONTINUE


      CALL DETRSD('CHAM_NO_S',CNS1)
      CALL DETRSD('CHAM_NO_S',CNS)
      CALL JEDEMA()
      END
