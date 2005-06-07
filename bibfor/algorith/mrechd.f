      SUBROUTINE MRECHD(IZONE,COOR1,NEWGEO,DEFICO,POSMIN,DIR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/08/2002   AUTEUR ADBHHPM P.MASSIN 
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

      IMPLICIT NONE

      INTEGER IZONE,POSMIN
      REAL*8 COOR1(3),DIR(3),NORMD,NORMV
      CHARACTER*24 NEWGEO,DEFICO

C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : MAPPAR
C ----------------------------------------------------------------------
C ON CHERCHE LE NOEUD MAITRE LE PLUS PROCHE DANS UNE DIRECTION DONNEE.

C  ---->   IZONE  : NUMERO DE LA ZONE DE CONTACT
C  ---->   COOR1  : COORDONNEES DU NOEUD ESCLAVE
C  ---->   DIR    : DIRECTION DE LA RECHERCHE
C  ---->   NEWGEO : NOUVELLE GEOMETRIE (AVEC DEPLACEMENT GEOMETRIQUE)
C  ---->   DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)

C  <----  POSMIN :POSITION (DANS CONTANO) DU NOEUD MAITRE LE PLUS PROCHE

C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------

C      CHARACTER*32       JEXNUM , JEXNOM
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

C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------

      INTEGER ISURF2,NBNO2,JDEC2,K2
      INTEGER POSNO2,NUMNO2,IFM,NIV
      INTEGER JZONE,JNOCO,JSUNO
      INTEGER JCOOR
      REAL*8 COOR2(3),R8GAEM,DMIN,DIST
      CHARACTER*24 PZONE,CONTNO,PSURNO

C ----------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C ======================================================================
C               RECUPERATION D'ADRESSES ET DE DIMENSIONS
C ======================================================================

C --- LECTURE DES SD POUR LE CONTACT POTENTIEL

      PZONE = DEFICO(1:16)//'.PZONECO'
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      PSURNO = DEFICO(1:16)//'.PSUNOCO'

      CALL JEVEUO(PZONE,'L',JZONE)
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(PSURNO,'L',JSUNO)
      CALL JEVEUO(NEWGEO(1:19)//'.VALE','L',JCOOR)


C ======================================================================
C REPERAGE DE LA SURFACE MAITRE DE LA ZONE NUMERO IZONE
C ======================================================================

C --- NUMEROS DE LA  SURFACE MAITRE
       ISURF2 = ZI(JZONE+IZONE)
C --- NOMBRE DE NOEUDS DE LA SURFACE MAITRE

      NBNO2 = ZI(JSUNO+ISURF2) - ZI(JSUNO+ISURF2-1)
      JDEC2 = ZI(JSUNO+ISURF2-1)

C ======================================================================
C  DOUBLE BOUCLE SUR LES NOEUDS
C ======================================================================

C  ( R8GAEM () DOIT DONNER UNE VALEUR TRES GRANDES)
      DMIN = 0.D00
      DO 10 K2 = 1,NBNO2
        POSNO2 = JDEC2 + K2
        NUMNO2 = ZI(JNOCO+POSNO2-1)
        COOR2(1) = ZR(JCOOR+3* (NUMNO2-1))
        COOR2(2) = ZR(JCOOR+3* (NUMNO2-1)+1)
        COOR2(3) = ZR(JCOOR+3* (NUMNO2-1)+2)
        NORMD    = SQRT(DIR(1)*DIR(1)+DIR(2)*DIR(2)+DIR(3)*DIR(3))
        NORMV    = SQRT((COOR1(1)-COOR2(1))**2+(COOR1(2)-COOR2(2))**2
     &                 +(COOR1(3)-COOR2(3))**2)
        IF(NORMV.EQ.0.D00) THEN 
           DIST=1.D00
           GOTO 9
        END IF  
       DIST = ABS((COOR1(1)-COOR2(1))*DIR(1)+ (COOR1(2)-COOR2(2))
     &        *DIR(2)+ (COOR1(3)-COOR2(3))*DIR(3))/(NORMD*NORMV)
 9     CONTINUE   
       IF (DIST.GT.DMIN) THEN
          POSMIN = POSNO2
          DMIN = DIST
        END IF
   10 CONTINUE

      CALL JEDEMA()
      END
