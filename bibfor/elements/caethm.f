      SUBROUTINE CAETHM(NOMTE,VOL2,BORD2,VOL3,BORD3,
     > ALIAS,AXI,DPLAN,TRAITE,
     > ISTHS2,ISTHS3,ISTHF8,ISTHF6,ISTH10,ISTH13,ISTH15,ISTH20,
     > ISTHT3,ISTHQ4,ISTHT6,ISTHQ8,
     > NNOMAX,NVOMAX,NSOMAX,
     > NSOM,VOISIN,NBVOS,P2P1,LUMPED)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/10/2002   AUTEUR UFBHHLL C.CHAVANT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C 
      IMPLICIT NONE
C
      CHARACTER*16       NOMTE      
      CHARACTER*8        ALIAS
      LOGICAL VOL2,BORD2,VOL3,BORD3,AXI,DPLAN,TRAITE
      LOGICAL ISTHT3
      LOGICAL ISTHQ4
      LOGICAL ISTHT6
      LOGICAL ISTHQ8
      LOGICAL ISTHS2
      LOGICAL ISTHS3
      LOGICAL ISTH10,ISTH13,ISTH15,ISTH20
      LOGICAL ISTHF8
      LOGICAL ISTHF6
      LOGICAL P2P1,LUMPED
      INTEGER NNOMAX,NVOMAX,NSOMAX
      INTEGER NSOM
      INTEGER VOISIN(NVOMAX,NNOMAX)
      INTEGER NBVOS(NSOMAX) 
C.......................................................................
C
C CETTE ROUTINE DONNE L ALIAS DES ELEMENTS 2D ET 1D THM
C               ELLE DIT SI ILS SONT DE TYPE T3 Q4 T6 Q8
C                                           S2 S3
C
C IN NNOMAX : DIMENSIONNEMENT DE VOISIN
C IN  NVOMAX : NOMBRE MAXIMUM DE MILIEUX VOIISINS D UN SOMMET
C IN NSOMAX  : NOMBRE MAXIMUM DE SOMMETS
C IN  NOMTE  : NOM DU TYPE ELEMENT
C IN  VOL2    : ON TRAITE LES ELEMENTS VOLUMIQUES 2D
C IN  BORD2   : ON TRAITE LES BORDS D ELEMNTS VOLUMIQUES 2D
C IN  VOL3    : ON TRAITE LES ELEMENTS VOLUMIQUES 3D
C IN  BORD3   : ON TRAITE LES BORDS D ELEMNTS VOLUMIQUES 3D
C  REMARQUE BORD2 ET VOL2 ON TRAITE LES DEUX
C OUT ALIAS:
C OUT AXI     :
C OUT DPLAN   :
C OUT TRAITE  :  
C OUT ISTHT3  : TRUE SI T3
C OUT ISTHQ4  : TRUE SI Q4
C OUT ISTHT6  : TRUE SI T6
C OUT ISTHQ8  : TRUE SI Q8
C OUT P2P1   : TRUE SI P2P1
C OUT LUMPED   : TRUE SI LUMPED
C OUT ISTH10  : TETRA 10
C OUT ISTH13  : PYRAM 13
C OUT ISTH15  : PENTA 15
C OUT ISTH20  : HEXA  20
C OUT VOISIN
C  POUR UN SOMMET J DE 1 A NSOM : 
C         VOISIN(1:NDIM,J) =  LES NDIM NOEUDS MILIEUX VOSIN DU SOMMMET
C
C
C  POUR UN MILIEU J DE 1 A NSOM +1 A NNO: 
C         VOISIN(1:2,J) =  LES 2 NOEUDS DU SEGMENT DONT IL EST LE MILIEU
C
C.......................................................................
C
C OUT NBVOS  : NBVOS(1,NSOMAX) : NOMBRE DE VOISINS D UN SOMMET < NVOMAX
C OUT NSOM   : NOMBRE DE NOEUDS SOMMETS
      INTEGER IS
      INTEGER IL,NLUMP
      PARAMETER (NLUMP=68)
      CHARACTER*16 ELLUM(NLUMP)

      LOGICAL TROUVE
C
      INTEGER NTHMS3,IHM3
      PARAMETER (NTHMS3=12)
      CHARACTER*16 THMS3(NTHMS3)
C
      INTEGER NTHMS2,IHM2
      PARAMETER (NTHMS2=10)
      CHARACTER*16 THMS2(NTHMS2)
C
      INTEGER NTHMT3
      PARAMETER (NTHMT3=20)
      CHARACTER*16 THMT3(NTHMT3)
C
      INTEGER NTHMQ4,IHM4
      PARAMETER (NTHMQ4=20)
      CHARACTER*16 THMQ4(NTHMQ4)
C
      INTEGER NTHMT6,IHM6
      PARAMETER (NTHMT6=22)
      CHARACTER*16 THMT6(NTHMT6)
C
      INTEGER NTHMQ8,IHM8
      PARAMETER (NTHMQ8=22)
      CHARACTER*16 THMQ8(NTHMQ8)
C
      INTEGER NTHMF8,IHF8
      PARAMETER (NTHMF8=6)
      CHARACTER*16 THMF8(NTHMF8)
C
      INTEGER NTHMF6,IHF6
      PARAMETER (NTHMF6=6)
      CHARACTER*16 THMF6(NTHMF6)
C
      INTEGER NHMH10,IHH10
      PARAMETER (NHMH10=12)
      CHARACTER*16 THMH10(NHMH10)
C
      INTEGER NHMH13,IHH13
      PARAMETER (NHMH13=12)
      CHARACTER*16 THMH13(NHMH13)
C
      INTEGER NHMH15,IHH15
      PARAMETER (NHMH15=12)
      CHARACTER*16 THMH15(NHMH15)
C
      INTEGER NHMH20,IHH20
      PARAMETER (NHMH20=12)
      CHARACTER*16 THMH20(NHMH20)
      
C
C     NTHMS2 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE SE2
C     THMS2  : NOM DE CES ELEMENTS
C     ISTHS2  : TRUE SI APPARTIENT A CETTE FAMILLE
C
C     NTHMS3 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE SE3
C     THMS3  : NOM DE CES ELEMENTS
C     ISTHS3  : TRUE SI APPARTIENT A CETTE FAMILLE
      DATA THMS3 / 
     > 'THHM_D_PLAN_SE3 ',
     > 'HM_D_PLAN_SE3   ',
     > 'HHM_D_PLAN_SE3  ',
     > 'THH_D_PLAN_SE3  ',
     > 'THM_D_PLAN_SE3  ',
     > 'THHM_AXIS_SE3   ',
     > 'HM_AXIS_SE3     ',
     > 'HHM_AXIS_SE3    ',
     > 'THH_AXIS_SE3    ',
     > 'THM_AXIS_SE3    ',
     > 'THV_AXIS_SE3    ',
     > 'THV_D_PLAN_SE3  ' /
C
      DATA THMS2 / 
     > 'THHM_D_PLAN_SE2 ',
     > 'HM_D_PLAN_SE2   ',
     > 'HHM_D_PLAN_SE2  ',
     > 'THH_D_PLAN_SE2  ',
     > 'THM_D_PLAN_SE2  ',
     > 'THHM_AXIS_SE2   ',
     > 'HM_AXIS_SE2     ',
     > 'HHM_AXIS_SE2    ',
     > 'THH_AXIS_SE2    ',
     > 'THM_AXIS_SE2    ' /
C      
      
C
C     NTHMT3 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE TR3
C     THMT3  : NOM DE CES ELEMENTS
C     ISTHT3  : TRUE SI APPARTIENT A CETTE FAMILLE
C
      DATA THMT3 / 
     > 'HM_DPTR3        ','HHM_DPTR3       ',
     > 'THH_DPTR3       ','THM_DPTR3       ',
     > 'THHM_DPTR3      ', 'THHM_AXIS_TR3   ',
     > 'HM_AXIS_TR3     ', 'HHM_AXIS_TR3    ',
     > 'THH_AXIS_TR3    ','THM_AXIS_TR3    ',     
     > 'HM_DPTR3D       ','HHM_DPTR3D      ',
     > 'THH_DPTR3D      ','THM_DPTR3D      ',
     > 'THHM_DPTR3D     ','THHM_AXIS_TR3D  ',
     > 'HM_AXIS_TR3D    ','HHM_AXIS_TR3D   ',
     > 'THH_AXIS_TR3D   ','THM_AXIS_TR3D   '  /
C
C     NTHMQ4 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE QUAD4
C     THMQ4  : NOM DE CES ELEMENTS
C     ISTHQ4  : TRUE SI APPARTIENT A CETTE FAMILLE
C
      DATA THMQ4 / 
     > 'HM_DPQ4         ','HHM_DPQ4        ',
     > 'THH_DPQ4        ','THM_DPQ4        ',
     > 'THHM_DPQ4       ','THHM_AXIS_QU4   ',
     > 'HM_AXIS_QU4     ','HHM_AXIS_QU4    ',
     > 'THH_AXIS_QU4    ','THM_AXIS_QU4    ',
     > 'HM_DPQ4D        ','HHM_DPQ4D       ',
     > 'THH_DPQ4D       ', 'THM_DPQ4D       ',
     > 'THHM_DPQ4D      ','THHM_AXIS_QU4D  ',
     > 'HM_AXIS_QU4D    ','HHM_AXIS_QU4D   ',
     > 'THH_AXIS_QU4D   ','THM_AXIS_QU4D   ' /
C
C     NTHMT6 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE TR6
C     THMT6  : NOM DE CES ELEMENTS
C     ISTHT6  : TRUE SI APPARTIENT A CETTE FAMILLE
C
      DATA THMT6 / 
     > 'HM_DPTR6        ','HHM_DPTR6       ',
     > 'THH_DPTR6       ','THM_DPTR6       ',
     > 'THHM_DPTR6      ','THHM_AXIS_TR6   ',
     > 'HM_AXIS_TR6     ','HHM_AXIS_TR6    ',
     > 'THH_AXIS_TR6    ','THM_AXIS_TR6    ',
     
     > 'HHM_DPTR6D      ','THH_DPTR6D      ',
     > 'THM_DPTR6D      ','THHM_DPTR6D     ',
     > 'HM_DPTR6D       ','HM_AXIS_TR6D    ',
     > 'HHM_AXIS_TR6D   ','THH_AXIS_TR6D   ',
     > 'THM_AXIS_TR6D   ','THHM_AXIS_TR6D  ',
     > 'THV_DPTR6D      ','THV_AXIS_TR6D   '/
C
C     NTHMQ8 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE QUAD8
C     THMQ8  : NOM DE CES ELEMENTS
C     ISTHQ8  : TRUE SI APPARTIENT A CETTE FAMILLE
C
      DATA THMQ8 / 
     > 'HM_DPQ8         ','HHM_DPQ8        ',
     > 'THH_DPQ8        ','THM_DPQ8        ',
     > 'THHM_DPQ8       ','THHM_AXIS_QU8   ',
     > 'HM_AXIS_QU8     ','HHM_AXIS_QU8    ',
     > 'THH_AXIS_QU8    ','THM_AXIS_QU8    ', 
     > 'HM_DPQ8D        ','HHM_DPQ8D       ',
     > 'THH_DPQ8D       ','THM_DPQ8D       ',
     > 'THHM_DPQ8D      ','THHM_AXIS_QU8D  ',
     > 'HM_AXIS_QU8D    ','HHM_AXIS_QU8D   ',
     > 'THH_AXIS_QU8D   ','THM_AXIS_QU8D   ',
     > 'THV_DPQ8D       ','THV_AXIS_QU8D   ' /
C
C
C     NTHMF6 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE TRIA6 EN TANT QUE FACE
C     D ELEMENTS 3D
C     THMF6  : NOM DE CES ELEMENTS
C     ISTHF6  : TRUE SI APPARTIENT A CETTE FAMILLE
C
      DATA THMF6 / 
     > 'HHM_FACE6       ',
     > 'HM_FACE6        ',
     > 'THH_FACE6       ',
     > 'THV_FACE6       ',
     > 'THHM_FACE6      ',
     > 'THM_FACE6       '/
C
C     NTHMF8 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE QUAD8 EN TANT QUE FACE
C     D ELEMENTS 3D
C     THMF8  : NOM DE CES ELEMENTS
C     ISTHF8  : TRUE SI APPARTIENT A CETTE FAMILLE
C
      DATA THMF8 / 
     > 'HHM_FACE8       ',
     > 'HM_FACE8        ',
     > 'THH_FACE8       ',
     > 'THV_FACE8       ',
     > 'THHM_FACE8      ',
     > 'THM_FACE8       '/
C
C
C     NHMH10 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE TETRA10 
C     THMH10  : NOM DE CES ELEMENTS
C     ISTH10  : TRUE SI APPARTIENT A CETTE FAMILLE
C
      DATA THMH10 / 
     > 'HHM_TETRA10      ',
     > 'HM_TETRA10       ',
     > 'THH_TETRA10      ',
     > 'THV_TETRA10      ',
     > 'THHM_TETRA10     ',
     > 'THM_TETRA10      ', 
     > 'HHM_TETRA10D     ',
     > 'HM_TETRA10D      ',
     > 'THH_TETRA10D     ',
     > 'THV_TETRA10D     ',
     > 'THHM_TETRA10D    ',
     > 'THM_TETRA10D     '/
C
C
C     NHMH13 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE PYRAM13 
C     THMH13  : NOM DE CES ELEMENTS
C     ISTH13  : TRUE SI APPARTIENT A CETTE FAMILLE
C
      DATA THMH13 / 
     > 'HHM_PYRAM13      ',
     > 'HM_PYRAM13       ',
     > 'THH_PYRAM13      ',
     > 'THV_PYRAM13      ',
     > 'THHM_PYRAM13     ',
     > 'THM_PYRAM13      ',
     > 'HHM_PYRAM13D     ',
     > 'HM_PYRAM13D      ',
     > 'THH_PYRAM13D     ',
     > 'THV_PYRAM13D     ',
     > 'THHM_PYRAM13D    ',
     > 'THM_PYRAM13D     '/
C
C
C     NHMH15 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE PENTA15 
C     THMH15  : NOM DE CES ELEMENTS
C     ISTH15  : TRUE SI APPARTIENT A CETTE FAMILLE
C
      DATA THMH15 / 
     > 'HHM_PENTA15      ',
     > 'HM_PENTA15       ',
     > 'THH_PENTA15      ',
     > 'THV_PENTA15      ',
     > 'THHM_PENTA15     ',
     > 'THM_PENTA15      ', 
     > 'HHM_PENTA15D     ',
     > 'HM_PENTA15D      ',
     > 'THH_PENTA15D     ',
     > 'THV_PENTA15D     ',
     > 'THHM_PENTA15D    ',
     > 'THM_PENTA15D     '/
C
C
C     NHMH20 : NOMBRE  D ELEMENT DE LA FAMILLE THM ET
C     SUPPORTES PAR UNE MAILLE HEXA20 
C     THMH20  : NOM DE CES ELEMENTS
C     ISTH20  : TRUE SI APPARTIENT A CETTE FAMILLE
C
      DATA THMH20 / 
     > 'HHM_HEXA20       ',
     > 'HM_HEXA20        ',
     > 'THH_HEXA20       ',
     > 'THV_HEXA20       ',
     > 'THHM_HEXA20      ',
     > 'THM_HEXA20       ',
     > 'HHM_HEXA20D      ',
     > 'HM_HEXA20D       ',
     > 'THH_HEXA20D      ',
     > 'THV_HEXA20D      ',
     > 'THHM_HEXA20D     ',
     > 'THM_HEXA20D      '/
C
C
       ELLUM(1) = 'HM_DPTR3D       '
       ELLUM(2) = 'HHM_DPTR3D      '
       ELLUM(3) = 'THH_DPTR3D      '
       ELLUM(4) = 'THM_DPTR3D      '
       ELLUM(5) = 'THHM_DPTR3D     '
       ELLUM(6) = 'THHM_AXIS_TR3D  '
       ELLUM(7) = 'HM_AXIS_TR3D    '
       ELLUM(8) = 'HHM_AXIS_TR3D   '
       ELLUM(9) = 'THH_AXIS_TR3D   '
       ELLUM(10) = 'THM_AXIS_TR3D   '
       ELLUM(11) = 'HM_DPQ4D        '
       ELLUM(12) = 'HHM_DPQ4D       '
       ELLUM(13) = 'THH_DPQ4D       '
       ELLUM(14) = 'THM_DPQ4D       '
       ELLUM(15) = 'THHM_DPQ4D      '
       ELLUM(16) = 'THHM_AXIS_QU4D  '
       ELLUM(17) = 'HM_AXIS_QU4D    '
       ELLUM(18) = 'HHM_AXIS_QU4D   '
       ELLUM(19) = 'THH_AXIS_QU4D   '
       ELLUM(20) = 'THM_AXIS_QU4D   '
       ELLUM(21) = 'HM_DPTR6D       '
       ELLUM(22) = 'HHM_DPTR6D      '
       ELLUM(23) = 'THH_DPTR6D      '
       ELLUM(24) = 'THM_DPTR6D      '
       ELLUM(25) = 'THHM_DPTR6D     '
       ELLUM(26) = 'THHM_AXIS_TR6D  '
       ELLUM(27) = 'HM_AXIS_TR6D    '
       ELLUM(28) = 'HHM_AXIS_TR6D   '
       ELLUM(29) = 'THH_AXIS_TR6D   '
       ELLUM(30) = 'THM_AXIS_TR6D   '
       ELLUM(31) = 'HM_DPQ8D        '
       ELLUM(32) = 'HHM_DPQ8D       '
       ELLUM(33) = 'THH_DPQ8D       '
       ELLUM(34) = 'THM_DPQ8D       '
       ELLUM(35) = 'THHM_DPQ8D      '
       ELLUM(36) = 'THHM_AXIS_QU8D  '
       ELLUM(37) = 'HM_AXIS_QU8D    '
       ELLUM(38) = 'HHM_AXIS_QU8D   '
       ELLUM(39) = 'THH_AXIS_QU8D   '
       ELLUM(40) = 'THM_AXIS_QU8D   '
       ELLUM(41) = 'HHM_TETRA10D    '     
       ELLUM(42) = 'HM_TETRA10D     '
       ELLUM(43) = 'THH_TETRA10D    '
       ELLUM(44) = 'THHM_TETRA10D   '
       ELLUM(45) = 'THM_TETRA10D    '
       ELLUM(46) = 'HHM_PYRAM13D    '
       ELLUM(47) = 'HM_PYRAM13D     '
       ELLUM(48) = 'THH_PYRAM13D    '
       ELLUM(49) = 'THHM_PYRAM13D   '
       ELLUM(50) = 'THM_PYRAM13D    '
       ELLUM(51) = 'HHM_PENTA15D    '
       ELLUM(52) = 'HM_PENTA15D     '
       ELLUM(53) = 'THH_PENTA15D    '
       ELLUM(54) = 'THHM_PENTA15D   '
       ELLUM(55) = 'THM_PENTA15D    '
       ELLUM(56) = 'HHM_HEXA20D     '
       ELLUM(57) = 'HM_HEXA20D      '
       ELLUM(58) = 'THH_HEXA20D     '
       ELLUM(59) = 'THHM_HEXA20D    '
       ELLUM(60) = 'THM_HEXA20D     '
C        
       ELLUM(61) = 'THV_DPTR6D      '
       ELLUM(62) = 'THV_AXIS_TR6D   '
       ELLUM(63) = 'THV_DPQ8D       '
       ELLUM(64) = 'THV_AXIS_QU8D   '
       ELLUM(65) = 'THV_TETRA10D    '
       ELLUM(66) = 'THV_PYRAM13D    '
       ELLUM(67) = 'THV_PENTA15D    '
       ELLUM(68) = 'THV_HEXA20D     '
C  
C
      TROUVE  = .FALSE.
      ISTHT3  = .FALSE.
      ISTHQ4  = .FALSE.
      ISTHT6  = .FALSE.
      ISTHQ8  = .FALSE.
      ISTHS2  = .FALSE.
      ISTHS3  = .FALSE.
      ISTH10 = .FALSE.
      ISTH13 = .FALSE.
      ISTH15 = .FALSE.
      ISTH20 = .FALSE.
      ISTHF8 = .FALSE.
      ISTHF6 = .FALSE.
C
      TRAITE = .FALSE.
      P2P1  = .FALSE.
      LUMPED = .FALSE.
C
      IF ( BORD2) THEN
      DO 10 IHM2 = 1 , NTHMS2
       IF (NOMTE.EQ.THMS2(IHM2)) THEN
        ISTHS2 = .TRUE.
        TRAITE = .TRUE.
        GOTO 11
       ENDIF
   10 CONTINUE
   11 CONTINUE
      IF ( .NOT.ISTHS2) THEN
       DO 20 IHM3 = 1 , NTHMS3
        IF (NOMTE.EQ.THMS3(IHM3)) THEN
         ISTHS3 = .TRUE.
         TRAITE = .TRUE.
         GOTO 21
        ENDIF
   20  CONTINUE
   21  CONTINUE
      ENDIF
      ENDIF 
      IF(VOL2) THEN
C
      DO 110 IHM3 = 1 , NTHMT3
       IF (NOMTE.EQ.THMT3(IHM3)) THEN
        ISTHT3 = .TRUE.
        TROUVE = .TRUE.
        TRAITE = .TRUE.
        GOTO 111
       ENDIF
  110 CONTINUE
  111 CONTINUE
      IF ( .NOT.TROUVE) THEN
       DO 120 IHM4 = 1 , NTHMQ4
        IF (NOMTE.EQ.THMQ4(IHM4)) THEN
         ISTHQ4 = .TRUE.
         TROUVE = .TRUE.
         TRAITE = .TRUE.
         GOTO 121
        ENDIF
  120  CONTINUE
  121  CONTINUE
      ENDIF
C
      IF ( .NOT.TROUVE) THEN
       DO 130 IHM6 = 1 , NTHMT6
        IF (NOMTE.EQ.THMT6(IHM6)) THEN
         ISTHT6 = .TRUE.
         TROUVE = .TRUE.
         TRAITE = .TRUE.
         GOTO 131
        ENDIF
  130  CONTINUE
  131  CONTINUE
      ENDIF
C
      IF ( .NOT.TROUVE) THEN
       DO 140 IHM8 = 1 , NTHMQ8
        IF (NOMTE.EQ.THMQ8(IHM8)) THEN
         ISTHQ8 = .TRUE.
         TROUVE = .TRUE.
         TRAITE = .TRUE.
         GOTO 141
        ENDIF
  140  CONTINUE
  141  CONTINUE
      ENDIF
      ENDIF
C
      IF ( VOL3) THEN
       DO 210 IHH10 = 1 , NHMH10
        IF (NOMTE.EQ.THMH10(IHH10)) THEN
         ISTH10 = .TRUE.
              TRAITE = .TRUE.
              TROUVE = .TRUE.
              GOTO 211
        ENDIF
  210  CONTINUE
  211  CONTINUE
       IF ( .NOT.TROUVE) THEN
        DO 220 IHH13 = 1 , NHMH13
         IF (NOMTE.EQ.THMH13(IHH13)) THEN
          ISTH13 = .TRUE.
              TRAITE = .TRUE.
              TROUVE = .TRUE.
              GOTO 221
         ENDIF
  220   CONTINUE
  221   CONTINUE
       ENDIF
       IF ( .NOT.TROUVE) THEN
        DO 230 IHH15 = 1 , NHMH15
         IF (NOMTE.EQ.THMH15(IHH15)) THEN
          ISTH15 = .TRUE.
              TRAITE = .TRUE.
              TROUVE = .TRUE.
              GOTO 231
         ENDIF
  230   CONTINUE
  231   CONTINUE
       ENDIF
       IF ( .NOT.TROUVE) THEN
        DO 240 IHH20 = 1 , NHMH20
         IF (NOMTE.EQ.THMH20(IHH20)) THEN
          ISTH20 = .TRUE.
              TRAITE = .TRUE.
              TROUVE = .TRUE.
              GOTO 241
         ENDIF
  240   CONTINUE
  241   CONTINUE
       ENDIF
      ENDIF 
C    
C
      IF ( BORD3) THEN
       DO 310 IHF8 = 1 , NTHMF8
        IF (NOMTE.EQ.THMF8(IHF8)) THEN
         ISTHF8 = .TRUE.
              TRAITE = .TRUE.
              TROUVE = .TRUE.
              GOTO 311
        ENDIF
  310  CONTINUE
  311  CONTINUE
       IF ( .NOT.TROUVE) THEN
        DO 320 IHF6 = 1 , NTHMF6
         IF (NOMTE.EQ.THMF6(IHF6)) THEN
               ISTHF6 = .TRUE.
               TRAITE = .TRUE.
               TROUVE = .TRUE.
               GOTO 321
         ENDIF
  320   CONTINUE
       ENDIF
  321  CONTINUE
      ENDIF
      
      DO 330 IL = 1 , NLUMP
         IF (NOMTE.EQ.ELLUM(IL)) THEN
          LUMPED = .TRUE.
          GOTO 331
         ENDIF
  330 CONTINUE
  331 CONTINUE
C
C  DEFINITION ALIAS
C
      IF ( .NOT.LUMPED) THEN
       IF(  ISTHT3) THEN
        ALIAS='TR3     '
       ELSEIF(  ISTHQ4) THEN
        ALIAS='QU4     '
       ELSEIF(  ISTHT6) THEN
        ALIAS='TR6     '
       ELSEIF(  ISTHQ8) THEN
        ALIAS='QU8     '
       ELSEIF(  ISTHS2) THEN
        ALIAS='SE2     '
       ELSEIF(  ISTHS3) THEN
        ALIAS='SE3     '   
       ELSEIF(  ISTHF8) THEN
        ALIAS='FACE8   '  
       ELSEIF(  ISTHF6) THEN
        ALIAS='FACE6   '
       ELSEIF(  ISTH10) THEN
        ALIAS ='TETRA10 ' 
       ELSEIF(  ISTH13) THEN
        ALIAS ='PYRAM13 '
       ELSEIF(  ISTH15) THEN
        ALIAS ='PENTA15 '
       ELSEIF(  ISTH20) THEN
        ALIAS ='HEXA20  ' 
       ENDIF
      ELSE
       IF(  ISTHT3) THEN
        ALIAS='TR3D    '
       ELSEIF(  ISTHQ4) THEN
        ALIAS='QU4D    '
       ELSEIF(  ISTHT6) THEN
        ALIAS='TR6D    '
       ELSEIF(  ISTHQ8) THEN
        ALIAS='QU8D    '
       ELSEIF(  ISTHS2) THEN
        ALIAS='SE2     '
       ELSEIF(  ISTHS3) THEN
        ALIAS='SE3     '   
       ELSEIF(  ISTHF8) THEN
        ALIAS='FACE8   '  
       ELSEIF(  ISTHF6) THEN
        ALIAS='FACE6   '
       ELSEIF(  ISTH10) THEN
        ALIAS ='TETRA10D' 
       ELSEIF(  ISTH13) THEN
        ALIAS ='PYRAM13D'
       ELSEIF(  ISTH15) THEN
        ALIAS ='PENTA15D'
       ELSEIF(  ISTH20) THEN
        ALIAS ='HEXA20D ' 
       ENDIF
      ENDIF
C
C  DEFINITION NSOM
C
       IF(  ISTHT3) THEN
        NSOM=3
       ELSEIF(  ISTHQ4) THEN
        NSOM=4
       ELSEIF(  ISTHT6) THEN
        NSOM=3
       ELSEIF(  ISTHQ8) THEN
        NSOM=4
       ELSEIF(  ISTHS2) THEN
        NSOM=2
       ELSEIF(  ISTHS3) THEN
        NSOM=2
       ELSEIF(  ISTHF8) THEN
        NSOM=4
       ELSEIF(  ISTHF6) THEN
        NSOM=3
       ELSEIF(  ISTH10) THEN
        NSOM=4
       ELSEIF(  ISTH13) THEN
        NSOM=5
       ELSEIF(  ISTH15) THEN
        NSOM=6
       ELSEIF(  ISTH20) THEN
        NSOM=8
       ENDIF
C
      AXI = .FALSE.
      DPLAN = .FALSE.
      IF (TRAITE) THEN
       IF ( NOMTE(6:9).EQ.'AXIS'
     >    .OR.NOMTE(4:7) .EQ.'AXIS'
     >    .OR.NOMTE(5:8).EQ.'AXIS')THEN
         AXI = .TRUE.
       ENDIF
       IF ( NOMTE(6:7).EQ.'DP'
     >    .OR.NOMTE(4:5) .EQ.'DP'
     >    .OR.NOMTE(5:6).EQ.'DP')THEN
         DPLAN = .TRUE.
       ENDIF
      ENDIF 
      
      IF (ISTHS3) THEN 
       P2P1 = .TRUE.
C
C     SEG3
C
       VOISIN(1,3) = 1
       VOISIN(2,3) = 2
      ENDIF
C
      IF (ISTHT6.OR.ISTHF6) THEN 
       P2P1 = .TRUE.
       DO 500 IS = 1,NSOM
        NBVOS(IS) = 2
  500  CONTINUE
C
C     TR6
C
       VOISIN(1,1) = 4
       VOISIN(2,1) = 6
       VOISIN(1,2) = 4
       VOISIN(2,2) = 5
       VOISIN(1,3) = 5
       VOISIN(2,3) = 6
C       
       VOISIN(1,4) = 1
       VOISIN(2,4) = 2
       VOISIN(1,5) = 2
       VOISIN(2,5) = 3
       VOISIN(1,6) = 3
       VOISIN(2,6) = 1 
      ENDIF
C
C     QU8
C
      IF (ISTHQ8.OR.ISTHF8) THEN
       P2P1 = .TRUE.
       DO 501 IS = 1,NSOM
        NBVOS(IS) = 2
  501  CONTINUE
       VOISIN(1,1) = 5
       VOISIN(2,1) = 8
       VOISIN(1,2) = 6
       VOISIN(2,2) = 5
       VOISIN(1,3) = 7
       VOISIN(2,3) = 6
       VOISIN(1,4) = 8
       VOISIN(2,4) = 7
C   
       VOISIN(1,5) = 1
       VOISIN(2,5) = 2
       VOISIN(1,6) = 2
       VOISIN(2,6) = 3
       VOISIN(1,7) = 3
       VOISIN(2,7) = 4
       VOISIN(1,8) = 4
       VOISIN(2,8) = 1
      ENDIF
C
C  TETRA 10
C
      IF (ISTH10) THEN
        P2P1 = .TRUE.
       DO 502 IS = 1,NSOM
        NBVOS(IS) = 3
  502  CONTINUE
        VOISIN(1,1) = 5
        VOISIN(2,1) = 7
        VOISIN(3,1) = 8      
        VOISIN(1,2) = 5
        VOISIN(2,2) = 6
        VOISIN(3,2) = 9
        VOISIN(1,3) = 6
        VOISIN(2,3) = 7
        VOISIN(3,3) = 10
        VOISIN(1,4) = 8
        VOISIN(2,4) = 9
        VOISIN(3,4) = 10
 
        VOISIN(1,5) = 1
        VOISIN(2,5) = 2
        VOISIN(1,6) = 2
        VOISIN(2,6) = 3
        VOISIN(1,7) = 3
        VOISIN(2,7) = 1
        VOISIN(1,8) = 1
        VOISIN(2,8) = 4
        VOISIN(1,9) = 4
        VOISIN(2,9) = 2
        VOISIN(1,10) = 4
        VOISIN(2,10) = 3
      ENDIF
C
C     PYRAM13
C
C
C
      IF (ISTH13) THEN
        P2P1 = .TRUE.
       DO 503 IS = 1,4
        NBVOS(IS) = 3
  503  CONTINUE
        NBVOS(5) = 4
        VOISIN(1,1) = 6
        VOISIN(2,1) = 9
        VOISIN(3,1) = 10  
        VOISIN(1,2) = 6
        VOISIN(2,2) = 7
        VOISIN(3,2) = 11
        VOISIN(1,3) = 7
        VOISIN(2,3) = 8
        VOISIN(3,3) = 12
        VOISIN(1,4) = 8
        VOISIN(2,4) = 9
        VOISIN(3,4) = 13
        VOISIN(1,5) = 10
        VOISIN(2,5) = 11
        VOISIN(3,5) = 12
        VOISIN(4,5) = 13
 
        VOISIN(1,6) = 1
        VOISIN(2,6) = 2
        VOISIN(1,7) = 2
        VOISIN(2,7) = 3
        VOISIN(1,8) = 3
        VOISIN(2,8) = 4
        VOISIN(1,9) = 4
        VOISIN(2,9) = 1
        VOISIN(1,10) = 1
        VOISIN(2,10) = 5
        VOISIN(1,11) = 2
        VOISIN(2,11) = 5
        VOISIN(1,12) = 3
        VOISIN(2,12) = 5
        VOISIN(1,13) = 4
        VOISIN(2,13) = 5
      ENDIF
C
C     PENTA15
C
C
C
      IF (ISTH15) THEN
        P2P1 = .TRUE.
       DO 504 IS = 1,NSOM
        NBVOS(IS) = 3
  504  CONTINUE
        VOISIN(1,1) = 7
        VOISIN(2,1) = 9
        VOISIN(3,1) = 10  
        VOISIN(1,2) = 7
        VOISIN(2,2) = 8
        VOISIN(3,2) = 11
        VOISIN(1,3) = 8
        VOISIN(2,3) = 9
        VOISIN(3,3) = 12
        VOISIN(1,4) = 10
        VOISIN(2,4) = 13
        VOISIN(3,4) = 15
        VOISIN(1,5) = 11
        VOISIN(2,5) = 13
        VOISIN(3,5) = 14
        VOISIN(1,6) = 12
        VOISIN(2,6) = 14
        VOISIN(3,6) = 15
 

        VOISIN(1,7)  = 1
        VOISIN(2,7)  = 2
        VOISIN(1,8)  = 2
        VOISIN(2,8)  = 3
        VOISIN(1,9)  = 3
        VOISIN(2,9)  = 1
        VOISIN(1,10) = 1
        VOISIN(2,10) = 4
        VOISIN(1,11) = 2
        VOISIN(2,11) = 5
        VOISIN(1,12) = 3
        VOISIN(2,12) = 6
        VOISIN(1,13) = 4
        VOISIN(2,13) = 5
        VOISIN(1,14) = 5
        VOISIN(2,14) = 6
        VOISIN(1,15) = 6
        VOISIN(2,15) = 4
      ENDIF

C
C     HEXA20
C
C
C
      IF (ISTH20) THEN
       DO 505 IS = 1,NSOM
        NBVOS(IS) = 3
  505  CONTINUE
        P2P1 = .TRUE.
        VOISIN(1,1) = 9
        VOISIN(2,1) = 12
        VOISIN(3,1) = 13       
        VOISIN(1,2) = 10
        VOISIN(2,2) = 9
        VOISIN(3,2) = 14
        VOISIN(1,3) = 11
        VOISIN(2,3) = 10
        VOISIN(3,3) = 15
        VOISIN(1,4) = 12
        VOISIN(2,4) = 11
        VOISIN(3,4) = 16
        VOISIN(1,5) = 17
        VOISIN(2,5) = 20
        VOISIN(3,5) = 13
        VOISIN(1,6) = 18
        VOISIN(2,6) = 17
        VOISIN(3,6) = 14
        VOISIN(1,7) = 19
        VOISIN(2,7) = 18
        VOISIN(3,7) = 15
        VOISIN(1,8) = 20
        VOISIN(2,8) = 19
        VOISIN(3,8) = 16
 
        VOISIN(1,9) =  1
        VOISIN(2,9) =  2
        VOISIN(1,10) = 2
        VOISIN(2,10) = 3
        VOISIN(1,11) = 3
        VOISIN(2,11) = 4
        VOISIN(1,12) = 4
        VOISIN(2,12) = 1
        VOISIN(1,13) = 1
        VOISIN(2,13) = 5
        VOISIN(1,14) = 2
        VOISIN(2,14) = 6
        VOISIN(1,15) = 3
        VOISIN(2,15) = 7
        VOISIN(1,16) = 4
        VOISIN(2,16) = 8
        VOISIN(1,17) = 5
        VOISIN(2,17) = 6
        VOISIN(1,18) = 6
        VOISIN(2,18) = 7
        VOISIN(1,19) = 7
        VOISIN(2,19) = 8
        VOISIN(1,20) = 8
        VOISIN(2,20) = 5
      ENDIF
      END
