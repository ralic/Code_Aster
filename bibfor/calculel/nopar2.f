      CHARACTER*8 FUNCTION NOPAR2(NOMOPT,NOMGD,STATUT)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 18/03/2003   AUTEUR VABHHTS J.PELLET 
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
C RESPONSABLE                            VABHHTS J.PELLET
      CHARACTER*(*)  NOMOPT,NOMGD,STATUT
C ----------------------------------------------------------------------
C     ENTREES:
C     NOMOPT   : NOM D'1 OPTION
C     NOMGD    : NOM D'1 GRANDEUR
C     STATUT : 'IN'/'OUT'

C     SORTIES:
C     NOPAR2 : NOM DU PARAMETRE DE L'OPTION NOMOPT
C              QUI CORRESPOND A LA GRANDEUR NOMGD ET AU STATUT STATUT
C              RQUE : ERREUR <F> SI ON EN TROUVE : 0,2,3,...

C ----------------------------------------------------------------------


C     VARIABLES LOCALES:
C     ------------------
      INTEGER OPT,NBIN,NBOUT,NBTROU,ITROU,GD,GD2 ,IADESC,IAOPPA,KK
      CHARACTER*16 NOMOP2
      CHARACTER*8  NOMGD2
      CHARACTER*3  STATU2
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM,JEXNOM
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI

C DEB-------------------------------------------------------------------
      NOMOP2=NOMOPT
      NOMGD2=NOMGD
      STATU2=STATUT

      CALL JENONU(JEXNOM('&CATA.OP.NOMOPT',NOMOP2),OPT)
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',NOMGD2),GD)
      CALL JEVEUO(JEXNUM('&CATA.OP.DESCOPT',OPT),'L',IADESC)
      CALL JEVEUO(JEXNUM('&CATA.OP.OPTPARA',OPT),'L',IAOPPA)
      NBIN = ZI(IADESC-1+2)
      NBOUT = ZI(IADESC-1+3)
      NBTROU=0


      IF (STATU2.EQ.'OUT') THEN
        DO 1,KK=1,NBOUT
          GD2 = ZI(IADESC-1+4+NBIN+KK)
          IF (GD.EQ.GD2) THEN
            NBTROU=NBTROU+1
            ITROU=KK
          END IF
1       CONTINUE

      ELSE IF (STATU2.EQ.'IN') THEN
        DO 2,KK=1,NBIN
          GD2 = ZI(IADESC-1+4+KK)
          IF (GD.EQ.GD2) THEN
            NBTROU=NBTROU+1
            ITROU=KK
          END IF
2       CONTINUE

      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      IF (NBTROU.EQ.0) CALL UTMESS('F','NOPAR2','IL N Y A PAS '//
     &      'DE PARAMETRE '//STATU2//' ASSOCIE A LA GRANDEUR:'
     &                             //NOMGD2//' DANS '//'L OPTION:'//
     &                             NOMOP2)
      IF (NBTROU.GT.1) CALL UTMESS('F','NOPAR2','IL Y A PLUSIEURS '//
     &       'PARAMETRES '//STATU2//' ASSOCIES A LA GRANDEUR:'
     &                             //NOMGD2//' DANS '//'L OPTION:'//
     &                             NOMOP2)

      IF (STATU2.EQ.'OUT') THEN
        NOPAR2=ZK8(IAOPPA-1+NBIN+ITROU)

      ELSE IF (STATU2.EQ.'IN') THEN
        NOPAR2=ZK8(IAOPPA-1+ITROU)

      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      END
