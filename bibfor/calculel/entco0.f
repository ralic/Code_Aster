      SUBROUTINE ENTCO0 ( MODE, IADM1, M1 )
      IMPLICIT   NONE
      INTEGER    MODE, IADM1, M1
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/01/2005   AUTEUR CIBHHLV L.VIVAN 
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
C-----------------------------------------------------------------------
C IN  : MODE   : MODE_LOCAL DE TYPE CHNO,VECT,OU MATR.
C OUT : IADM1  : ADRESSE
C OUT : M1     : ADRESSE
C
C-----------------------------------------------------------------------
C ----- COMMUNS NORMALISES  JEVEUX
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C-----------------------------------------------------------------------
C
      INTEGER      IADM, M2, CODE, CODE1
      CHARACTER*8  K8B1, K8B2, K8B3, K8B4
C
      CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',MODE),'L',IADM)

      CODE = ZI(IADM)

      IF (CODE.GT.3) THEN
        IF (CODE.EQ.4) THEN
          M1 = ZI(IADM+3)

        ELSE IF (CODE.EQ.5) THEN
          M1 = ZI(IADM+3)
          M2 = ZI(IADM+4)
          IF (M1.NE.M2) THEN
            CALL CODENT ( M1, 'D', K8B1 )
            CALL CODENT ( M2, 'D', K8B2 )
            CALL UTMESS('F',' ENTCOD', ' MODE LIGNE '//K8B1//
     +                                 ' /= MODE COLONNE '//K8B2)
          END IF
        END IF

        CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',M1),'L',IADM1)
        CODE1 = ZI(IADM1)

        IF (CODE1.GT.3) THEN
          CALL CODENT ( MODE , 'D', K8B1 )
          CALL CODENT ( CODE , 'D', K8B2 )
          CALL CODENT ( M1   , 'D', K8B3 )
          CALL CODENT ( CODE1, 'D', K8B4 )
          CALL UTMESS('F',' ENTCOD 2',' LE MODE '//K8B1//' DE CODE '//
     +            K8B2//' REFERENCE LE MODE '//K8B3//
     +            ' DONT LE CODE : '//K8B4//' > 3 ')
        END IF

      ELSE
        IADM1 = IADM
        M1 = MODE
      END IF
C
      END
