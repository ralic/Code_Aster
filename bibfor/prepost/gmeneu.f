      SUBROUTINE  GMENEU(NBNODE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/08/2003   AUTEUR DURAND C.DURAND 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      GMENEU --   ECRITURE DES NUMEROS DE NOEUDS ET DE LEURS
C                  COORDONNEES VENANT D'UN FICHIER .GMSH DANS
C                  UN FICHIER .MAIL
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NBNODE         IN    I         NOMBRE DE NOEUDS DU MAILLAGE
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           INTEGER  NBNODE
C -----  VARIABLES LOCALES
           CHARACTER*1  PRFNOE
           CHARACTER*4  CT(3)
           CHARACTER*8  CHNODE
           CHARACTER*12 CHENTI, AUT
           CHARACTER*80 CHFONE
           LOGICAL      DIM3D
           REAL*8       ZCTE
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER            ZI
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
      CHARACTER*32 JEXNOM, JEXNUM
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C.========================= DEBUT DU CODE EXECUTABLE ==================
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      CHNODE   = '        '
      PRFNOE   = 'N'
      CHFONE   = '%FORMAT=(1*NOM_DE_NOEUD,3*COORD)'
      CHENTI   = 'NBOBJ=      '
      CALL CODENT(NBNODE,'G',CHENTI(7:12))
C
C --- NUMERO D'UNITE LOGIQUE ASSOCIE AU FICHIER ASTER :
C     -----------------------------------------------
      IMOD = IUNIFI('FICHIER-MODELE')
C
C --- DATE :
C     ----
      CALL JJMMAA(CT,AUT)
C
C --- RECUPERATION DES VECTEURS DE TRAVAIL :
C     ------------------------------------
      CALL JEVEUO('&&PREGMS.INFO.NOEUDS','L',JINFO)
      CALL JEVEUO('&&PREGMS.COOR.NOEUDS','L',JCOOR)
C
      CALL CODNOP(CHNODE,PRFNOE,1,1)
C
C --- TEST SI MAILLAGE 2D OU 3D :
C     ------------------------------------------------------
      DIM3D = .FALSE.
      ZCTE  = ZR(JCOOR-1+3)
      DO 10 INODE = 2, NBNODE
        IF (ZR(JCOOR-1+3*(INODE-1)+3).NE.ZCTE) THEN
            DIM3D=.TRUE.
            GOTO 10
        ENDIF
  10  CONTINUE
C
      IF (DIM3D) THEN
         WRITE(IMOD,'(A,4X,A)')'COOR_3D',CHENTI
      ELSE
         WRITE(IMOD,'(A,4X,A)')'COOR_2D',CHENTI
      ENDIF
C      WRITE(IMOD,'(11X,2A,12X,A,A2,A,A2,A,A4)')
C     +'AUTEUR=',AUT,'DATE=',CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
C
      WRITE(IMOD,'(A)') CHFONE
C
C --- ECRITURE DES NUMEROS DE NOEUDS ET DE LEURS COORDONNEES :
C     ------------------------------------------------------
      DO 20 INODE = 1, NBNODE
C
        NODE = ZI(JINFO+INODE-1)
        X    = ZR(JCOOR-1+3*(INODE-1)+1) 
        Y    = ZR(JCOOR-1+3*(INODE-1)+2) 
        Z    = ZR(JCOOR-1+3*(INODE-1)+3) 
C
        CALL CODENT(NODE,'G',CHNODE(2:8))
        IF (DIM3D) THEN
          WRITE(IMOD,'(2X,A,2X,3(1PE21.14),1X)') CHNODE,X,Y,Z
        ELSE
          WRITE(IMOD,'(2X,A,2X,2(1PE21.14),1X)') CHNODE,X,Y
        ENDIF
C
  20  CONTINUE
C
      WRITE(IMOD,'(A)') 'FINSF'
      WRITE(IMOD,'(A)') '%'
C
      CALL JEDEMA()
C
C.============================ FIN DE LA ROUTINE ======================
      END
