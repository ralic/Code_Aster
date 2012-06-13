      SUBROUTINE COENEU(IMOD,NBNODE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GREFFET N.GREFFET
      IMPLICIT NONE
C
C      COENEU --   ECRITURE DES NUMEROS DE NOEUDS ET DE LEURS
C                  COORDONNEES VENANT D'UN FICHIER .GMSH DANS
C                  UN FICHIER .MAIL
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NBNODE         IN    I         NOMBRE DE NOEUDS DU MAILLAGE
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      INCLUDE 'jeveux.h'
           INTEGER  IMOD,NBNODE
C -----  VARIABLES LOCALES
           INTEGER      JINFO, JDETR, JCOOR, INODE, NODE
           CHARACTER*1  PRFNOE
           CHARACTER*4  CT(3)
           CHARACTER*8  CHNODE
           CHARACTER*12 CHENTI, AUT
           CHARACTER*80 CHFONE
           LOGICAL      DIM3D
           REAL*8       ZCTE, X, Y, Z
C
C
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
C --- DATE :
C     ----
      CALL JJMMAA(CT,AUT)
C
C --- RECUPERATION DES VECTEURS DE TRAVAIL :
C     ------------------------------------
      CALL JEVEUO('&&PRECOU.INFO.NOEUDS','L',JINFO)
      CALL JEVEUO('&&PRECOU.DETR.NOEUDS','L',JDETR)
      CALL JEVEUO('&&PRECOU.COOR.NOEUDS','L',JCOOR)
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
        NODE = ZI(JINFO+INODE-1)

C      ON N'ECRIT PAS LES NOEUDS ORPHELINS
        IF (ZI(JDETR+NODE).EQ.0) GOTO 20

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
