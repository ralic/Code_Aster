      SUBROUTINE  PREGMS
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 27/05/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C TOLE CRS_512
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      PREGMS --   INTERFACE GMSH --> ASTER
C                  LECTURE DU FICHIER  .GMSH
C                  ECRITURE DU FICHIER .MAIL
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
C -----  VARIABLES LOCALES
           PARAMETER (MAXNOD=32, NBTYMA=15)
           CHARACTER*4  NOD, CT(3), ELM
           CHARACTER*7  ENDNOD
           CHARACTER*8  NOMAIL(NBTYMA), RQUOI
           CHARACTER*12 AUT
           CHARACTER*14 AUT1
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C ---- INITIALISATIONS
C      ---------------
      RQUOI   = '????????'
C
      DO 10 I = 1, NBTYMA
        NOMAIL(I) = RQUOI
  10  CONTINUE
C
C --- RECUPERATION DES NUMEROS D'UNITE LOGIQUE DES FICHIERS :
C     -----------------------------------------------------
      IMES  = IUNIFI('MESSAGE')
      IGMSH = IUNIFI('GMSH')
      IMOD  = IUNIFI('FICHIER-MODELE')
C
C --- AFFECTATION DE NOMAIL AVEC LE NOM DU TYPE DES ELEMENTS :
C     ------------------------------------------------------
      CALL INIGMS(NOMAIL)
C
C --- LECTURE EN DEBUT DU FICHIER .GMSH DE $NOD :
C     -----------------------------------------
      READ(IGMSH,'(A4)') NOD
C
C --- ECRITURE DU TITRE DANS LE FICHIER .MAIL :
C     ---------------------------------------
      WRITE(IMOD,'(A)') 'TITRE'
C
C --- ECRITURE DE LA DATE DU JOUR :
C     ---------------------------
      CALL JJMMAA(CT,AUT)
      AUT1 = 'INTERFACE_GMSH'
      WRITE(IMOD,'(9X,2A,17X,A,A2,A,A2,A,A4)')'AUTEUR=',AUT1,'DATE=',
     +  CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
      WRITE(IMOD,'(A)') 'FINSF'
      WRITE(IMOD,'(A)') '%'
      WRITE(IMES,*) 'ECRITURE DU TITRE'
C
C --- LECTURE DES NOEUDS ET DE LEURS COORDONNEES DANS LE FICHIER .GMSH:
C     ----------------------------------------------------------------
      CALL GMLNEU(NBNODE)
C
C --- ECRITURE DES NOEUDS ET DE LEURS COORDONNEES DANS LE FICHIER .MAIL:
C     -----------------------------------------------------------------
      CALL GMENEU(NBNODE)
C
C --- FIN DE LA LECTURE DES NOEUDS :
C     ----------------------------
      READ(IGMSH,'(A7)') ENDNOD
C
C --- DEBUT DE LA LECTURE DES ELEMENTS DANS LE FICHIER .GMSH :
C     ------------------------------------------------------
      READ(IGMSH,'(A4)') ELM
C
C --- LECTURE DES MAILLES ET DES GROUP_MA :
C     -----------------------------------
       CALL GMLELT(MAXNOD,NBTYMA,NBMAIL)
C
C --- ECRITURE DES MAILLES ET DES GROUP_MA DANS LE FICHIER .MAIL :
C     ----------------------------------------------------------
       CALL GMEELT(NBTYMA,NOMAIL,NBMAIL)
C
C --- MENAGE :
C     ------
      CALL JEDETR('&&PREGMS.INFO.NOEUDS')
      CALL JEDETR('&&PREGMS.COOR.NOEUDS')
      CALL JEDETR('&&PREGMS.NUMERO.MAILLES')
      CALL JEDETR('&&PREGMS.TYPE.MAILLES')
      CALL JEDETR('&&PREGMS.GROUPE.MAILLES')
      CALL JEDETR('&&PREGMS.NBNO.MAILLES')
      CALL JEDETR('&&PREGMS.CONNEC.MAILLES')
      CALL JEDETR('&&PREGMS.NBMA.GROUP_MA')
      CALL JEDETR('&&PREGMS.NBTYP.MAILLES')
      CALL JEDETR('&&PREGMS.LISTE.GROUP_MA')
      CALL JEDETR('&&PREGMS.INDICE.GROUP_MA')
      CALL JEDETR('&&PREGMS.GRMA.MAILLES')
C
C.============================ FIN DE LA ROUTINE ======================
      END
