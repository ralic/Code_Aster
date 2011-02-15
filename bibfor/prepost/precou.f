      SUBROUTINE PRECOU(IMOD,TYPEMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/02/2011   AUTEUR GREFFET N.GREFFET 
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
C RESPONSABLE GREFFET N.GREFFET
C.======================================================================
      IMPLICIT NONE
      INTEGER      IMOD
      CHARACTER*8  TYPEMA
C
C      PRECOU --   INTERFACE COUPLAGE --> ASTER
C                  LECTURE DU MAILLAGE .COUP PAR RECEPTION MEMOIRE
C                  ECRITURE DU FICHIER .MAIL
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  VARIABLES LOCALES
C      PARAMETER (MAXNOD=32, NBTYMA=15)
      INTEGER      MAXNOD,NBTYMA,I,NBNODE,IUNIFI,NBMAIL
      PARAMETER    (MAXNOD=32,NBTYMA=19)
      INTEGER      NBNOMA(NBTYMA), NUCONN(NBTYMA,MAXNOD),IMES
      CHARACTER*4  CT(3)
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
      CALL JEMARQ()
      RQUOI   = '????????'
C
      DO 10 I = 1, NBTYMA
        NOMAIL(I) = RQUOI
  10  CONTINUE
C
C --- RECUPERATION DES NUMEROS D'UNITE LOGIQUE DES FICHIERS :
C     -----------------------------------------------------
      IMES  = IUNIFI('MESSAGE')
C
C --- AFFECTATION DE NOMAIL AVEC LE NOM DU TYPE DES ELEMENTS :
C     ------------------------------------------------------
      CALL INIGMS(NOMAIL,NBNOMA,NUCONN)
C
C --- ECRITURE DU TITRE DANS LE FICHIER .MAIL :
C     ---------------------------------------
      WRITE(IMOD,'(A)') 'TITRE'
C
C --- ECRITURE DE LA DATE DU JOUR :
C     ---------------------------
      CALL JJMMAA(CT,AUT)
      AUT1 = 'INTERFACE_YACS'
      WRITE(IMOD,'(9X,2A,17X,A,A2,A,A2,A,A4)')'AUTEUR=',AUT1,'DATE=',
     &  CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
      WRITE(IMOD,'(A)') 'FINSF'
      WRITE(IMOD,'(A)') '%'
      WRITE(IMES,*) 'ECRITURE DU TITRE'
C
C --- LECTURE DES NOEUDS ET DE LEURS COORDONNEES PAR RECEPTION MEMOIRE :
C     ------------------------------------------------------------------
      CALL COLNEU(NBNODE,TYPEMA)
C
C --- LECTURE DES MAILLES ET DES GROUP_MA :
C     -----------------------------------
CFH
C=>
C      CALL COLELT(NBNODE, JGROMA, MAXNOD,NBTYMA,NBMAIL,NBNOMA,NUCONN)
      CALL COLELT(NBNODE, MAXNOD, NBTYMA, NBMAIL, NBNOMA, NUCONN)
C<=
CFH
C
C --- ECRITURE DES NOEUDS ET DE LEURS COORDONNEES DANS LE FICHIER .MAIL:
C     -----------------------------------------------------------------
      CALL COENEU(IMOD,NBNODE)
C
C --- ECRITURE DES MAILLES ET DES GROUP_MA DANS LE FICHIER .MAIL :
C     ----------------------------------------------------------
      CALL COEELT(IMOD,NBTYMA,NOMAIL,NBNOMA,NUCONN,NBMAIL)
C
C --- MENAGE :
C     ------
      CALL JEDETR('&&PRECOU.INFO.NOEUDS')
      CALL JEDETR('&&PRECOU.COOR.NOEUDS')
      CALL JEDETR('&&PRECOU.NUMERO.MAILLES')
      CALL JEDETR('&&PRECOU.TYPE.MAILLES')
      CALL JEDETR('&&PRECOU.GROUPE.MAILLES')
      CALL JEDETR('&&PRECOU.NBNO.MAILLES')
      CALL JEDETR('&&PRECOU.CONNEC.MAILLES')
      CALL JEDETR('&&PRECOU.NBMA.GROUP_MA')
      CALL JEDETR('&&PRECOU.NBTYP.MAILLES')
      CALL JEDETR('&&PRECOU.LISTE.GROUP_MA')
      CALL JEDETR('&&PRECOU.INDICE.GROUP_MA')
      CALL JEDETR('&&PRECOU.GRMA.MAILLES')
C
C.============================ FIN DE LA ROUTINE ======================
      CALL JEDEMA()
      END
