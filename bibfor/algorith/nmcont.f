      SUBROUTINE NMCONT (PREMIE,INST,NOMA,DEPPLU,DDEPLA,DEFICO,RESOCO,
     &                   LMAT,LDSCON,CNNUL,ITERAT,LICCVG,CONV,DEPDEL,
     &                   LREAC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/09/2002   AUTEUR PABHHHH N.TARDIEU 
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
C
      IMPLICIT NONE
C
      LOGICAL      PREMIE, LREAC(4)
      INTEGER      LMAT,LDSCON,ITERAT, LICCVG(*)
      REAL*8       INST,CONV(*)
      CHARACTER*8  NOMA
      CHARACTER*24 DEPPLU,DDEPLA,DEFICO,RESOCO,CNNUL,DEPDEL
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : NMCOFR
C ----------------------------------------------------------------------
C
C TRAITEMENT DU CONTACT DANS STAT_NON_LINE :
C   - APPARIEMENT NOEUD ESCLAVE - MAILLE (OU NOEUD) MAITRE
C   - PROJECTION (DDLS MAITRES, COEFFICIENTS, JEU)
C   - RESOLUTION PAR LA METHODE DES CONTRAINTES ACTIVES
C
C IN  PREMIE : VAUT .TRUE. SI C'EST LE PREMIER CALCUL (PAS DE PASSE)
C IN  INST   : VALEUR DE L'INSTANT DE CALCUL
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEPPLU : CHAMP DE DEPLACEMENTS A L'ITERATION DE NEWTON PRECEDENTE
C IN  DDEPLA : INCREMENT DE DEPLACEMENTS CALCULE EN IGNORANT LE CONTACT
C VAR DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C VAR RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  LDSCON : DESCRIPTEUR DE LA MATR_ASSE DE CONTACT
C IN  CNNUL  : CHAM_NO CINEMATIQUE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32       JEXNUM , JEXNOM
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      LOGICAL      GCPC
      INTEGER      ICONTA,JDIM,JMETH,JCHAM,JREAC,IZONE,NZOCO,POSNO
      INTEGER      JZOCO,JAPPAR,JCMU,IESCL,NESCL,JSLVK,IFM,NIV,ISTO
      REAL*8       ALPHA
      CHARACTER*19 SOLVEU
      CHARACTER*24 OLDGEO,NEWGEO,NDIMCO,METHCO,CHAMCO,APREAC,COEFMU
      CHARACTER*24 APPARI,NOZOCO
C
C ----------------------------------------------------------------------
C
      CALL INFNIV (IFM,NIV)
      CALL JEMARQ ()
C
C --- RECONNAISSANCE DU SOLVEUR 
      SOLVEU = '&&OP0070.SOLVEUR'
      CALL JEVEUO(SOLVEU//'.SLVK','L',JSLVK)
      GCPC = (ZK24(JSLVK-1+1).EQ.'GCPC')
C
C --- FAUT-IL TRAITER LE CONTACT  ?
C
      APREAC = RESOCO(1:14)//'.APREAC'
      CALL JEEXIN (APREAC,ICONTA)
      IF (ICONTA.EQ.0) GO TO 9999
C ======================================================================
C      DETERMINATION DES REACTUALISATIONS A FAIRE DANS CHAQUE ZONE
C ======================================================================
      NDIMCO = DEFICO(1:16)//'.NDIMCO'
      METHCO = DEFICO(1:16)//'.METHCO'
      CHAMCO = DEFICO(1:16)//'.CHAMCO'
      CALL JEVEUO (NDIMCO,'L',JDIM)
      CALL JEVEUO (METHCO,'L',JMETH)
      CALL JEVEUO (CHAMCO,'L',JCHAM)
      CALL JEVEUO (APREAC,'E',JREAC)
      NZOCO  = ZI(JDIM+1)
      DO 10 IZONE = 1,NZOCO
        CALL REACCO (PREMIE,ZI(JMETH+9*(IZONE-1)+1),ZI(JCHAM+IZONE-1),
     &               ZI(JREAC+4*(IZONE-1)))
 10   CONTINUE
C ======================================================================
C                     ACTUALISATION DE LA GEOMETRIE
C ======================================================================
      OLDGEO = NOMA // '.COORDO'
      NEWGEO = '&&NMCONT.ACTUGEO'
      ALPHA = 1.0D0
C ======================================================================
C --- REACTUALISATION ? ------------------------------------------------
C ======================================================================
      IF (LREAC(1)) THEN
         IF(NIV.EQ.2) THEN
            WRITE (IFM,1010)'REACTUALISATION GEOMETRIQUE' 
         ENDIF
C ======================================================================
C --- REACTUALISATION GEOMETRIQUE --------------------------------------
C ======================================================================
         CALL VTGPLD (OLDGEO, ALPHA, DEPPLU, 'V', NEWGEO )
C ======================================================================
C         APPARIEMENT NOEUD ESCLAVE - MAILLE (OU NOEUD) MAITRE
C ======================================================================
         CALL RECHCO (PREMIE,LREAC,INST,NOMA,NEWGEO,DEFICO,RESOCO)
C ======================================================================
C     PROJECTION (NOEUDS MAITRES, DDLS MAITRES, COEFFICIENTS, JEU)
C ======================================================================
         CALL PROJCO (INST,NOMA,NEWGEO,DEFICO,RESOCO)
C ======================================================================
C --- SAUVEGARDE DU JEU EN DEBUT DE PAS DE TEMPS EN CAS DE REDECOUPAGE -
C ======================================================================
         CALL SOVJEU (RESOCO)
      ELSE
         IF (ITERAT.EQ.0) THEN
            CALL REAJEU(RESOCO,DEPPLU,DEPDEL,LMAT)
         ENDIF
      ENDIF
C ======================================================================
C AFFECTATION DU TABLEAU COEFMU QUI SERVIRA DANS LA ROUTINE ALGOCO
C DANS LE CAS DE CONDITION UNILATERALE SUR LA PRESSION OU LA TEMPERATURE
C POUR DES ELEMENTS THM, LE MULTIPLICATEUR MU OBTENU DANS ALGOCO DOIT
C ETRE MULTIPLIE PAR -1 POUR ETRE COHERENT AVEC LA MULTIPLICATION PAR
C -1 DE L'EQUATION HYDRAULIQUE ET DE L'EQUATION THERMIQUE DU PB COUPLE
C ======================================================================
      NOZOCO = DEFICO(1:16)//'.NOZOCO'
      APPARI = RESOCO(1:14)//'.APPARI'
      COEFMU = RESOCO(1:14)//'.COEFMU'
C
      CALL JEVEUO (NOZOCO,'L',JZOCO)
      CALL JEVEUO (APPARI,'L',JAPPAR)
      CALL JEVEUO (COEFMU,'E',JCMU)
C
      NESCL = ZI(JAPPAR)
      DO 20 IESCL = 1,NESCL
        POSNO = ZI(JAPPAR+3*(IESCL-1)+1)
        IZONE = ZI(JZOCO+POSNO-1)
        ZR(JCMU+IESCL-1) = 1.D0
        IF ((ZI(JCHAM+IZONE-1).EQ.-2).OR.(ZI(JCHAM+IZONE-1).EQ.-3)) THEN
          ZR(JCMU+IESCL-1) = -1.D0
        END IF
 20   CONTINUE
C
C ======================================================================
C           RESOLUTION PAR LA METHODE DES CONTRAINTES ACTIVES
C ======================================================================
C
      IF(ZI(JMETH+6).NE.0.AND.ZI(JMETH+6).NE.1) THEN
        IF(GCPC) THEN
               CALL UTDEBM('F','NMCOFR','ERREUR DONNEE')
               CALL UTIMPK('L','POUR DU "FROTTEMENT", LA METHODE DU '//
     &                         'SOLVEUR NE PEUT PAS ETRE ',1,'GCPC')
               CALL UTFINM
        ENDIF
      ENDIF
C ======================================================================
C           INITIALISATION POUR LA DETERMINATION DE POINTS FIXE
C ======================================================================
      LREAC(2) = .FALSE.
C ======================================================================
C           ARRET OU NON SI MATRICE DE CONTACT SINGULIERE
C ======================================================================
      ISTO = ZI(JMETH+9)
C ======================================================================
      IF(ZI(JMETH+6).EQ.-1) THEN
         CALL ALGOCP(DEFICO,RESOCO,LMAT,LDSCON,NOMA,CNNUL,DDEPLA,
     &        DEPPLU,ITERAT,LREAC,CONV,DEPDEL)    
      ELSE IF(ZI(JMETH+6).EQ.0) THEN
         CALL ALGOCO(DEFICO,RESOCO,LMAT,LDSCON,NOMA,CNNUL,DDEPLA,
     &   DEPPLU,LICCVG,ISTO)
      ELSE IF(ZI(JMETH+6).EQ.1) THEN
         CALL ALGOCL(DEFICO,RESOCO,LMAT,LDSCON,NOMA,CNNUL,DDEPLA,
     &        DEPPLU,ITERAT,LREAC,ISTO)
      ELSE IF(ZI(JMETH+6).EQ.2) THEN
         CALL FRO2GD(DEFICO,RESOCO,LMAT,LDSCON,NOMA,CNNUL,DDEPLA,
     &        DEPPLU,ITERAT,LREAC,DEPDEL,ISTO)
      ELSE IF(ZI(JMETH+6).EQ.3) THEN
         CALL FROPGD(DEFICO,RESOCO,LMAT,LDSCON,NOMA,CNNUL,DDEPLA,
     &        DEPPLU,ITERAT,LREAC,CONV,DEPDEL,ISTO)
      ELSE IF(ZI(JMETH+6).EQ.4) THEN
         CALL FROLGD(DEFICO,RESOCO,LMAT,LDSCON,NOMA,CNNUL,DDEPLA,
     &        DEPPLU,ITERAT,LREAC,CONV,DEPDEL,ISTO)
      ELSE IF(ZI(JMETH+6).EQ.5) THEN
         CALL FROGDP(DEFICO,RESOCO,LMAT,LDSCON,NOMA,CNNUL,DDEPLA,
     &        DEPPLU,ITERAT,LREAC,CONV,DEPDEL)
      ENDIF
C
 9999 CONTINUE
C
C ----------------------------------------------------------------------
C
      CALL JEDEMA ()
C
C ======================================================================
C
 1000 FORMAT ('<CONTACT_2> ',A9,A8,A14,A14,A8)
 1010 FORMAT ('<CONTACT_3> ',A27,I5,A3,E10.3)
C
      END
