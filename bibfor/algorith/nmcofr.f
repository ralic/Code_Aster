      SUBROUTINE NMCOFR (NOMA,DEPPLU,DEPDEL,DDEPLA,DEFICO,
     &                   RESOCO,CNCINE,ITERAT,INST,CONV,LICCVG,LREAC)
C
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/11/2004   AUTEUR MABBAS M.ABBAS 
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
      IMPLICIT     NONE
      CHARACTER*8  NOMA
      CHARACTER*24 DEPPLU
      CHARACTER*24 DEPDEL
      CHARACTER*24 DDEPLA      
      CHARACTER*24 DEFICO 
      CHARACTER*24 RESOCO
      CHARACTER*24 CNCINE
      INTEGER      ITERAT
      REAL*8       INST
      REAL*8       CONV(*)
      INTEGER      LICCVG(*)
      LOGICAL      LREAC(4)
C
C ======================================================================
C ROUTINE APPELEE PAR : NMDEPL
C ======================================================================
C
C TRAITEMENT DU CONTACT AVEC OU SANS FROTTEMENT DANS STAT_NON_LINE.
C BRANCHEMENT SUR LES ROUTINES DE RESOLUTION.
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEPPLU : CHAMP DE DEPLACEMENTS A L'ITERATION DE NEWTON PRECEDENTE
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C IN  DEPPLA : INCREMENT DE DEPLACEMENTS CALCULE EN IGNORANT LE CONTACT
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  CNCINE : CHAM_NO CINEMATIQUE
C IN  ITERAT : ITERATION DE NEWTON
C IN  INST   : VALEUR DE L'INSTANT DE CALCUL
C IN  CONV   : INFORMATIONS SUR LA CONVERGENCE DU CALCUL
C                     1 - RESI_DUAL_ABSO      (LAGRANGIEN AUGMENTE)
C                     2 - RESI_PRIM_ABSO      (LAGRANGIEN AUGMENTE)
C                     3 - NOMBRE D'ITERATIONS DUAL (LAGRANGIEN AUGMENTE)
C                     4 - NUMERO ITERATION BFGS (LAGRANGIEN AUGMENTE)
C                    10 - NOMBRE D'ITERATIONS (RECHERCHE LINEAIRE)
C                    11 - RHO                 (RECHERCHE LINEAIRE)
C                    20 - RESI_GLOB_RELA
C                    21 - RESI_GLOB_MAXI
C OUT LICCVG : CODES RETOURS D'ERREUR
C                       (1) PILOTAGE
C                       (2) LOI DE COMPORTEMENT
C                       (3) CONTACT/FROTTEMENT: NOMBRE MAXI D'ITERATIONS
C                       (4) CONTACT/FROTTEMENT: MATRICE SINGULIERE
C I/O LREAC  : (1) = TRUE  SI REACTUALISATION A FAIRE  
C              (2) = TRUE  SI ATTENTE POINT FIXE CONTACT
C              (3) = TRUE  SI METHODE CONTINUE
C              (4) = TRUE  SI MODELISATION DU CONTACT
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      LOGICAL      PREMIE
      INTEGER      ICONTA,JPREM
      INTEGER      IFM,NIV
      REAL*8       TPS1(4),TPS2(4),TPSGEO,TPSALG
C
C ----------------------------------------------------------------------
C
      CALL INFNIV(IFM,NIV)
      CALL JEMARQ()
C
C --- TRAITEMENT DU CONTACT ?
C
      CALL JEEXIN ( RESOCO(1:14)//'.APREAC', ICONTA )

      IF (ICONTA.EQ.0) THEN
         GOTO 999
      ENDIF

      IF (NIV.GE.2) THEN
         WRITE (IFM,*) '<CONTACT> *** DEBUT DU TRAITEMENT *** <CONTACT>'
      ENDIF
C
C --- PREMIERE UTILISATION DU CONTACT OU NON
C
      CALL JEVEUO ( RESOCO(1:14) // '.PREM', 'E', JPREM )

      PREMIE = ZL(JPREM)
      IF ( PREMIE )  THEN 
        ZL(JPREM) = .FALSE.
      ENDIF
C
C ======================================================================
C --- TIMING CONTACT
C ======================================================================
C
      CALL UTTCPU (10,'INIT',4,TPS1)
      CALL UTTCPU (20,'INIT',4,TPS2)
C
C ======================================================================
C --- REACTUALISATION GEOMETRIQUE
C ======================================================================
C
      CALL UTTCPU (10,'DEBUT',4,TPS1)

      CALL CFGEOM(PREMIE,LREAC(1),ITERAT,INST,
     &            NOMA,DEFICO,RESOCO,
     &            DEPPLU,DEPDEL)

      CALL UTTCPU (10,'FIN',4,TPS1)

      TPSGEO = TPS1(4)
C
C ======================================================================
C     PREPARATION DES DONNEES POUR CONTACT EN THM (PRESSION/TEMPERATURE)
C ======================================================================
C
      CALL CFTHM(DEFICO,RESOCO)
C
C ======================================================================
C     ALGORITHMES DE CONTACT
C ======================================================================
C
      CALL UTTCPU (20,'DEBUT',4,TPS2)
      CALL CFALGO(NOMA,ITERAT,CONV,
     &            DEFICO,RESOCO,
     &            DEPPLU,DDEPLA,DEPDEL,CNCINE,
     &            LICCVG,LREAC)
      CALL UTTCPU (20,'FIN',4,TPS2)

      TPSALG = TPS2(4)
C
      IF (NIV.GE.2) THEN
         WRITE (IFM,*) '<CONTACT> TEMPS CPU POUR GEOMETRIE : ',TPSGEO
         WRITE (IFM,*) '<CONTACT> TEMPS CPU POUR ALGORITHME: ',TPSALG
         WRITE (IFM,*) '<CONTACT> *** FIN DU TRAITEMENT *** <CONTACT>'
      ENDIF
C
  999 CONTINUE
C
      CALL JEDEMA()
      END
