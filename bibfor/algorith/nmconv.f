      SUBROUTINE NMCONV (CNRESI, CNDIRI, CNFEXT, CNVCFO, PARCRI,
     &                   ITERAT, ETA   , CONV  , LICCVG, ITEMAX,
     &                   CONVER, ECHLDC, ECHEQU, ECHCON, FINPAS, 
     &                   CRITNL, NUMINS, FOINER, PARTPS, PARMET,
     &                   NEQ,    DEPDEL, AUTOC1, AUTOC2, VECONT,
     &                   LREAC,  CNVFRE)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/03/2003   AUTEUR JMBHH01 J.M.PROIX 
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
C TOLE CRP_21
C RESPONSABLE PBADEL P.BADEL
C ======================================================================
      IMPLICIT      NONE
      LOGICAL       ITEMAX, CONVER, ECHLDC, FINPAS, ECHCON(2), ECHEQU
      LOGICAL       LREAC(4)
      INTEGER       ITERAT, LICCVG(*), NUMINS, NEQ, VECONT(2)
      REAL*8        ETA, CONV(*), PARCRI(*), PARMET(*)
      CHARACTER*19  CRITNL, CNRESI, CNDIRI, CNFEXT, CNVCFO, CNVFRE
      CHARACTER*19  FOINER, PARTPS, AUTOC1, AUTOC2
      CHARACTER*24  DEPDEL
C ----------------------------------------------------------------------
C
C     COMMANDE STAT_NON_LINE : VERIFICATION DES CRITERES D'ARRET
C
C ----------------------------------------------------------------------
C IN       CNRESI  : FINT + BT.LAMBDA
C IN       CNDIRI  : BT.LAMBDA
C IN       CNFEXT  : RESULTANTE DES EFFORTS EXTERIEURS
C IN       CNVCFO  : FORCE DE REFERENCE DES VARI_COM
C IN       PARCRI  : CRITERES DE CONVERGENCE
C                     1 : ITER_GLOB_MAXI
C                     2 : RESI_GLOB_RELA
C                     3 : RESI_GLOB_MAXI
C                     4 : ARRET
C                     5 : ITER_GLOB_ELAS
C                     6 : RESI_REFE_RELA
C                    10 : RESI_PRIM_ABSO (LAGRANGIEN)
C                    11 : RESI_DUAL_ABSO (LAGRANGIEN)
C IN       ITERAT  : NUMERO D'ITERATION
C IN       ETA     : COEFFICIENT DE PILOTAGE
C IN       CONV    : INFORMATIONS SUR LA CONVERGENCE DU CALCUL
C                     1 - RESI_DUAL_ABSO      (LAGRANGIEN AUGMENTE)
C                     2 - RESI_PRIM_ABSO      (LAGRANGIEN AUGMENTE)
C                     3 - NOMBRE D'ITERATIONS DUAL (LAGRANGIEN AUGMENTE)
C                     4 - NUMERO ITERATION BFGS (LAGRANGIEN AUGMENTE)
C                    10 - NOMBRE D'ITERATIONS (RECHERCHE LINEAIRE)
C                    11 - RHO                 (RECHERCHE LINEAIRE)
C                    20 - RESI_GLOB_RELA
C                    21 - RESI_GLOB_MAXI
C IN       LICCVG : CODES RETOURS
C                   (1) : PILOTAGE
C                       =  0 CONVERGENCE
C                       =  1 PAS DE CONVERGENCE
C                       = -1 BORNE ATTEINTE
C                   (2) : INTEGRATION DE LA LOI DE COMPORTEMENT
C                       = 0 OK
C                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C                   (3) : TRAITEMENT DU CONTACT UNILATERAL EN GD. DEPL.
C                       = 0 OK
C                       = 1 ECHEC DANS LE TRAITEMENT DU CONTACT
C                   (4) : TRAITEMENT DU CONTACT UNILATERAL EN GD. DEPL.
C                       = 0 OK
C                       = 1 MATRICE DE CONTACT SINGULIERE
C                   (5) : MATRICE DU SYSTEME (MATASS)
C                       = 0 OK
C                       = 1 MATRICE SINGULIERE
C IN       FOINER  : FORCES D'INERTIE EN DYNA
C IN       PARTPS  : LISTE D'INSTANT
C IN       PARMET  : PARAMETRES DE LA METHODE DE RESOLUTION 
C                       3 - PAS_MINI_ELAS
C OUT   ITEMAX    : .TRUE. SI ITERATION MAXIMUM ATTEINTE
C OUT   CONVER    : .TRUE. SI CONVERGENCE REALISEE
C OUT   ECHLDC    : .TRUE. SI ECHEC INTEGRATION LOI DE COMPORTEMENT
C OUT   ECHCON    : .TRUE. SI ECHEC TRAITEMENT DU CONTACT UNILATERAL
C OUT   ECHEQU    : .TRUE. SI MATASS SINGULIERE
C OUT   FINPAS    : .TRUE. SI ON NE FAIT PLUS D'AUTRES PAS DE TEMPS
C OUT   MAXREL  : .TRUE. SI CRITERE RESI_GLOB_RELA ET CHARGEMENT = 0,
C                             ON UTILISE RESI_GLOB_MAXI
C IN/JXOUT CRITNL : SYNTHESE DES RESULTATS DE CONVERGENCE POUR ARCHIVAGE

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

      INTEGER            NBCOL
      PARAMETER        ( NBCOL=10 )

      LOGICAL            TEST(NBCOL), MAXREL
      INTEGER            LONCH, JRESI, JFEXT, JDIRI, JCRR, K, I, JFOINE
      INTEGER            JVCFO, IND(NBCOL), JREFE
      INTEGER            IBID,  IER,   IDEEQ
      REAL*8             VMAX1, VMAX2, VMAX3, INFO(NBCOL), R8VIDE
      REAL*8             INSTAM, INSTAP, DIINST, PASMIN
      CHARACTER*1        MARQ(NBCOL)
      CHARACTER*8        K8BID
      CHARACTER*19       PROFCH

C     IND : INDIRECTION DANS PARCRI
C   2 -> 2  PARCRI(2)  : RESI_GLOB_RELA
C   3 -> 3  PARCRI(3)  : RESI_GLOB_MAXI
C   4 -> 6  PARCRI(6)  : RESI_REFE_RELA
C   7 -> 11 PARCRI(11) : RESI_DUAL_ABSO
C   8 -> 10 PARCRI(10) : RESI_PRIM_ABSO

      DATA               IND  / 0, 2, 3, 6, 0, 0, 0, 11, 10, 0/
C ----------------------------------------------------------------------


      CALL JEMARQ()
      FINPAS = .FALSE.


C ======================================================================
C          TRAITEMENT DES CODES RETOUR DE LA LOI DE COMPORTEMENT
C ======================================================================

CJMP      ECHLDC = LICCVG(2) .NE. 0
      ECHLDC = (LICCVG(2).NE.0).AND.(LICCVG(2).NE.3)
      IF (ECHLDC) THEN
        CONVER = .FALSE.
        CALL NMIMPR('IMPR','ECHEC_LDC',' ',0.D0,0)
        GOTO 9999
      END IF

C ======================================================================
C          TRAITEMENT DES CODES RETOUR DE L'EQUILIBRE
C ======================================================================

      ECHEQU = LICCVG(5).NE.0
      IF (ECHEQU) THEN
        CONVER = .FALSE.
        CALL NMIMPR('IMPR','MATR_SING',' ',0.D0,0)
        GOTO 9999
      END IF

C ======================================================================
C          TRAITEMENT DES CODES RETOUR DU CONTACT
C ======================================================================

      ECHCON(1) = LICCVG(3) .NE. 0
      IF (ECHCON(1)) THEN
        CONVER = .FALSE.
        CALL NMIMPR('IMPR','ECHEC_CON',' ',0.D0,0)
        GOTO 9999
      END IF

      ECHCON(2) = LICCVG(4) .NE. 0
      IF (ECHCON(2)) THEN
        CONVER = .FALSE.
        CALL NMIMPR('IMPR','CONT_SING',' ',0.D0,0)
        GOTO 9999
      END IF


C ======================================================================
C                      EXAMEN DU NOMBRE D'ITERATIONS
C ======================================================================

      INSTAM = DIINST(PARTPS, NUMINS-1)
      INSTAP = DIINST(PARTPS, NUMINS  )
      PASMIN = PARMET(3)
      IF (ABS(INSTAP-INSTAM) .LT. PASMIN) THEN
        ITEMAX = ITERAT .GE. PARCRI(5)
      ELSE
        ITEMAX = ITERAT .GE. PARCRI(1)
      END IF


C ======================================================================
C                         EXAMEN DE LA CONVERGENCE
C ======================================================================

      DO 10 K = 1,NBCOL
        INFO(K) = -1.D+00
        MARQ(K) = ' '
        TEST(K) = .FALSE.
        IF ( IND(K) .NE. 0 ) THEN 
           TEST(K) = (PARCRI(IND(K)) .NE. R8VIDE())
        ENDIF
 10   CONTINUE


C -- EVALUATION DES RESIDUS ABSOLU ET RELATIF

      CALL JELIRA(CNRESI//'.VALE','LONMAX',LONCH,K8BID)
      CALL JEVEUO(CNRESI//'.VALE','L',JRESI)
      CALL JEVEUO(CNDIRI//'.VALE','L',JDIRI)
      CALL JEVEUO(CNFEXT//'.VALE','L',JFEXT)
      CALL JEVEUO(CNVCFO//'.VALE','L',JVCFO)
      IF (TEST(4)) CALL JEVEUO(CNVFRE//'.VALE','L',JREFE)
      CALL JEVEUO(FOINER//'.VALE','L',JFOINE)
      CALL DISMOI('F','PROF_CHNO',CNRESI,'CHAM_NO',IBID, PROFCH,IER)
      CALL JEVEUO(PROFCH // '.DEEQ','L',IDEEQ)

      VMAX1 = 0.D0
      VMAX2 = 0.D0
      VMAX3 = 0.D0
            
      DO 20 I=0,LONCH-1
        VMAX1 = MAX(ABS(ZR(JRESI+I)-ZR(JFEXT+I)),VMAX1)
        VMAX2 = MAX(ABS(ZR(JDIRI+I)-ZR(JFEXT+I))
     &             +ABS(ZR(JVCFO+I)) , VMAX2)
        VMAX2 = MAX(ABS(ZR(JFOINE+I)),VMAX2)
C       SI CONVERGENCE EN CONTRAINTE ACTIVE
        IF (TEST(4)) THEN
C         SI C'EST UN DDL PHYSIQUE
          IF (ZI(IDEEQ-1 + 2*I + 2).GT.0) THEN
            VMAX3 = MAX(ABS(ZR(JRESI+I)-ZR(JFEXT+I))/ZR(JREFE+I),VMAX3)
          ENDIF
C         SI C'EST UN DDL DE LAGRANGE : LA CONVERGENCE EST IMMEDIATE 
C        (PAS DE LIAISON NON LINEAIRE DANS ASTER) : ON NE LA TESTE PAS
        ENDIF  
 20   CONTINUE
      
      INFO(3) = VMAX1
      INFO(4) = VMAX3
      IF (VMAX2.GT.0.D0) THEN
        INFO(2) = VMAX1/VMAX2
      END IF


C -- CRITERES DU LAGRANGIEN AUGMENTE

      INFO(8) = CONV(1)
      INFO(9) = CONV(2)


C -- CRITERES D'ARRET

      CONVER = .TRUE.
      DO 120 K=1,NBCOL
        IF ( TEST(K) .AND.
     &  ( INFO(K) .GT. PARCRI(IND(K)) .OR. INFO(K).LT.0) ) THEN
          MARQ(K)  = 'X'
          CONVER = .FALSE.
        ENDIF
 120  CONTINUE
      

C -- SI CRITERE RESI_GLOB_RELA ET CHARGEMENT = 0, 
C -- ON UTILISE RESI_GLOB_MAXI

      MAXREL = .FALSE.
      IF (TEST(2) .AND. (NUMINS .GT. 1)) THEN
         CALL JEVEUO(CRITNL //'.CRTR','L',JCRR)
         IF (VMAX2 .LT. (1.D-6 * ZR(JCRR+5))) THEN
            MARQ(2)  = ' '
            MARQ(3)  = 'X'
            IF (INFO(3) .LT. ZR(JCRR+6)) THEN
               MARQ(2)  = ' '
               MARQ(3)  = ' '
               CONVER = .TRUE.
               MAXREL = .TRUE.
               CALL UTMESS('A','NMCONV','CONVERGENCE ATTEINTE AVEC RESI_
     &GLOB_MAXI POUR CAUSE DE CHARGEMENT PRESQUE NUL')
            ENDIF
         ENDIF
      ENDIF
      

C -- CONTROLE DES CODES RETOUR QUI CONDITIONNENT LA CONVERGENCE

C    PAR RAPPORT AU PILOTAGE
      IF (LICCVG(1) .GE. 1) THEN
        CONVER = .FALSE.
        MARQ(6) = 'X'
      END IF

      IF (LICCVG(1) .LT. 0) THEN
        MARQ(6) = 'S'
        FINPAS  = .TRUE.
      END IF


C -- IMPRESSIONS

      INFO(1) = ITERAT

C    ITERATIONS RECHERCHE LINEAIRE ET RHO
      IF (ITERAT.EQ.0) CONV(10)=0
      INFO(5) = CONV(10)
      INFO(6) = CONV(11)
      IF (NINT(INFO(5)).EQ.0) INFO(6) = 1.D0

      INFO(7) = ETA
      INFO(10) = CONV(3)
      CALL NMIMPR('IMPR','ETAT_CONV',MARQ,INFO,0)
C ======================================================================
      IF (CONVER) THEN
C ======================================================================
         IF (LREAC(4)) THEN
C ======================================================================
C --- CONVERGENCE ADAPTEE AU CONTACT -----------------------------------
C ======================================================================
            CALL CVGCNT(ITEMAX, NEQ, DEPDEL, AUTOC1, AUTOC2, VECONT,
     +                                                            LREAC)
         ELSE
C ======================================================================
C --- LE NOMBRE D'ITERATIONS MAXIMAL EST_IL ATTEINT? -------------------
C ======================================================================
            IF (.NOT. MAXREL) THEN 
              CALL NMIMPR('IMPR','CONV_OK',' ',0.D0,0)
            ELSE 
              CALL NMIMPR('IMPR','MAXI_RELA',' ',ZR(JCRR+8),0)
            ENDIF
         ENDIF
C ======================================================================
C --- METHODE DE BORST -------------------------------------------------
C ======================================================================
         IF (LICCVG(2).EQ.3) THEN
            CALL NMIMPR('IMPR','DE_BORST',' ',0.D0,0)
            CONVER=.FALSE.
         END IF
      ELSE
C ======================================================================
C --- IL N'Y A PAS CONVERGENCE -----------------------------------------
C ======================================================================
         IF (ITEMAX) THEN
C ======================================================================
C --- NOMBRE MAXIMUM D'ITERATIONS --------------------------------------
C ======================================================================
            CALL NMIMPR('IMPR','ITER_MAXI',' ',0.D0,0)
         ELSE
            LREAC(1) = .FALSE.
         ENDIF
      ENDIF
C ======================================================================
C --- ON PREND EN COMPTE LA CONVERGENCE NUMERIQUE (CONVER) -------------
C --- ET LA CONVERGENCE GEOMETRIQUE (LREAC(3)) -------------------------
C --- LREAC(3) VAUT TOUJOURS .TRUE. S'IL N'Y A PAS CONTACT) ------------
C ======================================================================
      CONVER = CONVER.AND.LREAC(3)
C ======================================================================
C -- LES RESIDUS -------------------------------------------------------
C ======================================================================
      CONV(20) = INFO(2)
      CONV(21) = INFO(3)
C ======================================================================
C -- PREPARATION DES PARAMETRES ARCHIVEES ------------------------------
C ======================================================================
      CALL JEVEUO(CRITNL //'.CRTR','E',JCRR)
      ZR(JCRR+0) = ITERAT
      ZR(JCRR+1) = INFO(5)
      ZR(JCRR+2) = INFO(2)
      ZR(JCRR+3) = INFO(3)
      ZR(JCRR+4) = ETA
      IF (NUMINS.EQ.1 .AND. ITERAT.EQ.0) THEN 
         ZR(JCRR+5) = VMAX2
      ELSE
         IF (CONVER.AND..NOT.MAXREL) ZR(JCRR+5) = MIN(VMAX2, ZR(JCRR+5))
      ENDIF
      IF (CONVER)  ZR(JCRR+6) = VMAX1
C ======================================================================
 9999 CONTINUE
      CALL JEDEMA()
      END
