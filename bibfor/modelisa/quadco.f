      SUBROUTINE QUADCO (CHAR,MOTFAC,NZOCP,
     &                   INDQUA)   
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/09/2005   AUTEUR MABBAS M.ABBAS 
C RESPONSABLE MABBAS M.ABBAS
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      IMPLICIT      NONE
      CHARACTER*8  CHAR
      CHARACTER*16 MOTFAC
      INTEGER      NZOCP  
      INTEGER      INDQUA      
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CALICO
C ----------------------------------------------------------------------
C
C SI LA METHODE EST 'PENALISATION' OU 'CONTINUE' -> INDQUA = 1
C CE DRAPEAU SERVIRA DANS L'APPEL DE NBNOEL ET PERMETTRA DE
C TRAITER LES NOEUDS MILIEUX DES MAILLES QUADRATIQUES         
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
C IN  NZOCP  : NOMBRE DE ZONES DE CONTACT PRINCIPALES
C OUT INDQUA : VAUT 1 SANS LINEARISATION POUR LES NOEUDS MILIEUX
C              DES MAILLES QUADRATIQUES
C              VAUT 0 AVEC LINEARISATION DES MAILLES QUADRATIQUES
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER        IOC,NOC,NOCN
      CHARACTER*16   TYPF,PROJ,GLIS
      REAL*8         COEFPN
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

      INDQUA = 0

C --- ON NE BOUCLE QUE SUR LES ZONES PRINCIPALES
      DO 2 IOC = 1,NZOCP
         CALL GETVTX (MOTFAC,'METHODE',IOC,1,1,TYPF,NOC)
         IF (TYPF(1:8).EQ.'CONTRAIN') THEN
            INDQUA = 0
            CALL GETVTX (MOTFAC,'GLISSIERE',IOC,1,1,GLIS,NOC)
            IF (GLIS.EQ.'OUI') INDQUA = 1
         ELSE IF (TYPF(1:8).EQ.'LAGRANGI') THEN
            INDQUA = 0
         ELSE IF (TYPF(1:8).EQ.'PENALISA') THEN
            CALL GETVR8 (MOTFAC,'E_N',1,1,1,COEFPN,NOCN)
            IF (NOCN.NE.0) THEN 
               INDQUA=1
            ENDIF
         ELSE IF (TYPF(1:5).EQ.'VERIF') THEN
           INDQUA = 1
         ELSE IF (TYPF(1:8).EQ.'CONTINUE') THEN
           INDQUA = 1
         ELSE
          CALL UTMESS('F','QUADCO',
     &                'METHODE DE CONTACT INCONNUE (DVLP)') 
         ENDIF          
  2   CONTINUE     

C
C ----------------------------------------------------------------------
C
      CALL JEDEMA()
      END
