      SUBROUTINE FOLIE1 ( NBFR, FREQ, ACCE, NBCREU, LCREU, VALTG,
     +                    VALTD, LISS, NOMF )
      IMPLICIT  NONE
      INTEGER             NBFR, NBCREU
      REAL*8              FREQ(*), ACCE(*), LCREU(*), VALTG,VALTD, LISS
      CHARACTER*(*)       NOMF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/12/2002   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C  IN : NBFR   : NOMBRE DE FREQUENCES DE LA FONCTION A TRAITER
C  IN : FREQ   : LISTE DES ABSCISSES DE LA FONCTION A TRAITER
C  IN : ACCE   : LISTE DES ORDONNEES DE LA FONCTION A TRAITER
C  IN : NBCREU : NOMBRE DE FREQUENCES DE CREUX A GARDER
C  IN : LCREU  : LISTE DES FREQUENCES DE CREUX
C  IN : VALTG  : VALEUR DE L'ELARGISSEMENT A GAUCHE
C  IN : VALTD  : VALEUR DE L'ELARGISSEMENT A DROITE
C  IN : LISS   : CRITERE POUR L'ELIMINATION DES POINTS DE LISSAGE
C OUT : NOMF   : FONCTION RESULTAT
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       I,IF, IDEB, IFIN, NBFR0, NBVAL, NBFLIS, JENV, JRESU
      CHARACTER*8   K8B
      CHARACTER*24  NOMC
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
C     ------------------------------------------------------------------
C                    --- LISSSAGE ENVELOPPE ---
C     ------------------------------------------------------------------
C
      IF ( NBCREU .EQ. 0 ) THEN
C
         CALL FOLIE0 ( NBFR, FREQ, ACCE, VALTG, VALTD, LISS, NOMF )
C
      ELSE
C
         CALL WKVECT ( NOMF , 'V V R', 2*NBFR, JRESU )
         NBVAL = 0
C
         IDEB = 1
         DO 100 IF = 1 , NBCREU+1
            IFIN = NBFR
            IF ( IF .LE. NBCREU ) THEN
               DO 110 I = NBFR , 1, -1
                  IF ( FREQ(I) .LE. LCREU(IF) ) THEN
                     IFIN = I
                     GOTO 112
                  ENDIF
 110           CONTINUE      
 112           CONTINUE
            ENDIF
            NBFR0 = NBFR - IDEB + 1 - ( NBFR - IFIN )
C
            NOMC = '&&FOLIE1.RESU'
            CALL FOLIE0 ( NBFR0, FREQ(IDEB), ACCE(IDEB), 
     +                                       VALTG, VALTD, LISS, NOMC )
C
            CALL JELIRA ( NOMC, 'LONMAX', NBFLIS, K8B )
            NBFLIS = NBFLIS / 2
            CALL JEVEUO ( NOMC, 'L', JENV )
C
            DO 120 I = 1 , NBFLIS
               NBVAL =  NBVAL + 1
               ZR(JRESU+NBVAL-1) = ZR(JENV+I-1)
               ZR(JRESU+NBFR+NBVAL-1) = ZR(JENV+NBFLIS+I-1)
 120        CONTINUE  
C
            CALL JEDETR ( NOMC )
            IDEB = IFIN
 100     CONTINUE  
C
         CALL JEECRA ( NOMF , 'LONUTI', 2*NBVAL, ' ' )
C   
      ENDIF
C
      CALL JEDEMA()
      END
