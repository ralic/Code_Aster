      SUBROUTINE DIMAX2 ( JDOM, NBPT, CUON, CVON, RAYON, CUPN, CVPN,
     &                    IRET )
      IMPLICIT   NONE
      INTEGER             JDOM, NBPT, IRET
      REAL*8              CUON, CVON, RAYON, CUPN, CVPN
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/06/2002   AUTEUR F1BHHAJ J.ANGLES 
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
C RESPONSABLE F1BHHAJ
C ---------------------------------------------------------------------
C BUT: PARMI UNE LISTE DE POINTS, DETERMINER LE POINT QUI EST LE PLUS
C      ELOIGNE D'UN POINT DONNE.
C ---------------------------------------------------------------------
C ARGUMENTS :
C     JDOM    : IN  : ADRESSE DU VECTEUR CONTENANT LES POINTS DU 
C                     DOMAINE PARMI LESQUELS, NOUS CHERCHONS S'IL
C                     EXISTE UN POINT QUI SOIT PLUS ELOIGNE DU CENTRE
C                     QUE LE RAYON PRECEDEMMENT CALCULE.
C     NBPT    : IN  : NOMBRE DE POINTS DANS LE DOMAINE.
C     CUON    : IN  : COMPOSANTE U DU CENTRE "On".
C     CVON    : IN  : COMPOSANTE V DU CENTRE "On".
C     RAYON   : IN  : VALEUR DU RAYON CIRCONSCRIT AVANT RECHERCHE D'UN
C                     POINT PLUS ELOIGNE DU CENTRE "On".
C     CUPN    : OUT : COMPOSANTE U DU POINT LE PLUS ELOIGNE S'IL EN
C                     EXISTE UN.
C     CVPN    : OUT : COMPOSANTE V DU POINT LE PLUS ELOIGNE S'IL EN
C                     EXISTE UN.
C     IRET    : OUT : INDIQUE S'IL EXISTE UN POINT PLUS ELOIGNE
C                     DU CENTRE "On" QUE DE LA VALEUR DU RAYON
C                     CIRCONSCRIT PRECEDENT.
C                     IRET = 0 => IL N'Y A PAS DE POINT PLUS ELOIGNE;
C                     IRET = 1 => IL Y A AUMOINS UN POINT PLUS ELOIGNE.
C     -----------------------------------------------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ------------------------------------------------------------------
      INTEGER    I
      REAL*8     CUI, CVI, DIST
C     ------------------------------------------------------------------
C
C234567                                                              012
      CALL JEMARQ()

      IRET = 0
      CUPN = 0.0D0
      CVPN = 0.0D0

      DO 10 I=1, NBPT
         CUI = ZR(JDOM + (I-1)*2)
         CVI = ZR(JDOM + (I-1)*2 + 1)
         DIST = SQRT((CUON - CUI)**2 + (CVON - CVI)**2)

         IF (DIST .GT. (RAYON+1.0D-9)) THEN
            RAYON = DIST
            CUPN = CUI
            CVPN = CVI
            IRET = 1
         ENDIF

 10   CONTINUE

      CALL JEDEMA()
      END
